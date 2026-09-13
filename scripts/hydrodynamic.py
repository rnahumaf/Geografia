"""Solver 2D preliminar de inundação pelo modelo de inércia local.

O módulo resolve continuidade e momento simplificado em uma grade regular,
com molhamento/secagem, atrito de Manning, hidrograma de ruptura e controle
de volume disponível por célula. O resultado serve para exploração e não
substitui um modelo hidráulico calibrado.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any

import numpy as np
from pyproj import Transformer
from rasterio.features import rasterize, shapes
from rasterio.transform import from_bounds as transform_from_bounds
from scipy.ndimage import minimum_filter
from shapely.geometry import LineString, MultiPolygon, Polygon, mapping, shape
from shapely.ops import transform, unary_union

try:
    from scripts.compute_backend import select_compute_backend
except ModuleNotFoundError:  # Execução direta por `python scripts/prepare_data.py`.
    from compute_backend import select_compute_backend

try:
    from app_backend.settings import load_compute_preference
except ModuleNotFoundError:
    def load_compute_preference() -> str:
        return "auto"


GRAVITY = 9.80665


@dataclass(frozen=True)
class SimulationConfig:
    compute_backend: str = field(default_factory=load_compute_preference)
    downsample_factor: int = 2
    focus_bounds_wgs84: tuple[float, float, float, float] = (
        -47.68,
        -23.65,
        -47.29,
        -23.33,
    )
    released_volume_hm3: float = 247.57
    peak_discharge_m3s: float = 15_000.0
    time_to_peak_minutes: float = 60.0
    extra_routing_hours: float = 3.0
    manning_floodplain: float = 0.060
    manning_channel: float = 0.035
    cfl: float = 0.42
    maximum_froude_number: float = 2.5
    maximum_timestep_seconds: float = 20.0
    wet_threshold_m: float = 0.03
    mapped_depth_threshold_m: float = 0.30


def run_hydrodynamic_simulation(
    elevation: np.ndarray,
    metadata: dict[str, Any],
    rivers: list[dict[str, Any]],
    reservoirs: list[dict[str, Any]],
    route_wgs84: LineString,
    dam: dict[str, Any],
    output_dir: Path,
    config: SimulationConfig | None = None,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], dict[str, Any]]:
    config = config or SimulationConfig()
    terrain, grid_transform, grid_bounds = _prepare_grid(
        elevation,
        metadata,
        config.downsample_factor,
        config.focus_bounds_wgs84,
    )
    rows, columns = terrain.shape
    center_latitude = (grid_bounds["north"] + grid_bounds["south"]) / 2
    cell_width_m = (
        (grid_bounds["east"] - grid_bounds["west"])
        * 111_320
        * math.cos(math.radians(center_latitude))
        / columns
    )
    cell_height_m = (
        (grid_bounds["north"] - grid_bounds["south"])
        * 110_540
        / rows
    )
    cell_area_m2 = cell_width_m * cell_height_m

    channel_mask = rasterize(
        [(shape(feature["geometry"]), 1) for feature in rivers],
        out_shape=terrain.shape,
        transform=grid_transform,
        fill=0,
        all_touched=True,
        dtype=np.uint8,
    ).astype(bool)
    # Condicionamento simples do leito: reduz picos de pontes, copas e prédios
    # no DSM, sem escavar artificialmente toda a planície.
    local_minimum = minimum_filter(terrain, size=3, mode="nearest")
    terrain[channel_mask] = np.minimum(
        terrain[channel_mask], local_minimum[channel_mask] + 0.35
    )

    solid_mask = rasterize(
        [
            (shape(feature["geometry"]), 1)
            for feature in reservoirs
            if feature.get("geometry")
        ],
        out_shape=terrain.shape,
        transform=grid_transform,
        fill=0,
        all_touched=True,
        dtype=np.uint8,
    ).astype(bool)
    source_mask = _source_cells(
        route_wgs84,
        solid_mask,
        grid_transform,
        terrain.shape,
    )

    backend = select_compute_backend(config.compute_backend)
    xp = backend.xp
    manning = np.full(terrain.shape, config.manning_floodplain, dtype=np.float32)
    manning[channel_mask] = config.manning_channel
    terrain_device = xp.asarray(terrain)
    solid_mask_device = xp.asarray(solid_mask)
    source_mask_device = xp.asarray(source_mask)
    manning_device = xp.asarray(manning)
    depth = xp.zeros(terrain.shape, dtype=xp.float32)
    discharge_x = xp.zeros((rows, columns - 1), dtype=xp.float32)
    discharge_y = xp.zeros((rows - 1, columns), dtype=xp.float32)
    maximum_depth = xp.zeros_like(depth)
    maximum_velocity = xp.zeros_like(depth)
    maximum_hazard = xp.zeros_like(depth)
    arrival_seconds = xp.full(terrain.shape, xp.nan, dtype=xp.float32)

    released_volume_m3 = config.released_volume_hm3 * 1_000_000
    time_to_peak_s = config.time_to_peak_minutes * 60
    hydrograph_duration_s = 2 * released_volume_m3 / config.peak_discharge_m3s
    if hydrograph_duration_s <= time_to_peak_s:
        raise ValueError("O hidrograma não comporta o volume e o tempo de pico.")
    simulation_duration_s = hydrograph_duration_s + config.extra_routing_hours * 3_600

    time_s = 0.0
    injected_volume_m3 = 0.0
    next_summary_s = 0.0
    time_series: list[dict[str, float]] = []
    iteration_count = 0

    while time_s < simulation_duration_s:
        wet = depth > config.wet_threshold_m
        if bool(backend.scalar(wet.any())):
            # A aproximação de inércia local elimina a aceleração advectiva;
            # sua condição CFL usa a celeridade da onda, sqrt(g h).
            representative_speed = backend.scalar(xp.sqrt(GRAVITY * xp.max(depth[wet])))
        else:
            representative_speed = 1.0
        timestep_s = min(
            config.maximum_timestep_seconds,
            config.cfl * min(cell_width_m, cell_height_m) / max(representative_speed, 1),
            simulation_duration_s - time_s,
        )
        timestep_s = max(timestep_s, 0.2)

        midpoint_s = time_s + timestep_s / 2
        inflow_m3s = _hydrograph_discharge(
            midpoint_s,
            config.peak_discharge_m3s,
            time_to_peak_s,
            hydrograph_duration_s,
        )
        step_volume_m3 = min(
            inflow_m3s * timestep_s,
            max(0.0, released_volume_m3 - injected_volume_m3),
        )
        if step_volume_m3 > 0:
            depth[source_mask_device] += step_volume_m3 / (
                backend.scalar(source_mask_device.sum()) * cell_area_m2
            )
            injected_volume_m3 += step_volume_m3

        surface = terrain_device + depth
        discharge_x = _face_discharge(
            discharge_x,
            surface[:, :-1],
            surface[:, 1:],
            terrain_device[:, :-1],
            terrain_device[:, 1:],
            0.5 * (manning_device[:, :-1] + manning_device[:, 1:]),
            timestep_s,
            cell_width_m,
            ~(solid_mask_device[:, :-1] | solid_mask_device[:, 1:]),
            config.maximum_froude_number,
            xp,
        )
        discharge_y = _face_discharge(
            discharge_y,
            surface[:-1, :],
            surface[1:, :],
            terrain_device[:-1, :],
            terrain_device[1:, :],
            0.5 * (manning_device[:-1, :] + manning_device[1:, :]),
            timestep_s,
            cell_height_m,
            ~(solid_mask_device[:-1, :] | solid_mask_device[1:, :]),
            config.maximum_froude_number,
            xp,
        )

        discharge_x, discharge_y = _limit_outflow(
            discharge_x,
            discharge_y,
            depth,
            timestep_s,
            cell_width_m,
            cell_height_m,
            xp,
        )
        divergence = xp.zeros_like(depth)
        divergence[:, :-1] -= discharge_x / cell_width_m
        divergence[:, 1:] += discharge_x / cell_width_m
        divergence[:-1, :] -= discharge_y / cell_height_m
        divergence[1:, :] += discharge_y / cell_height_m
        depth += timestep_s * divergence
        depth[depth < 1e-5] = 0
        depth[solid_mask_device] = 0

        velocity = _cell_velocity(
            discharge_x,
            discharge_y,
            depth,
            config.maximum_froude_number,
            xp,
        )
        mapped_now = depth >= config.mapped_depth_threshold_m
        maximum_depth = xp.maximum(maximum_depth, depth)
        maximum_velocity = xp.maximum(
            maximum_velocity, xp.where(mapped_now, velocity, 0)
        )
        maximum_hazard = xp.maximum(
            maximum_hazard,
            xp.where(mapped_now, depth * (velocity + 0.5), 0),
        )
        newly_arrived = xp.isnan(arrival_seconds) & (
            depth >= config.mapped_depth_threshold_m
        )
        arrival_seconds[newly_arrived] = time_s + timestep_s

        time_s += timestep_s
        iteration_count += 1
        if time_s >= next_summary_s or time_s >= simulation_duration_s:
            mapped = depth >= config.mapped_depth_threshold_m
            time_series.append(
                {
                    "timeHours": round(time_s / 3_600, 3),
                    "injectedVolumeHm3": round(injected_volume_m3 / 1_000_000, 3),
                    "waterOnGridHm3": round(
                        backend.scalar(depth.sum()) * cell_area_m2 / 1_000_000, 3
                    ),
                    "floodedAreaKm2": round(
                        backend.scalar(mapped.sum()) * cell_area_m2 / 1_000_000, 3
                    ),
                    "maximumDepthM": round(backend.scalar(depth.max()), 3),
                }
            )
            next_summary_s += 3_600

    maximum_depth = backend.to_numpy(maximum_depth)
    maximum_velocity = backend.to_numpy(maximum_velocity)
    maximum_hazard = backend.to_numpy(maximum_hazard)
    arrival_seconds = backend.to_numpy(arrival_seconds)
    depth = backend.to_numpy(depth)
    mapped = maximum_depth >= config.mapped_depth_threshold_m
    stored_volume_m3 = float(depth.sum() * cell_area_m2)
    mass_error_pct = (
        100 * (stored_volume_m3 - injected_volume_m3) / injected_volume_m3
        if injected_volume_m3
        else 0.0
    )
    features, raw_features = _hazard_features(
        maximum_hazard,
        maximum_depth,
        maximum_velocity,
        arrival_seconds,
        mapped,
        grid_transform,
        cell_area_m2,
        route_wgs84,
        max(cell_width_m, cell_height_m),
    )

    output_dir.mkdir(parents=True, exist_ok=True)
    np.savez_compressed(
        output_dir / "hydrodynamic-grid.npz",
        terrain_m=terrain,
        maximum_depth_m=maximum_depth,
        maximum_velocity_ms=maximum_velocity,
        maximum_hazard_index=maximum_hazard,
        arrival_seconds=arrival_seconds,
        bounds=np.array(
            [
                grid_bounds["west"],
                grid_bounds["south"],
                grid_bounds["east"],
                grid_bounds["north"],
            ],
            dtype=np.float64,
        ),
    )

    valid_arrivals = arrival_seconds[np.isfinite(arrival_seconds)]
    result = {
        "model": "equações de águas rasas 2D — aproximação de inércia local",
        "status": "experimental e não calibrado",
        "computeBackend": backend.name,
        "computeDevice": backend.device_name,
        "floodedAreaKm2": round(float(mapped.sum() * cell_area_m2) / 1_000_000, 2),
        "maximumDepthM": round(float(maximum_depth[mapped].max()), 2) if mapped.any() else 0,
        "depthP95M": round(float(np.percentile(maximum_depth[mapped], 95)), 2) if mapped.any() else 0,
        "maximumVelocityMs": round(float(maximum_velocity[mapped].max()), 2) if mapped.any() else 0,
        "earliestArrivalMinutes": round(float(valid_arrivals.min()) / 60, 2) if valid_arrivals.size else None,
        "releasedVolumeHm3": round(injected_volume_m3 / 1_000_000, 3),
        "waterOnGridAtEndHm3": round(stored_volume_m3 / 1_000_000, 3),
        "massBalanceErrorPct": round(mass_error_pct, 5),
        "peakDischargeM3s": config.peak_discharge_m3s,
        "hydrographDurationHours": round(hydrograph_duration_s / 3_600, 3),
        "simulationDurationHours": round(simulation_duration_s / 3_600, 3),
        "iterations": iteration_count,
        "cellWidthM": round(cell_width_m, 1),
        "cellHeightM": round(cell_height_m, 1),
        "configuration": asdict(config),
        "timeSeries": time_series,
    }
    return features, raw_features, result


def _prepare_grid(
    elevation: np.ndarray,
    metadata: dict[str, Any],
    factor: int,
    focus_bounds: tuple[float, float, float, float] | None = None,
) -> tuple[np.ndarray, Any, dict[str, float]]:
    source_bounds = metadata["bounds"]
    source_rows, source_columns = elevation.shape
    if focus_bounds is None:
        column_start, row_start = 0, 0
        column_end, row_end = source_columns, source_rows
    else:
        west, south, east, north = focus_bounds
        column_start = max(
            0,
            math.floor(
                (west - source_bounds["west"])
                / (source_bounds["east"] - source_bounds["west"])
                * source_columns
            ),
        )
        column_end = min(
            source_columns,
            math.ceil(
                (east - source_bounds["west"])
                / (source_bounds["east"] - source_bounds["west"])
                * source_columns
            ),
        )
        row_start = max(
            0,
            math.floor(
                (source_bounds["north"] - north)
                / (source_bounds["north"] - source_bounds["south"])
                * source_rows
            ),
        )
        row_end = min(
            source_rows,
            math.ceil(
                (source_bounds["north"] - south)
                / (source_bounds["north"] - source_bounds["south"])
                * source_rows
            ),
        )

    subset = elevation[row_start:row_end, column_start:column_end]
    rows = subset.shape[0] // factor
    columns = subset.shape[1] // factor
    trimmed = subset[: rows * factor, : columns * factor]
    blocks = trimmed.reshape(rows, factor, columns, factor)
    # Percentil baixo aproxima o terreno nu e preserva os fundos de vale do DSM.
    terrain = np.percentile(blocks, 20, axis=(1, 3)).astype(np.float32)
    cell_lon = (source_bounds["east"] - source_bounds["west"]) / source_columns
    cell_lat = (source_bounds["north"] - source_bounds["south"]) / source_rows
    bounds = {
        "west": float(source_bounds["west"] + column_start * cell_lon),
        "north": float(source_bounds["north"] - row_start * cell_lat),
        "east": float(
            source_bounds["west"] + (column_start + columns * factor) * cell_lon
        ),
        "south": float(
            source_bounds["north"] - (row_start + rows * factor) * cell_lat
        ),
    }
    grid_transform = transform_from_bounds(
        bounds["west"],
        bounds["south"],
        bounds["east"],
        bounds["north"],
        columns,
        rows,
    )
    return terrain, grid_transform, bounds


def _source_cells(
    route_wgs84: LineString,
    solid_mask: np.ndarray,
    grid_transform: Any,
    shape_: tuple[int, int],
) -> np.ndarray:
    to_utm = Transformer.from_crs("EPSG:4326", "EPSG:32723", always_xy=True)
    to_wgs84 = Transformer.from_crs("EPSG:32723", "EPSG:4326", always_xy=True)
    route_utm = transform(to_utm.transform, route_wgs84)
    sample_distances = np.linspace(180, min(1_000, route_utm.length), 7)
    source_line_utm = LineString(
        [route_utm.interpolate(float(distance)) for distance in sample_distances]
    )
    source_line = transform(to_wgs84.transform, source_line_utm)
    source = rasterize(
        [(source_line, 1)],
        out_shape=shape_,
        transform=grid_transform,
        fill=0,
        all_touched=True,
        dtype=np.uint8,
    ).astype(bool)
    source &= ~solid_mask
    if not source.any():
        raise RuntimeError("Não foi possível posicionar a fonte a jusante da barragem.")
    return source


def _hydrograph_discharge(
    time_s: float,
    peak_m3s: float,
    time_to_peak_s: float,
    duration_s: float,
) -> float:
    if time_s <= 0 or time_s >= duration_s:
        return 0.0
    if time_s <= time_to_peak_s:
        return peak_m3s * time_s / time_to_peak_s
    return peak_m3s * (duration_s - time_s) / (duration_s - time_to_peak_s)


def _face_discharge(
    previous: np.ndarray,
    surface_a: np.ndarray,
    surface_b: np.ndarray,
    terrain_a: np.ndarray,
    terrain_b: np.ndarray,
    manning: np.ndarray,
    timestep_s: float,
    spacing_m: float,
    valid_face: np.ndarray,
    maximum_froude_number: float,
    xp: Any = np,
) -> np.ndarray:
    flow_depth = xp.maximum(surface_a, surface_b) - xp.maximum(terrain_a, terrain_b)
    active = (flow_depth > 1e-4) & valid_face
    slope = (surface_b - surface_a) / spacing_m
    numerator = previous - GRAVITY * flow_depth * timestep_s * slope
    denominator = 1 + (
        GRAVITY
        * timestep_s
        * manning**2
        * xp.abs(previous)
        / xp.maximum(flow_depth, 1e-4) ** (7 / 3)
    )
    result = xp.where(active, numerator / denominator, 0)
    froude_limit = (
        maximum_froude_number
        * flow_depth
        * xp.sqrt(GRAVITY * xp.maximum(flow_depth, 0))
    )
    result = xp.clip(result, -froude_limit, froude_limit)
    return result.astype(np.float32, copy=False)


def _limit_outflow(
    discharge_x: np.ndarray,
    discharge_y: np.ndarray,
    depth: np.ndarray,
    timestep_s: float,
    cell_width_m: float,
    cell_height_m: float,
    xp: Any = np,
) -> tuple[np.ndarray, np.ndarray]:
    outflow_m3s = xp.zeros_like(depth)
    outflow_m3s[:, :-1] += xp.maximum(discharge_x, 0) * cell_height_m
    outflow_m3s[:, 1:] += xp.maximum(-discharge_x, 0) * cell_height_m
    outflow_m3s[:-1, :] += xp.maximum(discharge_y, 0) * cell_width_m
    outflow_m3s[1:, :] += xp.maximum(-discharge_y, 0) * cell_width_m
    available_m3 = depth * cell_width_m * cell_height_m
    scale = xp.minimum(
        1,
        available_m3 / xp.maximum(outflow_m3s * timestep_s, 1e-9),
    )
    discharge_x = xp.where(
        discharge_x >= 0,
        discharge_x * scale[:, :-1],
        discharge_x * scale[:, 1:],
    )
    discharge_y = xp.where(
        discharge_y >= 0,
        discharge_y * scale[:-1, :],
        discharge_y * scale[1:, :],
    )
    return discharge_x.astype(np.float32), discharge_y.astype(np.float32)


def _cell_velocity(
    discharge_x: np.ndarray,
    discharge_y: np.ndarray,
    depth: np.ndarray,
    maximum_froude_number: float,
    xp: Any = np,
) -> np.ndarray:
    unit_x = xp.zeros_like(depth)
    unit_y = xp.zeros_like(depth)
    unit_x[:, :-1] += discharge_x * 0.5
    unit_x[:, 1:] += discharge_x * 0.5
    unit_y[:-1, :] += discharge_y * 0.5
    unit_y[1:, :] += discharge_y * 0.5
    velocity = xp.hypot(unit_x, unit_y) / xp.maximum(depth, 0.03)
    velocity = xp.minimum(
        velocity,
        maximum_froude_number * xp.sqrt(GRAVITY * xp.maximum(depth, 0)),
    )
    velocity[depth < 0.03] = 0
    return velocity.astype(np.float32)


def _hazard_features(
    hazard_index: np.ndarray,
    maximum_depth: np.ndarray,
    maximum_velocity: np.ndarray,
    arrival_seconds: np.ndarray,
    mapped: np.ndarray,
    grid_transform: Any,
    cell_area_m2: float,
    route_wgs84: LineString,
    cell_size_m: float,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    definitions = [
        (1, "Menor", "#f4c542", 0.0, 1.0),
        (2, "Moderado", "#f29f24", 1.0, 4.0),
        (3, "Alto", "#ef6c23", 4.0, 10.0),
        (4, "Muito alto", "#d93f20", 10.0, 25.0),
        (5, "Extremo", "#b42318", 25.0, math.inf),
    ]
    to_utm = Transformer.from_crs("EPSG:4326", "EPSG:32723", always_xy=True)
    to_wgs84 = Transformer.from_crs("EPSG:32723", "EPSG:4326", always_xy=True)
    features: list[dict[str, Any]] = []
    raw_features: list[dict[str, Any]] = []

    for severity, label, color, lower, upper in definitions:
        class_mask = mapped & (hazard_index >= lower) & (hazard_index < upper)
        if not class_mask.any():
            continue
        # Geometria cumulativa: classes mais intensas são desenhadas por cima.
        # Isso evita frestas entre contornos suavizados sem alterar as métricas.
        display_mask = mapped & (hazard_index >= lower)
        polygons = [
            shape(geometry)
            for geometry, value in shapes(
                display_mask.astype(np.uint8),
                mask=display_mask,
                transform=grid_transform,
            )
            if value == 1
        ]
        geometry_utm = transform(
            to_utm.transform, unary_union(polygons).buffer(0)
        )
        raw_wgs84 = transform(to_wgs84.transform, geometry_utm)
        smoothing_m = cell_size_m * 0.42
        smoothed_utm = (
            geometry_utm.buffer(smoothing_m, join_style="round")
            .buffer(-smoothing_m, join_style="round")
            .simplify(cell_size_m * 0.62, preserve_topology=True)
        )
        smoothed_utm = _chaikin_geometry(smoothed_utm, iterations=2).buffer(0)
        smoothed_wgs84 = transform(to_wgs84.transform, smoothed_utm)
        class_arrivals = arrival_seconds[class_mask & np.isfinite(arrival_seconds)]
        properties = {
            "label": label,
            "severity": severity,
            "color": color,
            "areaKm2": round(
                float(class_mask.sum() * cell_area_m2) / 1_000_000, 2
            ),
            "meanDepthM": round(float(maximum_depth[class_mask].mean()), 2),
            "maxDepthM": round(float(maximum_depth[class_mask].max()), 2),
            "maxVelocityMs": round(float(maximum_velocity[class_mask].max()), 2),
            "earliestArrivalMin": round(float(class_arrivals.min()) / 60, 1)
            if class_arrivals.size
            else None,
            "method": "inércia local 2D; risco por profundidade × (velocidade + 0,5)",
            "displaySmoothingM": round(smoothing_m, 1),
        }
        features.append(
            {
                "type": "Feature",
                "properties": properties,
                "geometry": mapping(smoothed_wgs84),
            }
        )
        raw_features.append(
            {
                "type": "Feature",
                "properties": {**properties, "displaySmoothingM": 0},
                "geometry": mapping(raw_wgs84),
            }
        )

    features.append(
        {
            "type": "Feature",
            "properties": {
                "label": "Eixo hidráulico de referência",
                "kind": "route",
            },
            "geometry": mapping(route_wgs84),
        }
    )
    raw_features.append(features[-1])
    return features, raw_features


def _chaikin_geometry(geometry: Any, iterations: int) -> Any:
    """Arredonda anéis poligonais apenas para a representação cartográfica."""
    if isinstance(geometry, Polygon):
        exterior = _chaikin_ring(list(geometry.exterior.coords), iterations)
        holes = [
            _chaikin_ring(list(interior.coords), iterations)
            for interior in geometry.interiors
            if interior.length > 1
        ]
        return Polygon(exterior, holes)
    if isinstance(geometry, MultiPolygon):
        polygons = [
            _chaikin_geometry(polygon, iterations)
            for polygon in geometry.geoms
            if not polygon.is_empty
        ]
        return MultiPolygon(polygons)
    return geometry


def _chaikin_ring(
    coordinates: list[tuple[float, float]], iterations: int
) -> list[tuple[float, float]]:
    points = np.asarray(coordinates[:-1], dtype=np.float64)
    if len(points) < 4:
        return coordinates
    for _ in range(iterations):
        following = np.roll(points, -1, axis=0)
        first = 0.75 * points + 0.25 * following
        second = 0.25 * points + 0.75 * following
        expanded = np.empty((len(points) * 2, 2), dtype=np.float64)
        expanded[0::2] = first
        expanded[1::2] = second
        points = expanded
    closed = np.vstack([points, points[0]])
    return [tuple(point) for point in closed]
