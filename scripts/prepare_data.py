"""Prepara os dados geoespaciais do protótipo Itupararanga.

O script baixa um recorte do Copernicus DEM GLO-30, consulta a hidrografia
visível no OpenStreetMap e gera artefatos leves para a interface web.
Dados brutos ficam fora do Git; os produtos derivados ficam em public/data.
"""

from __future__ import annotations

import heapq
import json
import math
from collections import defaultdict
from pathlib import Path
from typing import Any

import numpy as np
import rasterio
import requests
from PIL import Image
from pyproj import Transformer
from rasterio.enums import Resampling
from rasterio.features import rasterize, shapes
from rasterio.transform import from_bounds as transform_from_bounds
from rasterio.transform import rowcol
from rasterio.windows import from_bounds
from scipy.ndimage import distance_transform_edt, label, percentile_filter
from shapely.geometry import LineString, mapping, shape
from shapely.ops import polygonize, transform, unary_union

from hydrodynamic import run_hydrodynamic_simulation


ROOT = Path(__file__).resolve().parents[1]
RAW_DIR = ROOT / "data" / "raw"
OUTPUT_DIR = ROOT / "public" / "data"
PROCESSED_DIR = ROOT / "data" / "processed"

DEM_URL = (
    "https://copernicus-dem-30m.s3.amazonaws.com/"
    "Copernicus_DSM_COG_10_S24_00_W048_00_DEM/"
    "Copernicus_DSM_COG_10_S24_00_W048_00_DEM.tif"
)
DEM_PATH = RAW_DIR / "Copernicus_DSM_COG_10_S24_00_W048_00_DEM.tif"
OSM_PATH = RAW_DIR / "osm_hydrography.json"
MUNICIPALITY_URL = (
    "https://servicodados.ibge.gov.br/api/v3/malhas/municipios/3552205"
    "?formato=application/vnd.geo+json&qualidade=intermediaria"
)
MUNICIPALITY_PATH = RAW_DIR / "sorocaba-boundary.geojson"

# Recorte que inclui a represa, Votorantim, Sorocaba e o vale a jusante.
BOUNDS = {
    "west": -47.68,
    "south": -23.78,
    "east": -47.05,
    "north": -23.28,
}
MAX_OUTPUT_WIDTH = 1_400

DAM = {
    "name": "Barragem de Itupararanga",
    "longitude": -(47 + 23 / 60 + 53 / 3600),
    "latitude": -(23 + 36 / 60 + 44 / 3600),
    "height_m": 35,
    "crest_length_m": 415,
    "total_capacity_hm3": 302.087,
    "useful_volume_hm3": 247.57,
}

SOROCABA_DOWNSTREAM_TARGET = (-47.462, -23.405)

COPERNICUS_NOTICE = (
    "produced using Copernicus WorldDEM-30 © DLR e.V. 2010-2014 and "
    "© Airbus Defence and Space GmbH 2014-2018 provided under "
    "COPERNICUS by the European Union and ESA; all rights reserved"
)


def download_file(url: str, destination: Path) -> None:
    if destination.exists() and destination.stat().st_size > 1_000_000:
        return

    print(f"Baixando {destination.name}...")
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_suffix(destination.suffix + ".part")
    with requests.get(url, stream=True, timeout=180) as response:
        response.raise_for_status()
        with temporary.open("wb") as file:
            for chunk in response.iter_content(chunk_size=1024 * 1024):
                if chunk:
                    file.write(chunk)
    temporary.replace(destination)


def hillshade(elevation: np.ndarray, cell_size_m: float) -> np.ndarray:
    gradient_y, gradient_x = np.gradient(elevation, cell_size_m, cell_size_m)
    slope = np.pi / 2 - np.arctan(np.hypot(gradient_x, gradient_y))
    aspect = np.arctan2(-gradient_x, gradient_y)
    azimuth = math.radians(315)
    altitude = math.radians(42)
    shaded = (
        np.sin(altitude) * np.sin(slope)
        + np.cos(altitude) * np.cos(slope) * np.cos(azimuth - aspect)
    )
    normalized = np.clip((shaded + 1) * 127.5, 0, 255)
    return normalized.astype(np.uint8)


def prepare_dem() -> tuple[dict[str, Any], np.ndarray]:
    download_file(DEM_URL, DEM_PATH)
    print("Processando relevo...")

    with rasterio.open(DEM_PATH) as source:
        window = from_bounds(
            BOUNDS["west"],
            BOUNDS["south"],
            BOUNDS["east"],
            BOUNDS["north"],
            transform=source.transform,
        ).round_offsets().round_lengths()
        scale = min(1.0, MAX_OUTPUT_WIDTH / window.width)
        output_width = max(1, round(window.width * scale))
        output_height = max(1, round(window.height * scale))
        elevation = source.read(
            1,
            window=window,
            out_shape=(output_height, output_width),
            resampling=Resampling.bilinear,
        ).astype(np.float32)
        actual_bounds = rasterio.windows.bounds(window, source.transform)

    valid = np.isfinite(elevation) & (elevation > -100)
    minimum = float(np.nanmin(elevation[valid]))
    maximum = float(np.nanmax(elevation[valid]))
    quantized = np.zeros_like(elevation, dtype=np.uint8)
    quantized[valid] = np.clip(
        np.rint((elevation[valid] - minimum) / (maximum - minimum) * 255),
        0,
        255,
    ).astype(np.uint8)

    # O segundo canal carrega luz/sombra. Isso permite recolorir o relevo no
    # navegador sem perder a leitura das encostas.
    shade = hillshade(np.where(valid, elevation, minimum), cell_size_m=60)
    alpha = np.where(valid, 255, 0).astype(np.uint8)
    texture = np.dstack([quantized, shade, np.zeros_like(quantized), alpha])
    Image.fromarray(texture).save(OUTPUT_DIR / "terrain.png", optimize=True)

    percentiles = [12, 27, 42, 57, 72, 87]
    suggested_breaks = [
        int(round(value / 5) * 5)
        for value in np.percentile(elevation[valid], percentiles)
    ]
    # Evita controles sobrepostos em recortes com pouca variação.
    for index in range(1, len(suggested_breaks)):
        suggested_breaks[index] = max(
            suggested_breaks[index], suggested_breaks[index - 1] + 5
        )

    metadata = {
        "width": output_width,
        "height": output_height,
        "bounds": {
            "west": actual_bounds[0],
            "south": actual_bounds[1],
            "east": actual_bounds[2],
            "north": actual_bounds[3],
        },
        "elevationMinM": round(minimum, 2),
        "elevationMaxM": round(maximum, 2),
        "suggestedBreaksM": suggested_breaks,
        "source": "Copernicus DEM GLO-30",
        "sourceUrl": "https://dataspace.copernicus.eu/explore-data/data-collections/copernicus-contributing-missions/collections-description/COP-DEM",
        "resolution": "aproximadamente 30 m na origem; textura de exibição reamostrada",
        "notice": COPERNICUS_NOTICE,
    }
    write_json(OUTPUT_DIR / "terrain-metadata.json", metadata)
    return metadata, elevation


def fetch_osm() -> dict[str, Any]:
    if OSM_PATH.exists() and OSM_PATH.stat().st_size > 10_000:
        cached = json.loads(OSM_PATH.read_text(encoding="utf-8"))
        if any(element.get("tags", {}).get("highway") for element in cached.get("elements", [])):
            return cached

    bbox = (
        f'{BOUNDS["south"]},{BOUNDS["west"]},'
        f'{BOUNDS["north"]},{BOUNDS["east"]}'
    )
    road_bbox = "-23.66,-47.58,-23.36,-47.32"
    query = f"""
    [out:json][timeout:240];
    (
      way["waterway"~"^(river|stream|canal)$"]({bbox});
      relation["waterway"~"^(river|stream|canal)$"]({bbox});
      way["natural"="water"]["name"~"Itupararanga",i]({bbox});
      relation["natural"="water"]["name"~"Itupararanga",i]({bbox});
      way["water"="reservoir"]["name"~"Itupararanga",i]({bbox});
      relation["water"="reservoir"]["name"~"Itupararanga",i]({bbox});
      way["highway"~"^(motorway|trunk|primary|secondary|tertiary|residential|unclassified|living_street)$"]({road_bbox});
    );
    out body geom;
    """
    print("Consultando hidrografia e ruas no OpenStreetMap...")
    response = requests.post(
        "https://overpass-api.de/api/interpreter",
        data={"data": query},
        headers={"User-Agent": "itupararanga-flood-lab/0.1"},
        timeout=300,
    )
    response.raise_for_status()
    data = response.json()
    OSM_PATH.parent.mkdir(parents=True, exist_ok=True)
    OSM_PATH.write_text(json.dumps(data), encoding="utf-8")
    return data


def geometry_coordinates(geometry: list[dict[str, float]]) -> list[list[float]]:
    return [[point["lon"], point["lat"]] for point in geometry]


def prepare_hydrography(osm: dict[str, Any]) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    rivers: list[dict[str, Any]] = []
    reservoirs: list[dict[str, Any]] = []
    seen_river_geometries: set[tuple[tuple[float, float], ...]] = set()

    for element in osm.get("elements", []):
        tags = element.get("tags", {})
        geometry = element.get("geometry")

        if geometry and tags.get("waterway") in {"river", "stream", "canal"}:
            coordinates = geometry_coordinates(geometry)
            if len(coordinates) < 2:
                continue
            key = tuple((round(x, 7), round(y, 7)) for x, y in coordinates)
            if key in seen_river_geometries:
                continue
            seen_river_geometries.add(key)
            rivers.append(
                {
                    "type": "Feature",
                    "properties": {
                        "name": tags.get("name", "Curso d'água sem nome"),
                        "waterway": tags.get("waterway"),
                        "source": "OpenStreetMap contributors",
                    },
                    "geometry": {"type": "LineString", "coordinates": coordinates},
                }
            )

        name = str(tags.get("name", ""))
        if "itupararanga" not in name.lower():
            continue

        if element.get("type") == "way" and geometry:
            coordinates = geometry_coordinates(geometry)
            if len(coordinates) >= 4 and coordinates[0] == coordinates[-1]:
                reservoirs.append(
                    {
                        "type": "Feature",
                        "properties": {
                            "name": name,
                            "kind": "reservoir",
                            "source": "OpenStreetMap contributors",
                        },
                        "geometry": {"type": "Polygon", "coordinates": [coordinates]},
                    }
                )
        elif element.get("type") == "relation":
            outer_lines: list[LineString] = []
            for member in element.get("members", []):
                member_geometry = member.get("geometry")
                if member.get("role") == "outer" and member_geometry:
                    coordinates = geometry_coordinates(member_geometry)
                    if len(coordinates) >= 2:
                        outer_lines.append(LineString(coordinates))
            if outer_lines:
                polygons = list(polygonize(unary_union(outer_lines)))
                for polygon in polygons:
                    reservoirs.append(
                        {
                            "type": "Feature",
                            "properties": {
                                "name": name,
                                "kind": "reservoir",
                                "source": "OpenStreetMap contributors",
                            },
                            "geometry": mapping(polygon),
                        }
                    )

    write_json(
        OUTPUT_DIR / "hydrography.geojson",
        {"type": "FeatureCollection", "features": rivers + reservoirs},
    )
    return rivers, reservoirs


def prepare_roads(osm: dict[str, Any]) -> list[dict[str, Any]]:
    road_groups: dict[str, list[list[tuple[float, float]]]] = defaultdict(list)
    allowed = {
        "motorway",
        "trunk",
        "primary",
        "secondary",
        "tertiary",
        "residential",
        "unclassified",
        "living_street",
    }
    for element in osm.get("elements", []):
        tags = element.get("tags", {})
        geometry = element.get("geometry")
        highway = tags.get("highway")
        if highway not in allowed or not geometry:
            continue
        coordinates = geometry_coordinates(geometry)
        if len(coordinates) < 2:
            continue
        simplified = LineString(coordinates).simplify(0.000025, preserve_topology=False)
        road_groups[highway].append(list(simplified.coords))

    # Uma feição MultiLineString por classe evita milhares de objetos e
    # propriedades repetidas no GeoJSON e reduz o custo de parse no navegador.
    roads = [
        {
            "type": "Feature",
            "properties": {
                "highway": highway,
                "segmentCount": len(lines),
                "source": "OpenStreetMap contributors",
            },
            "geometry": {
                "type": "MultiLineString",
                "coordinates": lines,
            },
        }
        for highway, lines in road_groups.items()
    ]
    write_json(
        OUTPUT_DIR / "roads.geojson",
        {"type": "FeatureCollection", "features": roads},
    )
    return roads


def prepare_municipality() -> dict[str, Any]:
    if MUNICIPALITY_PATH.exists() and MUNICIPALITY_PATH.stat().st_size > 1_000:
        collection = json.loads(MUNICIPALITY_PATH.read_text(encoding="utf-8"))
    else:
        print("Baixando o limite municipal de Sorocaba no IBGE...")
        response = requests.get(
            MUNICIPALITY_URL,
            headers={"User-Agent": "itupararanga-flood-lab/0.1"},
            timeout=120,
        )
        response.raise_for_status()
        collection = response.json()
        MUNICIPALITY_PATH.parent.mkdir(parents=True, exist_ok=True)
        MUNICIPALITY_PATH.write_text(
            json.dumps(collection, ensure_ascii=False), encoding="utf-8"
        )

    for feature in collection.get("features", []):
        feature.setdefault("properties", {})
        feature["properties"].update(
            {
                "name": "Sorocaba",
                "code": "3552205",
                "source": "IBGE — Malhas Territoriais",
            }
        )
    write_json(OUTPUT_DIR / "sorocaba-boundary.geojson", collection)
    return collection


def distance_approx(a: tuple[float, float], b: tuple[float, float]) -> float:
    lon_scale = 111_320 * math.cos(math.radians((a[1] + b[1]) / 2))
    dx = (a[0] - b[0]) * lon_scale
    dy = (a[1] - b[1]) * 110_540
    return math.hypot(dx, dy)


def build_river_graph(
    rivers: list[dict[str, Any]],
) -> dict[tuple[float, float], list[tuple[tuple[float, float], float]]]:
    graph: dict[
        tuple[float, float], list[tuple[tuple[float, float], float]]
    ] = defaultdict(list)
    for feature in rivers:
        coordinates = feature["geometry"]["coordinates"]
        for raw_a, raw_b in zip(coordinates, coordinates[1:]):
            a = (round(raw_a[0], 7), round(raw_a[1], 7))
            b = (round(raw_b[0], 7), round(raw_b[1], 7))
            weight = distance_approx(a, b)
            graph[a].append((b, weight))
            graph[b].append((a, weight))
    return graph


def nearest_node(
    nodes: Any, target: tuple[float, float], maximum_m: float
) -> tuple[float, float]:
    node, distance = min(
        ((candidate, distance_approx(candidate, target)) for candidate in nodes),
        key=lambda item: item[1],
    )
    if distance > maximum_m:
        raise RuntimeError(
            f"Não há curso d'água a menos de {maximum_m:.0f} m de {target}."
        )
    return node


def shortest_path(
    graph: dict[tuple[float, float], list[tuple[tuple[float, float], float]]],
    start: tuple[float, float],
    end: tuple[float, float],
) -> list[tuple[float, float]]:
    distances = {start: 0.0}
    previous: dict[tuple[float, float], tuple[float, float]] = {}
    queue = [(0.0, start)]

    while queue:
        current_distance, current = heapq.heappop(queue)
        if current == end:
            break
        if current_distance != distances.get(current):
            continue
        for neighbor, weight in graph[current]:
            candidate = current_distance + weight
            if candidate < distances.get(neighbor, math.inf):
                distances[neighbor] = candidate
                previous[neighbor] = current
                heapq.heappush(queue, (candidate, neighbor))

    if end not in distances:
        raise RuntimeError("A rede hidrográfica não conectou a barragem à área-alvo.")

    path = [end]
    while path[-1] != start:
        path.append(previous[path[-1]])
    path.reverse()
    return path


def downstream_path(rivers: list[dict[str, Any]]) -> LineString:
    graph = build_river_graph(rivers)
    dam_point = (DAM["longitude"], DAM["latitude"])
    try:
        start = nearest_node(graph.keys(), dam_point, maximum_m=2_500)
        end = nearest_node(
            graph.keys(), SOROCABA_DOWNSTREAM_TARGET, maximum_m=5_000
        )
        path = shortest_path(graph, start, end)
        return LineString([dam_point, *path])
    except (RuntimeError, ValueError):
        # Eixo conservador para que o preparo continue se o Overpass retornar
        # uma rede temporariamente incompleta. A interface sinaliza o caráter
        # preliminar dessas zonas.
        return LineString(
            [
                dam_point,
                (-47.401, -23.600),
                (-47.414, -23.581),
                (-47.430, -23.558),
                (-47.443, -23.530),
                (-47.456, -23.502),
                (-47.462, -23.470),
                SOROCABA_DOWNSTREAM_TARGET,
            ]
        )


def prepare_damage_zones(
    elevation: np.ndarray,
    metadata: dict[str, Any],
    rivers: list[dict[str, Any]],
    reservoirs: list[dict[str, Any]],
) -> dict[str, float]:
    """Gera uma mancha topográfica conectada ao eixo do Rio Sorocaba.

    Esta etapa não resolve as equações de águas rasas. Ela melhora a triagem
    anterior ao condicionar cada célula à elevação, à superfície d'água local,
    à distância lateral e à conexão contínua com o vale a jusante.
    """

    route_wgs84 = downstream_path(rivers)
    to_utm = Transformer.from_crs("EPSG:4326", "EPSG:32723", always_xy=True)
    to_wgs84 = Transformer.from_crs("EPSG:32723", "EPSG:4326", always_xy=True)
    route_utm = transform(to_utm.transform, route_wgs84)
    route_length_m = route_utm.length

    height, width = elevation.shape
    bounds = metadata["bounds"]
    grid_transform = transform_from_bounds(
        bounds["west"],
        bounds["south"],
        bounds["east"],
        bounds["north"],
        width,
        height,
    )
    center_latitude = (bounds["north"] + bounds["south"]) / 2
    cell_width_m = (
        (bounds["east"] - bounds["west"])
        * 111_320
        * math.cos(math.radians(center_latitude))
        / width
    )
    cell_height_m = (
        (bounds["north"] - bounds["south"]) * 110_540 / height
    )
    cell_area_m2 = cell_width_m * cell_height_m

    sample_distances = np.arange(0, route_length_m + 40, 40, dtype=np.float32)
    sample_distances[-1] = min(sample_distances[-1], route_length_m)
    sample_wgs84 = [
        transform(to_wgs84.transform, route_utm.interpolate(float(distance)))
        for distance in sample_distances
    ]
    sample_rows: list[int] = []
    sample_columns: list[int] = []
    valid_distances: list[float] = []
    raw_bed: list[float] = []
    for distance, point in zip(sample_distances, sample_wgs84):
        row, column = rowcol(grid_transform, point.x, point.y)
        if 0 <= row < height and 0 <= column < width:
            sample_rows.append(row)
            sample_columns.append(column)
            valid_distances.append(float(distance))
            raw_bed.append(float(elevation[row, column]))

    if len(raw_bed) < 2:
        raise RuntimeError("O eixo hidráulico ficou fora do recorte de relevo.")

    bed_profile = percentile_filter(
        np.asarray(raw_bed, dtype=np.float32),
        percentile=25,
        size=25,
        mode="nearest",
    )
    # O talvegue deve perder cota no sentido de jusante; a acumulação mínima
    # remove picos espúrios de vegetação, pontes e edificações presentes no DSM.
    bed_profile = np.minimum.accumulate(bed_profile)

    chainage_seeds = np.full((height, width), np.nan, dtype=np.float32)
    bed_seeds = np.full((height, width), np.nan, dtype=np.float32)
    for row, column, distance, bed in zip(
        sample_rows, sample_columns, valid_distances, bed_profile
    ):
        chainage_seeds[row, column] = distance
        bed_seeds[row, column] = bed

    seed_mask = np.isfinite(chainage_seeds)
    distance_to_route, nearest = distance_transform_edt(
        ~seed_mask,
        sampling=(cell_height_m, cell_width_m),
        return_indices=True,
    )
    nearest_rows, nearest_columns = nearest
    chainage = chainage_seeds[nearest_rows, nearest_columns]
    local_bed = bed_seeds[nearest_rows, nearest_columns]

    # Envelope de altura da onda para triagem: 24 m junto à ruptura, com
    # atenuação exponencial e cauda de 1,2 m no trecho distante.
    hydraulic_depth = 1.2 + 22.8 * np.exp(-chainage / 14_000)
    water_surface = local_bed + hydraulic_depth
    raw_depth = water_surface - elevation
    lateral_limit = 350 + 2_600 * np.exp(-chainage / 20_000)

    reservoir_geometries = [
        shape(feature["geometry"])
        for feature in reservoirs
        if feature.get("geometry")
    ]
    reservoir_mask = rasterize(
        [(geometry, 1) for geometry in reservoir_geometries],
        out_shape=(height, width),
        transform=grid_transform,
        fill=0,
        all_touched=True,
        dtype=np.uint8,
    ).astype(bool)

    longitude_centers = bounds["west"] + (
        (np.arange(width, dtype=np.float32) + 0.5) / width
    ) * (bounds["east"] - bounds["west"])
    upstream_side = (
        (longitude_centers[np.newaxis, :] > DAM["longitude"] + 0.001)
        & (chainage < 3_000)
    )
    candidate = (
        (raw_depth >= 0.3)
        & (distance_to_route <= lateral_limit)
        & (chainage <= route_length_m)
        & ~reservoir_mask
        & ~upstream_side
    )

    connected_labels, _ = label(
        candidate,
        structure=np.ones((3, 3), dtype=np.uint8),
    )
    route_labels = np.unique(
        connected_labels[candidate & (distance_to_route <= max(cell_width_m, cell_height_m) * 2)]
    )
    route_labels = route_labels[route_labels != 0]
    connected = np.isin(connected_labels, route_labels)
    # O DSM pode conter depressões laterais artificiais. A profundidade fica
    # limitada a 30 m nesta triagem para não transformar esses artefatos em
    # resultados extremos sem sustentação hidráulica.
    depth = np.where(connected, np.clip(raw_depth, 0, 30), 0).astype(np.float32)

    severity = np.zeros((height, width), dtype=np.uint8)
    severity[(depth >= 0.3) & (depth < 1.5)] = 1
    severity[(depth >= 1.5) & (depth < 3.0)] = 2
    severity[(depth >= 3.0) & (depth < 5.0)] = 3
    severity[(depth >= 5.0) & (depth < 8.0)] = 4
    severity[depth >= 8.0] = 5

    definitions = {
        1: ("Menor", "#f4c542"),
        2: ("Moderado", "#f29f24"),
        3: ("Alto", "#ef6c23"),
        4: ("Muito alto", "#d93f20"),
        5: ("Extremo", "#b42318"),
    }
    features: list[dict[str, Any]] = []
    for severity_value, (severity_label, color) in definitions.items():
        mask = severity == severity_value
        if not mask.any():
            continue
        polygons = [
            shape(geometry)
            for geometry, value in shapes(
                mask.astype(np.uint8),
                mask=mask,
                transform=grid_transform,
            )
            if value == 1
        ]
        merged_wgs84 = unary_union(polygons)
        merged_utm = transform(to_utm.transform, merged_wgs84).buffer(0)
        simplified_utm = merged_utm.simplify(22, preserve_topology=True)
        simplified_wgs84 = transform(to_wgs84.transform, simplified_utm)
        class_depth = depth[mask]
        class_chainage = chainage[mask]
        features.append(
            {
                "type": "Feature",
                "properties": {
                    "label": severity_label,
                    "severity": severity_value,
                    "color": color,
                    "startKm": round(float(class_chainage.min()) / 1_000, 2),
                    "endKm": round(float(class_chainage.max()) / 1_000, 2),
                    "areaKm2": round(float(mask.sum() * cell_area_m2) / 1_000_000, 2),
                    "meanDepthM": round(float(class_depth.mean()), 2),
                    "maxDepthM": round(float(class_depth.max()), 2),
                    "method": "envelope topográfico conectado ao vale",
                },
                "geometry": mapping(simplified_wgs84),
            }
        )

    features.append(
        {
            "type": "Feature",
            "properties": {
                "label": "Eixo hidráulico preliminar",
                "kind": "route",
                "lengthKm": round(route_length_m / 1_000, 2),
            },
            "geometry": mapping(route_wgs84),
        }
    )
    write_json(
        OUTPUT_DIR / "damage-zones.geojson",
        {"type": "FeatureCollection", "features": features},
    )

    flooded = depth >= 0.3
    return {
        "floodedAreaKm2": round(float(flooded.sum() * cell_area_m2) / 1_000_000, 2),
        "concurrentEnvelopeVolumeHm3": round(
            float((depth * cell_area_m2).sum()) / 1_000_000, 2
        ),
        "maximumDepthM": round(float(depth.max()), 2),
        "routeLengthKm": round(route_length_m / 1_000, 2),
        "cellWidthM": round(cell_width_m, 1),
        "cellHeightM": round(cell_height_m, 1),
    }


def prepare_hydrodynamic_zones(
    elevation: np.ndarray,
    metadata: dict[str, Any],
    rivers: list[dict[str, Any]],
    reservoirs: list[dict[str, Any]],
) -> dict[str, Any]:
    route_wgs84 = downstream_path(rivers)
    features, raw_features, simulation = run_hydrodynamic_simulation(
        elevation,
        metadata,
        rivers,
        reservoirs,
        route_wgs84,
        DAM,
        PROCESSED_DIR,
    )
    write_json(
        OUTPUT_DIR / "damage-zones.geojson",
        {"type": "FeatureCollection", "features": features},
    )
    write_json(
        OUTPUT_DIR / "damage-zones-raw.geojson",
        {"type": "FeatureCollection", "features": raw_features},
    )
    write_json(OUTPUT_DIR / "hydrodynamic-summary.json", simulation)
    return simulation


def prepare_scenario(simulation: dict[str, Any]) -> None:
    released_fraction = DAM["useful_volume_hm3"] / DAM["total_capacity_hm3"]
    scenario = {
        "dam": DAM,
        "baseline": {
            "storedVolumeHm3": DAM["total_capacity_hm3"],
            "releasedVolumeHm3": DAM["useful_volume_hm3"],
            "releaseFraction": released_fraction,
            "residualVolumeHm3": round(
                DAM["total_capacity_hm3"] - DAM["useful_volume_hm3"], 3
            ),
            "upperBoundReleasedVolumeHm3": DAM["total_capacity_hm3"],
        },
        "hydrodynamicSimulation": simulation,
        "sources": [
            {
                "label": "Cadastro de barragens do Estado de São Paulo (2019)",
                "url": "https://arquivo.ambiente.sp.gov.br/publicacoes/2019/07/barragens-noestadodespaulo2019.pdf",
                "supports": "coordenadas, altura, comprimento e capacidade total",
            },
            {
                "label": "ANEEL — volume útil de Itupararanga",
                "url": "https://www2.aneel.gov.br/cedoc/reh2006341.pdf",
                "supports": "volume útil de 247,57 hm³",
            },
            {
                "label": "ANA — metodologia simplificada para áreas inundadas",
                "url": "https://www.ana.gov.br/arquivos/imprensa/eventosprojetos/20130717_AreasDeInundacaoAplicacaoDaMetodologia_JoseMelo.pdf",
                "supports": "cruzamento de níveis máximos com a topografia em uma triagem simplificada",
            },
            {
                "label": "USACE — análise de ruptura com áreas 2D no HEC-RAS",
                "url": "https://www.hec.usace.army.mil/confluence/rasdocs/hgt/latest/tutorials/2d-unsteady-flow/dam-breach-analysis-with-2d-areas",
                "supports": "requisitos da futura propagação hidrodinâmica 2D",
            },
            {
                "label": "IBGE — Malha Municipal de Sorocaba (código 3552205)",
                "url": "https://www.ibge.gov.br/cidades-e-estados/sp/sorocaba.html",
                "supports": "limite territorial oficial exibido no mapa",
            },
            {
                "label": "OpenStreetMap contributors",
                "url": "https://www.openstreetmap.org/copyright",
                "supports": "rede viária, hidrografia e geometria do reservatório",
            },
        ],
        "limitations": [
            "O volume-base liberado usa o volume útil como parcela mobilizável; o limite superior usa toda a capacidade cadastrada.",
            "A propagação resolve continuidade e momento pela aproximação 2D de inércia local, com molhamento, secagem e atrito de Manning.",
            "O hidrograma triangular experimental libera 247,57 hm³, atinge pico configurado de 15.000 m³/s em 60 minutos e ainda não deriva de uma brecha calibrada.",
            "O relevo usa Copernicus DSM reduzido para a grade hidráulica e condicionamento simples do leito; pontes, aterros e microdrenagem ainda não foram representados individualmente.",
            "Ainda faltam curva cota-volume, geometria oficial da brecha, rugosidade espacial calibrada, condições de contorno e validação contra marcas ou estudos de campo.",
            "Não utilizar para evacuação, licenciamento ou tomada de decisão operacional.",
        ],
    }
    write_json(OUTPUT_DIR / "scenario.json", scenario)
    write_json(
        OUTPUT_DIR / "dam.geojson",
        {
            "type": "FeatureCollection",
            "features": [
                {
                    "type": "Feature",
                    "properties": {
                        "name": DAM["name"],
                        "heightM": DAM["height_m"],
                        "crestLengthM": DAM["crest_length_m"],
                    },
                    "geometry": {
                        "type": "Point",
                        "coordinates": [DAM["longitude"], DAM["latitude"]],
                    },
                }
            ],
        },
    )


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(value, ensure_ascii=False, separators=(",", ":")),
        encoding="utf-8",
    )


def main() -> None:
    RAW_DIR.mkdir(parents=True, exist_ok=True)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    metadata, elevation = prepare_dem()
    osm = fetch_osm()
    rivers, reservoirs = prepare_hydrography(osm)
    roads = prepare_roads(osm)
    road_segment_count = sum(
        int(feature["properties"]["segmentCount"]) for feature in roads
    )
    prepare_municipality()
    print("Executando propagação hidrodinâmica 2D experimental...")
    simulation = prepare_hydrodynamic_zones(
        elevation, metadata, rivers, reservoirs
    )
    prepare_scenario(simulation)
    print(
        "Dados preparados: "
        f'{metadata["width"]}×{metadata["height"]} px, '
        f"{len(rivers)} trechos de drenagem, "
        f"{road_segment_count} trechos viários em {len(roads)} classes, "
        f"{len(reservoirs)} geometrias de Itupararanga, "
        f'{simulation["floodedAreaKm2"]} km² na propagação 2D.'
    )


if __name__ == "__main__":
    main()
