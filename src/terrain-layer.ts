import L from "leaflet";

export interface TerrainMetadata {
  width: number;
  height: number;
  bounds: {
    west: number;
    south: number;
    east: number;
    north: number;
  };
  elevationMinM: number;
  elevationMaxM: number;
  suggestedBreaksM: number[];
  source: string;
  sourceUrl: string;
  resolution: string;
  notice: string;
}

export interface ElevationBand {
  upperM: number;
  color: string;
}

export class TerrainLayer extends L.GridLayer {
  private readonly sourcePixels: Uint8ClampedArray;
  private bands: ElevationBand[];
  private readonly liveTiles = new Map<HTMLCanvasElement, L.Coords>();
  private repaintFrame: number | undefined;

  constructor(
    private readonly metadata: TerrainMetadata,
    sourceImage: ImageData,
    bands: ElevationBand[],
  ) {
    super({
      tileSize: 256,
      opacity: 0.94,
      updateWhenZooming: false,
      keepBuffer: 1,
      bounds: L.latLngBounds(
        [metadata.bounds.south, metadata.bounds.west],
        [metadata.bounds.north, metadata.bounds.east],
      ),
      minZoom: 8,
      maxZoom: 15,
      pane: "terrainPane",
      attribution: `Relevo: <a href="${metadata.sourceUrl}">${metadata.source}</a>`,
    });
    this.sourcePixels = sourceImage.data;
    this.bands = normalizeBands(bands);
    this.on("tileunload", (event: L.TileEvent) => {
      if (event.tile instanceof HTMLCanvasElement) {
        this.liveTiles.delete(event.tile);
      }
    });
  }

  setBands(bands: ElevationBand[]): void {
    this.bands = normalizeBands(bands);
    if (this.repaintFrame !== undefined) {
      window.cancelAnimationFrame(this.repaintFrame);
    }
    this.repaintFrame = window.requestAnimationFrame(() => {
      // Os canvases permanecem visíveis durante todo o recálculo. O navegador
      // apresenta os novos pixels no mesmo frame, sem revelar o mapa-base.
      this.liveTiles.forEach((coords, tile) => this.paintTile(tile, coords, 256));
      this.repaintFrame = undefined;
    });
  }

  createTile(coords: L.Coords, done: L.DoneCallback): HTMLElement {
    const canvas = document.createElement("canvas");
    const size = 256;
    canvas.width = size;
    canvas.height = size;
    canvas.setAttribute("role", "presentation");
    canvas.dataset.terrainTile = `${coords.z}/${coords.x}/${coords.y}`;
    this.liveTiles.set(canvas, L.point(coords.x, coords.y) as L.Coords);
    this.liveTiles.get(canvas)!.z = coords.z;

    requestAnimationFrame(() => {
      try {
        this.paintTile(canvas, coords, size);
        done(undefined, canvas);
      } catch (error) {
        done(error instanceof Error ? error : new Error(String(error)), canvas);
      }
    });
    return canvas;
  }

  private paintTile(canvas: HTMLCanvasElement, coords: L.Coords, size: number): void {
    const context = canvas.getContext("2d", { alpha: true });
    if (!context) {
      throw new Error("Canvas 2D indisponível.");
    }

    const output = context.createImageData(size, size);
    const outputPixels = output.data;
    const scale = 2 ** coords.z;
    const xIndices = new Int32Array(size);
    const xValid = new Uint8Array(size);
    const colors = this.bands.map((band) => parseHexColor(band.color));

    for (let x = 0; x < size; x += 1) {
      const longitude = ((coords.x + x / size) / scale) * 360 - 180;
      const sourceX = Math.round(
        ((longitude - this.metadata.bounds.west) /
          (this.metadata.bounds.east - this.metadata.bounds.west)) *
          (this.metadata.width - 1),
      );
      xIndices[x] = sourceX;
      xValid[x] = sourceX >= 0 && sourceX < this.metadata.width ? 1 : 0;
    }

    for (let y = 0; y < size; y += 1) {
      const mercatorY = Math.PI * (1 - (2 * (coords.y + y / size)) / scale);
      const latitude = (Math.atan(Math.sinh(mercatorY)) * 180) / Math.PI;
      const sourceY = Math.round(
        ((this.metadata.bounds.north - latitude) /
          (this.metadata.bounds.north - this.metadata.bounds.south)) *
          (this.metadata.height - 1),
      );
      if (sourceY < 0 || sourceY >= this.metadata.height) {
        continue;
      }

      for (let x = 0; x < size; x += 1) {
        if (!xValid[x]) {
          continue;
        }
        const sourceOffset = (sourceY * this.metadata.width + xIndices[x]) * 4;
        const sourceAlpha = this.sourcePixels[sourceOffset + 3];
        if (sourceAlpha === 0) {
          continue;
        }

        const elevation =
          this.metadata.elevationMinM +
          (this.sourcePixels[sourceOffset] / 255) *
            (this.metadata.elevationMaxM - this.metadata.elevationMinM);
        const bandIndex = findBandIndex(this.bands, elevation);
        const base = colors[bandIndex];
        const shade = this.sourcePixels[sourceOffset + 1] / 255;
        const light = 0.66 + shade * 0.52;
        const outputOffset = (y * size + x) * 4;
        outputPixels[outputOffset] = Math.min(255, Math.round(base[0] * light));
        outputPixels[outputOffset + 1] = Math.min(
          255,
          Math.round(base[1] * light),
        );
        outputPixels[outputOffset + 2] = Math.min(
          255,
          Math.round(base[2] * light),
        );
        outputPixels[outputOffset + 3] = sourceAlpha;
      }
    }

    context.putImageData(output, 0, 0);
  }
}

export async function loadTerrainTexture(url: string): Promise<ImageData> {
  const image = new Image();
  image.src = url;
  await image.decode();
  const canvas = document.createElement("canvas");
  canvas.width = image.naturalWidth;
  canvas.height = image.naturalHeight;
  const context = canvas.getContext("2d", { willReadFrequently: true });
  if (!context) {
    throw new Error("Não foi possível ler a textura de relevo.");
  }
  context.drawImage(image, 0, 0);
  return context.getImageData(0, 0, canvas.width, canvas.height);
}

function normalizeBands(bands: ElevationBand[]): ElevationBand[] {
  return bands
    .map((band) => ({ ...band }))
    .sort((a, b) => a.upperM - b.upperM);
}

function findBandIndex(bands: ElevationBand[], elevation: number): number {
  for (let index = 0; index < bands.length; index += 1) {
    if (elevation <= bands[index].upperM) {
      return index;
    }
  }
  return bands.length - 1;
}

function parseHexColor(hex: string): [number, number, number] {
  const normalized = hex.replace("#", "");
  return [
    Number.parseInt(normalized.slice(0, 2), 16),
    Number.parseInt(normalized.slice(2, 4), 16),
    Number.parseInt(normalized.slice(4, 6), 16),
  ];
}
