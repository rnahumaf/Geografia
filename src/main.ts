import L, { type GeoJSON as LeafletGeoJSON } from "leaflet";
import "leaflet/dist/leaflet.css";
import "./styles.css";
import {
  appApi,
  type ComputeCapabilities,
  type OpenRouterSettings,
  type ResearchResult,
} from "./app-api";
import {
  DEFAULT_RELEASE_FRACTION,
  TOTAL_CAPACITY_HM3,
  estimateReleasedVolume,
  formatHm3,
} from "./scenario";
import {
  TerrainLayer,
  loadTerrainTexture,
  type ElevationBand,
  type TerrainMetadata,
} from "./terrain-layer";
import { exportMap, type ExportFormat } from "./map-export";

interface ScenarioData {
  dam: {
    name: string;
    longitude: number;
    latitude: number;
    height_m: number;
    crest_length_m: number;
    total_capacity_hm3: number;
    useful_volume_hm3: number;
  };
  baseline: {
    storedVolumeHm3: number;
    releasedVolumeHm3: number;
    releaseFraction: number;
    residualVolumeHm3: number;
    upperBoundReleasedVolumeHm3: number;
  };
  hydrodynamicSimulation: {
    model: string;
    status: string;
    floodedAreaKm2: number;
    maximumDepthM: number;
    depthP95M: number;
    maximumVelocityMs: number;
    earliestArrivalMinutes: number | null;
    releasedVolumeHm3: number;
    massBalanceErrorPct: number;
    peakDischargeM3s: number;
    simulationDurationHours: number;
    cellWidthM: number;
    cellHeightM: number;
  };
  sources: Array<{ label: string; url: string; supports: string }>;
  limitations: string[];
}

interface DamageProperties {
  label?: string;
  severity?: number;
  color?: string;
  startKm?: number;
  endKm?: number;
  areaKm2?: number;
  meanDepthM?: number;
  maxDepthM?: number;
  maxVelocityMs?: number;
  earliestArrivalMin?: number;
  kind?: string;
  lengthKm?: number;
}

const BAND_COLORS = [
  "#246b52",
  "#4f8a55",
  "#86a85b",
  "#c3bd67",
  "#d5a85d",
  "#b8794c",
  "#775247",
];

document.querySelector<HTMLDivElement>("#app")!.innerHTML = `
  <div class="app-shell">
    <aside class="sidebar" aria-label="Controles do mapa">
      <header class="product-header">
        <p class="eyebrow">Laboratório geoespacial</p>
        <h1>Itupararanga</h1>
        <p>Relevo de Sorocaba e cenário preliminar de ruptura</p>
      </header>

      <div class="notice" role="note">
        <strong>Estudo exploratório</strong>
        <span>Não substitui o PAE da barragem nem uma modelagem hidrodinâmica validada.</span>
      </div>

      <section class="control-section integration-section" aria-labelledby="integration-title">
        <div class="section-heading">
          <div>
            <p class="section-kicker">Aplicativo</p>
            <h2 id="integration-title">Agente e processamento</h2>
          </div>
          <span id="openrouter-status" class="status-badge is-neutral">Verificando</span>
        </div>
        <div class="runtime-summary">
          <div>
            <span>Modelo</span>
            <strong>GLM-5.3-Flash</strong>
          </div>
          <div>
            <span>Processamento</span>
            <strong id="compute-backend">Detectando…</strong>
          </div>
        </div>
        <details class="integration-details">
          <summary>Configurar OpenRouter e pesquisar cenário</summary>
          <div class="integration-body">
            <label class="field-label" for="compute-preference">Modo de processamento</label>
            <select id="compute-preference">
              <option value="auto">Automático — priorizar GPU</option>
              <option value="gpu">GPU CUDA</option>
              <option value="cpu">CPU</option>
            </select>
            <p id="compute-diagnostic" class="field-note">Verificando o runtime CUDA…</p>
            <label class="field-label" for="openrouter-key">Chave API</label>
            <div class="key-field">
              <input id="openrouter-key" type="password" autocomplete="off" placeholder="sk-or-v1-…" spellcheck="false" />
              <button id="save-openrouter-key" class="primary-button" type="button">Salvar</button>
            </div>
            <p id="credential-note" class="field-note">A chave fica no cofre de credenciais do Windows.</p>
            <div class="inline-actions">
              <button id="test-openrouter" class="secondary-button" type="button" disabled>Testar conexão</button>
              <button id="remove-openrouter-key" class="text-button danger-text" type="button" disabled>Remover chave</button>
            </div>
            <div id="integration-feedback" class="form-feedback" role="status" aria-live="polite"></div>

            <div class="research-form" aria-labelledby="research-title">
              <h3 id="research-title">Pesquisa de insumos</h3>
              <label class="field-label" for="research-dam">Barragem</label>
              <input id="research-dam" type="text" value="Itupararanga" />
              <label class="field-label" for="research-city">Município</label>
              <input id="research-city" type="text" value="Sorocaba, SP" />
              <label class="field-label" for="grid-profile">Grade de cálculo</label>
              <select id="grid-profile">
                <option value="fast">Rápida — menor detalhe</option>
                <option value="detailed" selected>Detalhada — bairros</option>
                <option value="fine">Fina — mais lenta</option>
              </select>
              <button id="run-research" class="primary-button full-button" type="button" disabled>Pesquisar dados com o agente</button>
            </div>
            <div id="research-result" class="research-result" aria-live="polite"></div>
          </div>
        </details>
      </section>

      <section class="control-section" aria-labelledby="terrain-title">
        <div class="section-heading">
          <div>
            <p class="section-kicker">Camada 01</p>
            <h2 id="terrain-title">Cores do relevo</h2>
          </div>
          <button class="text-button" id="reset-palette" type="button">Restaurar</button>
        </div>
        <p class="section-description">
          Arraste os limites e escolha uma cor para cada faixa. O mapa responde em tempo real.
        </p>
        <div id="elevation-bands" class="elevation-bands"></div>
      </section>

      <section class="control-section" aria-labelledby="layers-title">
        <div class="section-heading">
          <div>
            <p class="section-kicker">Camada 02</p>
            <h2 id="layers-title">Elementos visíveis</h2>
          </div>
        </div>
        <label class="toggle-row toggle-row-primary">
          <input id="toggle-terrain" type="checkbox" checked />
          <span>Relevo colorido</span>
        </label>
        <label class="toggle-row">
          <input id="toggle-hydrography" type="checkbox" checked />
          <span>Rios e reservatório</span>
        </label>
        <label class="toggle-row">
          <input id="toggle-roads" type="checkbox" checked />
          <span>Ruas e rodovias</span>
        </label>
        <label class="toggle-row">
          <input id="toggle-boundary" type="checkbox" checked />
          <span>Limite de Sorocaba</span>
        </label>
        <label class="toggle-row">
          <input id="toggle-damage" type="checkbox" checked />
          <span>Áreas de risco 2D</span>
        </label>
        <label class="toggle-row toggle-row-option">
          <input id="toggle-smoothing" type="checkbox" checked />
          <span>Suavizar contornos</span>
        </label>
        <p id="risk-layer-note" class="layer-note">A mancha usa propagação 2D, profundidade e velocidade. A suavização afeta somente o contorno exibido.</p>
      </section>

      <section class="control-section" aria-labelledby="scenario-title">
        <div class="section-heading">
          <div>
            <p class="section-kicker">Cenário 01</p>
            <h2 id="scenario-title">Volume mobilizado</h2>
          </div>
        </div>
        <p class="section-description">
          Represa na capacidade máxima cadastrada. A fração mobilizada pode variar entre o volume útil e o limite superior total.
        </p>
        <label class="scenario-slider" for="release-fraction">
          <span>Fração liberada</span>
          <output id="release-percent">82%</output>
        </label>
        <input
          id="release-fraction"
          type="range"
          min="${DEFAULT_RELEASE_FRACTION * 100}"
          max="100"
          value="${DEFAULT_RELEASE_FRACTION * 100}"
          step="0.01"
        />
        <p class="simulation-note">A mancha 2D corresponde ao cenário-base de 247,57 hm³. O ajuste acima compara volumes, mas não recalcula a propagação.</p>
        <dl class="metrics">
          <div>
            <dt>Armazenado</dt>
            <dd>${formatHm3(TOTAL_CAPACITY_HM3)} hm³</dd>
          </div>
          <div>
            <dt>Liberado na barragem</dt>
            <dd id="released-volume">—</dd>
          </div>
          <div>
            <dt>Residual estimado</dt>
            <dd id="residual-volume">—</dd>
          </div>
          <div>
            <dt>Área indicativa</dt>
            <dd id="flooded-area">—</dd>
          </div>
          <div>
            <dt>Profundidade P95</dt>
            <dd id="maximum-depth">—</dd>
          </div>
        </dl>
      </section>

      <section class="control-section methodology" aria-labelledby="method-title">
        <details>
          <summary id="method-title">Hipóteses e fontes</summary>
          <div id="method-content"></div>
        </details>
      </section>
    </aside>

    <main class="map-region">
      <div id="map" aria-label="Mapa de relevo, hidrografia e áreas de risco hidrodinâmico"></div>
      <div class="export-toolbar" aria-label="Exportar mapa de risco">
        <label for="export-format">Exportar</label>
        <select id="export-format" aria-label="Formato de exportação">
          <option value="png">PNG</option>
          <option value="jpeg">JPEG</option>
          <option value="pdf">PDF</option>
        </select>
        <button id="export-map" type="button">Baixar</button>
      </div>
      <div class="map-key" aria-label="Legenda das zonas de dano">
        <span class="key-title">Risco 2D experimental</span>
        <span><i style="--key-color:#b42318"></i>Extremo</span>
        <span><i style="--key-color:#d93f20"></i>Muito alto</span>
        <span><i style="--key-color:#ef6c23"></i>Alto</span>
        <span><i style="--key-color:#f29f24"></i>Moderado</span>
        <span><i style="--key-color:#f4c542"></i>Menor</span>
      </div>
      <div id="map-status" class="map-status" role="status">Preparando o mapa…</div>
    </main>
  </div>
`;

void initializeAppServices();
void initializeMap();

async function initializeMap(): Promise<void> {
  const [
    metadata,
    terrainTexture,
    hydrography,
    roads,
    municipality,
    damageZones,
    rawDamageZones,
    dam,
    scenario,
  ] = await Promise.all([
      fetchJson<TerrainMetadata>("/data/terrain-metadata.json"),
      loadTerrainTexture("/data/terrain.png"),
      fetchJson<GeoJSON.FeatureCollection>("/data/hydrography.geojson"),
      fetchJson<GeoJSON.FeatureCollection>("/data/roads.geojson"),
      fetchJson<GeoJSON.FeatureCollection>("/data/sorocaba-boundary.geojson"),
      fetchJson<GeoJSON.FeatureCollection<GeoJSON.Geometry, DamageProperties>>(
        "/data/damage-zones.geojson",
      ),
      fetchJson<GeoJSON.FeatureCollection<GeoJSON.Geometry, DamageProperties>>(
        "/data/damage-zones-raw.geojson",
      ),
      fetchJson<GeoJSON.FeatureCollection>("/data/dam.geojson"),
      fetchJson<ScenarioData>("/data/scenario.json"),
  ]);

  const defaultBands = createDefaultBands(metadata);
  renderBandControls(metadata, defaultBands);
  renderMethodology(scenario, metadata);
  updateVolumeMetrics(DEFAULT_RELEASE_FRACTION);
  document.querySelector<HTMLElement>("#flooded-area")!.textContent =
    `${scenario.hydrodynamicSimulation.floodedAreaKm2.toLocaleString("pt-BR")} km²`;
  document.querySelector<HTMLElement>("#maximum-depth")!.textContent =
    `${scenario.hydrodynamicSimulation.depthP95M.toLocaleString("pt-BR")} m`;
  document.querySelector<HTMLElement>("#risk-layer-note")!.textContent =
    `Grade de ${scenario.hydrodynamicSimulation.cellWidthM.toLocaleString("pt-BR")} × ${scenario.hydrodynamicSimulation.cellHeightM.toLocaleString("pt-BR")} m. A suavização afeta somente o contorno exibido.`;

  const map = L.map("map", {
    zoomControl: false,
    preferCanvas: true,
    minZoom: 8,
    maxZoom: 16,
  });
  map.createPane("terrainPane").style.zIndex = "250";
  map.createPane("damagePane").style.zIndex = "410";
  map.createPane("hydroPane").style.zIndex = "430";
  map.createPane("roadsPane").style.zIndex = "440";
  map.createPane("boundaryPane").style.zIndex = "445";
  map.createPane("damPane").style.zIndex = "450";

  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19,
    opacity: 0.38,
    attribution: "© OpenStreetMap contributors",
    crossOrigin: "anonymous",
  }).addTo(map);

  const terrainLayer = new TerrainLayer(metadata, terrainTexture, defaultBands).addTo(
    map,
  );
  const damageLayer = createDamageLayer(damageZones).addTo(map);
  const rawDamageLayer = createDamageLayer(rawDamageZones);
  const hydroLayer = createHydrographyLayer(hydrography).addTo(map);
  const roadsLayer = createRoadsLayer(roads).addTo(map);
  const boundaryLayer = createBoundaryLayer(municipality).addTo(map);
  const damLayer = createDamLayer(dam).addTo(map);

  L.control.zoom({ position: "topright" }).addTo(map);
  L.control.scale({ imperial: false, position: "bottomright" }).addTo(map);
  map.fitBounds(
    [
      [-23.7, -47.53],
      [-23.36, -47.18],
    ],
    { padding: [18, 18] },
  );

  bindPaletteControls(metadata, defaultBands, terrainLayer);
  bindLayerToggle("toggle-terrain", terrainLayer, map);
  bindLayerToggle("toggle-hydrography", hydroLayer, map);
  bindLayerToggle("toggle-roads", roadsLayer, map);
  bindLayerToggle("toggle-boundary", boundaryLayer, map);
  bindDamageControls(damageLayer, rawDamageLayer, map);
  bindReleaseControl();
  bindExportControl();

  damLayer.bringToFront();
  const status = document.querySelector<HTMLDivElement>("#map-status")!;
  status.textContent = "Mapa pronto";
  window.setTimeout(() => status.classList.add("is-hidden"), 1_600);
}

function bindExportControl(): void {
  const button = document.querySelector<HTMLButtonElement>("#export-map")!;
  const select = document.querySelector<HTMLSelectElement>("#export-format")!;
  const status = document.querySelector<HTMLDivElement>("#map-status")!;
  button.addEventListener("click", async () => {
    const original = button.textContent ?? "Baixar";
    button.disabled = true;
    button.textContent = "Gerando…";
    status.textContent = "Gerando arquivo…";
    status.classList.remove("is-hidden");
    try {
      await exportMap(
        document.querySelector<HTMLElement>(".map-region")!,
        select.value as ExportFormat,
      );
      status.textContent = `${select.options[select.selectedIndex].text} gerado`;
    } catch (error) {
      status.textContent = errorMessage(error);
    } finally {
      button.disabled = false;
      button.textContent = original;
      window.setTimeout(() => status.classList.add("is-hidden"), 2_000);
    }
  });
}

async function initializeAppServices(): Promise<void> {
  bindIntegrationControls();
  const feedback = document.querySelector<HTMLDivElement>("#integration-feedback")!;
  try {
    const [settings, capabilities] = await Promise.all([
      appApi.getSettings(),
      appApi.getCapabilities(),
    ]);
    renderOpenRouterSettings(settings);
    renderComputeCapabilities(capabilities);
  } catch (error) {
    renderServiceError(error);
    feedback.textContent = "Inicie o backend local para configurar o agente e detectar a GPU.";
  }
}

function bindIntegrationControls(): void {
  const keyInput = document.querySelector<HTMLInputElement>("#openrouter-key")!;
  const saveButton = document.querySelector<HTMLButtonElement>("#save-openrouter-key")!;
  const testButton = document.querySelector<HTMLButtonElement>("#test-openrouter")!;
  const removeButton = document.querySelector<HTMLButtonElement>("#remove-openrouter-key")!;
  const researchButton = document.querySelector<HTMLButtonElement>("#run-research")!;
  const computeSelect = document.querySelector<HTMLSelectElement>("#compute-preference")!;
  const feedback = document.querySelector<HTMLDivElement>("#integration-feedback")!;

  computeSelect.addEventListener("change", async () => {
    const previous = computeSelect.dataset.savedValue ?? "auto";
    computeSelect.disabled = true;
    try {
      const capabilities = await appApi.saveComputePreference(
        computeSelect.value as "auto" | "gpu" | "cpu",
      );
      renderComputeCapabilities(capabilities);
      setFeedback(
        capabilities.selected === "gpu"
          ? "GPU CUDA selecionada e validada."
          : "Processamento por CPU selecionado.",
      );
    } catch (error) {
      computeSelect.value = previous;
      setFeedback(errorMessage(error), true);
    } finally {
      computeSelect.disabled = false;
    }
  });

  saveButton.addEventListener("click", async () => {
    const apiKey = keyInput.value.trim();
    if (!apiKey) {
      setFeedback("Informe a chave OpenRouter.", true);
      keyInput.focus();
      return;
    }
    await runBusy(saveButton, "Salvando…", async () => {
      try {
        await appApi.saveKey(apiKey);
        keyInput.value = "";
        renderOpenRouterSettings(await appApi.getSettings());
        setFeedback("Chave salva com segurança.");
      } catch (error) {
        setFeedback(errorMessage(error), true);
      }
    });
  });

  testButton.addEventListener("click", async () => {
    await runBusy(testButton, "Testando…", async () => {
      try {
        await appApi.testConnection();
        setFeedback("Conexão confirmada com o GLM-5.3-Flash.");
      } catch (error) {
        setFeedback(errorMessage(error), true);
      }
    });
  });

  removeButton.addEventListener("click", async () => {
    await runBusy(removeButton, "Removendo…", async () => {
      try {
        await appApi.removeKey();
        renderOpenRouterSettings(await appApi.getSettings());
        setFeedback("Chave removida do cofre de credenciais.");
      } catch (error) {
        setFeedback(errorMessage(error), true);
      }
    });
  });

  researchButton.addEventListener("click", async () => {
    const damName = document.querySelector<HTMLInputElement>("#research-dam")!.value.trim();
    const cityName = document.querySelector<HTMLInputElement>("#research-city")!.value.trim();
    const profile = document.querySelector<HTMLSelectElement>("#grid-profile")!.value;
    if (!damName || !cityName) {
      setFeedback("Informe a barragem e o município.", true);
      return;
    }
    await runBusy(researchButton, "Pesquisando…", async () => {
      try {
        const result = await appApi.research(damName, cityName, profile);
        renderResearchResult(result);
        setFeedback("Pesquisa concluída. Revise as fontes antes da simulação.");
      } catch (error) {
        setFeedback(errorMessage(error), true);
      }
    });
  });

  function setFeedback(message: string, isError = false): void {
    feedback.textContent = message;
    feedback.classList.toggle("is-error", isError);
  }
}

function renderComputeCapabilities(capabilities: ComputeCapabilities): void {
  const compute = document.querySelector<HTMLElement>("#compute-backend")!;
  const select = document.querySelector<HTMLSelectElement>("#compute-preference")!;
  const diagnostic = document.querySelector<HTMLElement>("#compute-diagnostic")!;
  compute.textContent = `${capabilities.backend} · ${capabilities.device}`;
  compute.title = capabilities.policy;
  select.value = capabilities.preference;
  select.dataset.savedValue = capabilities.preference;
  diagnostic.textContent = capabilities.gpuAvailable
    ? `CUDA ${capabilities.cudaRuntimeVersion ?? "detectada"} · CuPy ${capabilities.cupyVersion ?? "instalado"} · ${capabilities.gpuDevice ?? "GPU disponível"}`
    : capabilities.diagnostic
      ? `CUDA não inicializada: ${capabilities.diagnostic}`
      : "CUDA não pôde ser inicializada. Automático usa CPU; GPU exibe o erro de diagnóstico.";
}

function renderOpenRouterSettings(settings: OpenRouterSettings): void {
  const status = document.querySelector<HTMLElement>("#openrouter-status")!;
  const test = document.querySelector<HTMLButtonElement>("#test-openrouter")!;
  const remove = document.querySelector<HTMLButtonElement>("#remove-openrouter-key")!;
  const research = document.querySelector<HTMLButtonElement>("#run-research")!;
  const note = document.querySelector<HTMLElement>("#credential-note")!;
  status.textContent = settings.configured ? "Configurado" : "Sem chave";
  status.className = `status-badge ${settings.configured ? "is-ready" : "is-neutral"}`;
  test.disabled = !settings.configured;
  remove.disabled = !settings.configured;
  research.disabled = !settings.configured;
  note.textContent = settings.configured
    ? `${settings.maskedKey} · ${settings.storage}`
    : `A chave fica no ${settings.storage.toLocaleLowerCase("pt-BR")}.`;
}

function renderServiceError(error: unknown): void {
  const status = document.querySelector<HTMLElement>("#openrouter-status")!;
  status.textContent = "Backend offline";
  status.className = "status-badge is-error";
  document.querySelector<HTMLElement>("#compute-backend")!.textContent = "Não detectado";
  status.title = errorMessage(error);
}

function renderResearchResult(result: ResearchResult): void {
  const container = document.querySelector<HTMLDivElement>("#research-result")!;
  container.replaceChildren();
  const summary = document.createElement("p");
  summary.textContent = result.summary;
  container.append(summary);
  if (result.datasets.length) {
    const heading = document.createElement("h4");
    heading.textContent = "Fontes localizadas";
    const list = document.createElement("ul");
    result.datasets.forEach((dataset) => {
      const item = document.createElement("li");
      const link = document.createElement("a");
      link.href = dataset.sourceUrl;
      link.target = "_blank";
      link.rel = "noreferrer";
      link.textContent = `${dataset.kind}: ${dataset.title}`;
      item.append(link);
      list.append(item);
    });
    container.append(heading, list);
  }
  if (result.warnings.length) {
    const warning = document.createElement("p");
    warning.className = "research-warning";
    warning.textContent = result.warnings.join(" ");
    container.append(warning);
  }
}

async function runBusy(
  button: HTMLButtonElement,
  busyLabel: string,
  action: () => Promise<void>,
): Promise<void> {
  const original = button.textContent ?? "";
  button.disabled = true;
  button.textContent = busyLabel;
  try {
    await action();
  } finally {
    button.textContent = original;
    button.disabled = false;
    const settings = await appApi.getSettings().catch(() => null);
    if (settings) renderOpenRouterSettings(settings);
  }
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : "Ocorreu uma falha inesperada.";
}

function createDefaultBands(metadata: TerrainMetadata): ElevationBand[] {
  return [
    ...metadata.suggestedBreaksM.map((upperM, index) => ({
      upperM,
      color: BAND_COLORS[index],
    })),
    { upperM: Math.ceil(metadata.elevationMaxM), color: BAND_COLORS.at(-1)! },
  ];
}

function renderBandControls(
  metadata: TerrainMetadata,
  bands: ElevationBand[],
): void {
  const container = document.querySelector<HTMLDivElement>("#elevation-bands")!;
  container.innerHTML = bands
    .map((band, index) => {
      const isLast = index === bands.length - 1;
      const lower = index === 0 ? metadata.elevationMinM : bands[index - 1].upperM;
      const label = isLast ? `Acima de ${Math.round(lower)} m` : `Até ${band.upperM} m`;
      return `
        <div class="band-row" data-band-index="${index}">
          <input
            class="color-input"
            type="color"
            value="${band.color}"
            aria-label="Cor da faixa ${index + 1}"
            data-color-index="${index}"
          />
          <div class="band-control">
            <div class="band-label">
              <span>Faixa ${String(index + 1).padStart(2, "0")}</span>
              <output data-band-output="${index}">${label}</output>
            </div>
            ${
              isLast
                ? '<div class="range-track range-track-static" aria-hidden="true"></div>'
                : `<input
                    type="range"
                    min="${Math.floor(metadata.elevationMinM / 5) * 5}"
                    max="${Math.ceil(metadata.elevationMaxM / 5) * 5}"
                    value="${band.upperM}"
                    step="5"
                    aria-label="Limite superior da faixa ${index + 1}"
                    data-threshold-index="${index}"
                  />`
            }
          </div>
        </div>
      `;
    })
    .join("");
}

function bindPaletteControls(
  metadata: TerrainMetadata,
  defaultBands: ElevationBand[],
  terrainLayer: TerrainLayer,
): void {
  const container = document.querySelector<HTMLDivElement>("#elevation-bands")!;
  let bands = defaultBands.map((band) => ({ ...band }));
  let frame: number | undefined;

  const apply = () => {
    if (frame !== undefined) window.cancelAnimationFrame(frame);
    frame = window.requestAnimationFrame(() => terrainLayer.setBands(bands));
  };

  container.addEventListener("input", (event) => {
    const target = event.target;
    if (!(target instanceof HTMLInputElement)) return;

    const colorIndex = target.dataset.colorIndex;
    if (colorIndex !== undefined) {
      bands[Number(colorIndex)].color = target.value;
      apply();
      return;
    }

    const thresholdIndex = target.dataset.thresholdIndex;
    if (thresholdIndex === undefined) return;
    const index = Number(thresholdIndex);
    const previous = index === 0 ? metadata.elevationMinM : bands[index - 1].upperM;
    const next = bands[index + 1].upperM;
    const constrained = Math.min(next - 5, Math.max(previous + 5, Number(target.value)));
    target.value = String(constrained);
    bands[index].upperM = constrained;
    refreshBandLabels(metadata, bands);
    apply();
  });

  document.querySelector<HTMLButtonElement>("#reset-palette")!.addEventListener(
    "click",
    () => {
      bands = defaultBands.map((band) => ({ ...band }));
      document
        .querySelectorAll<HTMLInputElement>("[data-color-index]")
        .forEach((input) => {
          const index = Number(input.dataset.colorIndex);
          input.value = bands[index].color;
        });
      document
        .querySelectorAll<HTMLInputElement>("[data-threshold-index]")
        .forEach((input) => {
          const index = Number(input.dataset.thresholdIndex);
          input.value = String(bands[index].upperM);
        });
      refreshBandLabels(metadata, bands);
      terrainLayer.setBands(bands);
    },
  );
}

function refreshBandLabels(metadata: TerrainMetadata, bands: ElevationBand[]): void {
  bands.forEach((band, index) => {
    const output = document.querySelector<HTMLOutputElement>(
      `[data-band-output="${index}"]`,
    );
    if (!output) return;
    const previous = index === 0 ? metadata.elevationMinM : bands[index - 1].upperM;
    output.textContent =
      index === bands.length - 1
        ? `Acima de ${Math.round(previous)} m`
        : `Até ${Math.round(band.upperM)} m`;
  });
}

function createHydrographyLayer(
  collection: GeoJSON.FeatureCollection,
): LeafletGeoJSON {
  return L.geoJSON(collection, {
    pane: "hydroPane",
    style: (feature) => {
      const properties = feature?.properties ?? {};
      const isReservoir = properties.kind === "reservoir";
      const isRiver = properties.waterway === "river";
      const isSorocaba = String(properties.name ?? "")
        .toLocaleLowerCase("pt-BR")
        .includes("sorocaba");
      return {
        color: isReservoir ? "#0878be" : "#086dad",
        weight: isReservoir ? 1.2 : isSorocaba ? 2.8 : isRiver ? 1.8 : 0.7,
        opacity: isReservoir || isSorocaba ? 0.98 : isRiver ? 0.86 : 0.48,
        fillColor: "#178bd0",
        fillOpacity: isReservoir ? 0.9 : 0,
        interactive: true,
      };
    },
    onEachFeature: (feature, layer) => {
      const name = feature.properties?.name ?? "Curso d'água";
      layer.bindTooltip(String(name), { sticky: true });
    },
  });
}

function createDamageLayer(
  collection: GeoJSON.FeatureCollection<GeoJSON.Geometry, DamageProperties>,
): LeafletGeoJSON {
  return L.geoJSON(collection, {
    pane: "damagePane",
    style: (feature) => {
      if (feature?.properties.kind === "route") {
        return {
          color: "#6d281b",
          weight: 1.5,
          opacity: 0.75,
          dashArray: "5 5",
          fillOpacity: 0,
        };
      }
      return {
        color: feature?.properties.color ?? "#d93f20",
        weight: 1,
        opacity: 0.86,
        fillColor: feature?.properties.color ?? "#d93f20",
        fillOpacity: 0.42,
      };
    },
    onEachFeature: (feature, layer) => {
      const properties = feature.properties;
      if (properties.kind === "route") {
        layer.bindTooltip(
          `Eixo preliminar · ${properties.lengthKm?.toLocaleString("pt-BR")} km`,
          { sticky: true },
        );
        return;
      }
      layer.bindTooltip(
        `<strong>${properties.label}</strong><br>Profundidade média: ${properties.meanDepthM?.toLocaleString("pt-BR")} m<br>Profundidade máxima: ${properties.maxDepthM?.toLocaleString("pt-BR")} m<br>Velocidade máxima: ${properties.maxVelocityMs?.toLocaleString("pt-BR")} m/s<br>Primeira chegada: ${properties.earliestArrivalMin?.toLocaleString("pt-BR")} min<br>Área da classe: ${properties.areaKm2?.toLocaleString("pt-BR")} km²`,
        { sticky: true },
      );
    },
  });
}

function createRoadsLayer(collection: GeoJSON.FeatureCollection): LeafletGeoJSON {
  const majorRoads = new Set(["motorway", "trunk", "primary", "secondary"]);
  const collectorRoads = new Set(["tertiary", "unclassified"]);

  return L.geoJSON(collection, {
    pane: "roadsPane",
    interactive: false,
    style: (feature) => {
      const roadClass = String(feature?.properties?.highway ?? "residential");
      const isMajor = majorRoads.has(roadClass);
      const isCollector = collectorRoads.has(roadClass);
      return {
        color: isMajor ? "#303b42" : "#59646a",
        weight: isMajor ? 1.45 : isCollector ? 0.9 : 0.58,
        opacity: isMajor ? 0.82 : isCollector ? 0.64 : 0.45,
        lineCap: "round",
        lineJoin: "round",
      };
    },
  });
}

function createBoundaryLayer(
  collection: GeoJSON.FeatureCollection,
): LeafletGeoJSON {
  return L.geoJSON(collection, {
    pane: "boundaryPane",
    interactive: false,
    style: {
      color: "#553080",
      weight: 2.4,
      opacity: 0.98,
      dashArray: "8 5",
      fill: false,
    },
  });
}

function createDamLayer(collection: GeoJSON.FeatureCollection): LeafletGeoJSON {
  return L.geoJSON(collection, {
    pane: "damPane",
    pointToLayer: (_feature, latlng) =>
      L.circleMarker(latlng, {
        radius: 7,
        color: "#ffffff",
        weight: 2,
        fillColor: "#172b3a",
        fillOpacity: 1,
      }),
    onEachFeature: (feature, layer) => {
      layer.bindTooltip(
        `<strong>${feature.properties?.name}</strong><br>35 m de altura · 415 m de extensão`,
        { direction: "top", offset: [0, -6] },
      );
    },
  });
}

function bindLayerToggle(
  id: string,
  layer: L.Layer,
  map: L.Map,
): void {
  document.querySelector<HTMLInputElement>(`#${id}`)!.addEventListener("change", (event) => {
    const input = event.currentTarget as HTMLInputElement;
    if (input.checked) layer.addTo(map);
    else layer.removeFrom(map);
  });
}

function bindDamageControls(
  smoothedLayer: L.Layer,
  rawLayer: L.Layer,
  map: L.Map,
): void {
  const visibility = document.querySelector<HTMLInputElement>("#toggle-damage")!;
  const smoothing = document.querySelector<HTMLInputElement>("#toggle-smoothing")!;
  const syncLayers = (): void => {
    smoothedLayer.removeFrom(map);
    rawLayer.removeFrom(map);
    smoothing.disabled = !visibility.checked;
    if (visibility.checked) {
      (smoothing.checked ? smoothedLayer : rawLayer).addTo(map);
    }
  };
  visibility.addEventListener("change", syncLayers);
  smoothing.addEventListener("change", syncLayers);
}

function bindReleaseControl(): void {
  const input = document.querySelector<HTMLInputElement>("#release-fraction")!;
  input.addEventListener("input", () => updateVolumeMetrics(Number(input.value) / 100));
}

function updateVolumeMetrics(releaseFraction: number): void {
  const estimate = estimateReleasedVolume({
    storedVolumeHm3: TOTAL_CAPACITY_HM3,
    releaseFraction,
    downstreamRetentionFraction: 0,
  });
  document.querySelector<HTMLOutputElement>("#release-percent")!.textContent =
    `${new Intl.NumberFormat("pt-BR", { maximumFractionDigits: 1 }).format(releaseFraction * 100)}%`;
  document.querySelector<HTMLElement>("#released-volume")!.textContent =
    `${formatHm3(estimate.releasedAtDamHm3)} hm³`;
  document.querySelector<HTMLElement>("#residual-volume")!.textContent =
    `${formatHm3(estimate.residualHm3)} hm³`;
}

function renderMethodology(
  scenario: ScenarioData,
  metadata: TerrainMetadata,
): void {
  const container = document.querySelector<HTMLDivElement>("#method-content")!;
  container.innerHTML = `
    <h3>Hipóteses atuais</h3>
    <ul>${scenario.limitations.map((item) => `<li>${item}</li>`).join("")}</ul>
    <h3>Dados</h3>
    <ul>
      ${scenario.sources
        .map(
          (source) =>
            `<li><a href="${source.url}" target="_blank" rel="noreferrer">${source.label}</a><br><span>${source.supports}</span></li>`,
        )
        .join("")}
      <li><a href="${metadata.sourceUrl}" target="_blank" rel="noreferrer">${metadata.source}</a><br><span>${metadata.resolution}</span></li>
    </ul>
    <p class="source-notice">${metadata.notice}</p>
  `;
}

async function fetchJson<T>(url: string): Promise<T> {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Falha ao carregar ${url}: ${response.status}`);
  }
  return response.json() as Promise<T>;
}
