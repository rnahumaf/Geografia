export interface OpenRouterSettings {
  configured: boolean;
  maskedKey: string | null;
  model: string;
  modelLabel: string;
  storage: string;
}

export interface ComputeCapabilities {
  selected: "gpu" | "cpu";
  backend: string;
  device: string;
  gpuAvailable: boolean;
  gpuDevice: string | null;
  preference: "auto" | "gpu" | "cpu";
  policy: string;
  cpuLogicalCores: number | null;
  cupyVersion: string | null;
  cudaRuntimeVersion: string | null;
  cudaDriverVersion: string | null;
  diagnostic?: string;
}

export interface ResearchResult {
  summary: string;
  dam: Record<string, unknown>;
  municipality: Record<string, unknown>;
  datasets: Array<{
    kind: string;
    title: string;
    downloadUrl: string | null;
    sourceUrl: string;
    resolutionM: number | null;
    license: string | null;
  }>;
  assumptions: string[];
  warnings: string[];
  model: string;
  gridProfile: string;
}

async function request<T>(url: string, init?: RequestInit): Promise<T> {
  const response = await fetch(url, init);
  if (!response.ok) {
    let detail = `Falha na solicitação (${response.status}).`;
    try {
      const body = (await response.json()) as { detail?: string | Array<{ msg?: string }> };
      if (typeof body.detail === "string") detail = body.detail;
      else if (Array.isArray(body.detail) && body.detail[0]?.msg) detail = body.detail[0].msg;
    } catch {
      // A mensagem HTTP já é suficiente quando não há JSON.
    }
    throw new Error(detail);
  }
  return response.json() as Promise<T>;
}

export const appApi = {
  getSettings: () => request<OpenRouterSettings>("/api/settings/openrouter"),
  getCapabilities: () => request<ComputeCapabilities>("/api/system/capabilities"),
  saveComputePreference: (preference: "auto" | "gpu" | "cpu") =>
    request<ComputeCapabilities>("/api/settings/compute", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ preference }),
    }),
  saveKey: (apiKey: string) =>
    request<{ configured: boolean; maskedKey: string }>("/api/settings/openrouter", {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ apiKey }),
    }),
  removeKey: () =>
    request<{ configured: boolean }>("/api/settings/openrouter", { method: "DELETE" }),
  testConnection: () =>
    request<{ ok: boolean; model: string }>("/api/openrouter/test", { method: "POST" }),
  research: (damName: string, municipalityName: string, gridProfile: string) =>
    request<ResearchResult>("/api/agent/research", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ damName, municipalityName, gridProfile }),
    }),
};
