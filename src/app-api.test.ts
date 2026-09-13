import { afterEach, describe, expect, it, vi } from "vitest";

import { appApi } from "./app-api";

describe("appApi", () => {
  afterEach(() => vi.unstubAllGlobals());

  it("envia a chave apenas no corpo da gravação", async () => {
    const fetchMock = vi.fn().mockResolvedValue({
      ok: true,
      json: async () => ({ configured: true, maskedKey: "•••• 1234" }),
    });
    vi.stubGlobal("fetch", fetchMock);

    await appApi.saveKey("sk-or-v1-fixture1234");

    expect(fetchMock).toHaveBeenCalledWith(
      "/api/settings/openrouter",
      expect.objectContaining({
        method: "PUT",
        body: JSON.stringify({ apiKey: "sk-or-v1-fixture1234" }),
      }),
    );
  });

  it("propaga a mensagem segura devolvida pela API", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn().mockResolvedValue({
        ok: false,
        status: 422,
        json: async () => ({ detail: "Chave inválida." }),
      }),
    );

    await expect(appApi.saveKey("x")).rejects.toThrow("Chave inválida.");
  });
});
