from __future__ import annotations

import re
from pathlib import Path
from typing import Callable

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field

from .credentials import CredentialStore, SystemCredentialStore
from .openrouter import OPENROUTER_MODEL, OpenRouterClient, OpenRouterError
from .settings import ComputePreferenceStore


ROOT_DIR = Path(__file__).resolve().parents[1]
DIST_DIR = ROOT_DIR / "dist-web"
KEY_PATTERN = re.compile(r"^sk-or-(?:v1-)?[A-Za-z0-9_-]{12,}$")


class KeyPayload(BaseModel):
    apiKey: str = Field(max_length=512)


class ResearchPayload(BaseModel):
    damName: str = Field(min_length=2, max_length=160)
    municipalityName: str = Field(min_length=2, max_length=160)
    gridProfile: str = Field(pattern=r"^(fast|detailed|fine)$")


class ComputePayload(BaseModel):
    preference: str = Field(pattern=r"^(auto|gpu|cpu)$")


def create_app(
    credential_store: CredentialStore | None = None,
    compute_store: ComputePreferenceStore | None = None,
    client_factory: Callable[[str], OpenRouterClient] = OpenRouterClient,
    serve_frontend: bool = True,
) -> FastAPI:
    store = credential_store or SystemCredentialStore()
    preference_store = compute_store or ComputePreferenceStore()
    app = FastAPI(title="Itupararanga Flood Lab", docs_url=None, redoc_url=None)
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["http://127.0.0.1:4173", "http://localhost:4173"],
        allow_methods=["GET", "PUT", "POST", "DELETE"],
        allow_headers=["Content-Type"],
    )

    def read_key() -> str:
        try:
            key = store.get()
        except RuntimeError as error:
            raise HTTPException(status_code=503, detail=str(error)) from error
        if not key:
            raise HTTPException(
                status_code=409,
                detail="Salve uma chave OpenRouter antes de usar o agente.",
            )
        return key

    @app.get("/api/health")
    def health() -> dict[str, str]:
        return {"status": "ok"}

    @app.get("/api/settings/openrouter")
    def get_openrouter_settings() -> dict[str, str | bool | None]:
        try:
            key = store.get()
        except RuntimeError as error:
            raise HTTPException(status_code=503, detail=str(error)) from error
        return {
            "configured": bool(key),
            "maskedKey": f"•••• {key[-4:]}" if key else None,
            "model": OPENROUTER_MODEL,
            "modelLabel": "GLM-5.3-Flash",
            "storage": "Cofre de credenciais do Windows",
        }

    @app.put("/api/settings/openrouter")
    def save_openrouter_settings(payload: KeyPayload) -> dict[str, str | bool]:
        key = payload.apiKey.strip()
        if not KEY_PATTERN.fullmatch(key):
            raise HTTPException(
                status_code=422,
                detail="A chave deve ser uma chave OpenRouter no formato sk-or-…",
            )
        try:
            store.set(key)
        except RuntimeError as error:
            raise HTTPException(status_code=503, detail=str(error)) from error
        return {"configured": True, "maskedKey": f"•••• {key[-4:]}"}

    @app.delete("/api/settings/openrouter")
    def delete_openrouter_settings() -> dict[str, bool]:
        try:
            store.delete()
        except RuntimeError as error:
            raise HTTPException(status_code=503, detail=str(error)) from error
        return {"configured": False}

    @app.post("/api/openrouter/test")
    def test_openrouter() -> dict[str, str | bool]:
        try:
            client_factory(read_key()).test_connection()
        except OpenRouterError as error:
            raise HTTPException(status_code=502, detail=str(error)) from error
        return {"ok": True, "model": OPENROUTER_MODEL}

    @app.post("/api/agent/research")
    def research(payload: ResearchPayload) -> dict:
        try:
            return client_factory(read_key()).research_scenario(
                payload.damName.strip(),
                payload.municipalityName.strip(),
                payload.gridProfile,
            )
        except OpenRouterError as error:
            raise HTTPException(status_code=502, detail=str(error)) from error

    @app.get("/api/system/capabilities")
    def system_capabilities() -> dict[str, str | bool | int | None]:
        try:
            from scripts.compute_backend import probe_compute_backend

            return probe_compute_backend(preference_store.get())
        except Exception as error:
            return {
                "selected": "cpu",
                "backend": "CPU (NumPy)",
                "device": "Processador do sistema",
                "gpuAvailable": False,
                "gpuDevice": None,
                "preference": preference_store.get(),
                "policy": "GPU automática com fallback para CPU",
                "cpuLogicalCores": None,
                "cupyVersion": None,
                "cudaRuntimeVersion": None,
                "cudaDriverVersion": None,
                "diagnostic": f"{type(error).__name__}: {error}",
            }

    @app.put("/api/settings/compute")
    def save_compute_settings(payload: ComputePayload) -> dict[str, str | bool | int | None]:
        from scripts.compute_backend import probe_compute_backend, select_compute_backend

        if payload.preference == "gpu":
            try:
                select_compute_backend("gpu")
            except RuntimeError as error:
                raise HTTPException(status_code=409, detail=str(error)) from error
        preference_store.set(payload.preference)
        return probe_compute_backend(payload.preference)

    if serve_frontend and DIST_DIR.exists():
        assets_dir = DIST_DIR / "assets"
        if assets_dir.exists():
            app.mount("/assets", StaticFiles(directory=assets_dir), name="assets")
        data_dir = DIST_DIR / "data"
        if data_dir.exists():
            app.mount("/data", StaticFiles(directory=data_dir), name="data")

        @app.get("/{path:path}", include_in_schema=False)
        def frontend(path: str) -> FileResponse:
            candidate = (DIST_DIR / path).resolve()
            if path and candidate.is_file() and DIST_DIR.resolve() in candidate.parents:
                return FileResponse(candidate)
            return FileResponse(DIST_DIR / "index.html")

    return app


app = create_app()
