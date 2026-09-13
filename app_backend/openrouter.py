from __future__ import annotations

import json
from typing import Any

import requests


OPENROUTER_ENDPOINT = "https://openrouter.ai/api/v1/chat/completions"
OPENROUTER_MODEL = "z-ai/glm-5.3-flash"


RESEARCH_SCHEMA: dict[str, Any] = {
    "name": "flood_scenario_research",
    "strict": True,
    "schema": {
        "type": "object",
        "additionalProperties": False,
        "properties": {
            "summary": {"type": "string"},
            "dam": {
                "type": "object",
                "additionalProperties": False,
                "properties": {
                    "name": {"type": "string"},
                    "latitude": {"type": ["number", "null"]},
                    "longitude": {"type": ["number", "null"]},
                    "totalCapacityHm3": {"type": ["number", "null"]},
                    "usefulVolumeHm3": {"type": ["number", "null"]},
                    "heightM": {"type": ["number", "null"]},
                },
                "required": [
                    "name",
                    "latitude",
                    "longitude",
                    "totalCapacityHm3",
                    "usefulVolumeHm3",
                    "heightM",
                ],
            },
            "municipality": {
                "type": "object",
                "additionalProperties": False,
                "properties": {
                    "name": {"type": "string"},
                    "state": {"type": ["string", "null"]},
                    "country": {"type": "string"},
                    "latitude": {"type": ["number", "null"]},
                    "longitude": {"type": ["number", "null"]},
                },
                "required": ["name", "state", "country", "latitude", "longitude"],
            },
            "datasets": {
                "type": "array",
                "items": {
                    "type": "object",
                    "additionalProperties": False,
                    "properties": {
                        "kind": {
                            "type": "string",
                            "enum": ["elevation", "hydrography", "boundary", "roads", "dam"],
                        },
                        "title": {"type": "string"},
                        "downloadUrl": {"type": ["string", "null"]},
                        "sourceUrl": {"type": "string"},
                        "resolutionM": {"type": ["number", "null"]},
                        "license": {"type": ["string", "null"]},
                    },
                    "required": [
                        "kind",
                        "title",
                        "downloadUrl",
                        "sourceUrl",
                        "resolutionM",
                        "license",
                    ],
                },
            },
            "assumptions": {"type": "array", "items": {"type": "string"}},
            "warnings": {"type": "array", "items": {"type": "string"}},
        },
        "required": ["summary", "dam", "municipality", "datasets", "assumptions", "warnings"],
    },
}


class OpenRouterError(RuntimeError):
    pass


class OpenRouterClient:
    def __init__(self, api_key: str, timeout_seconds: float = 90) -> None:
        self.api_key = api_key
        self.timeout_seconds = timeout_seconds

    def _post(self, payload: dict[str, Any]) -> dict[str, Any]:
        try:
            response = requests.post(
                OPENROUTER_ENDPOINT,
                headers={
                    "Authorization": f"Bearer {self.api_key}",
                    "Content-Type": "application/json",
                    "HTTP-Referer": "http://127.0.0.1",
                    "X-Title": "Itupararanga Flood Lab",
                },
                json=payload,
                timeout=self.timeout_seconds,
            )
        except requests.RequestException as error:
            raise OpenRouterError("Não foi possível conectar ao OpenRouter.") from error
        if response.status_code >= 400:
            try:
                detail = response.json().get("error", {}).get("message")
            except (ValueError, AttributeError):
                detail = None
            suffix = f" {detail}" if detail else ""
            raise OpenRouterError(
                f"OpenRouter recusou a solicitação ({response.status_code}).{suffix}"
            )
        try:
            return response.json()
        except ValueError as error:
            raise OpenRouterError("O OpenRouter retornou uma resposta inválida.") from error

    def test_connection(self) -> None:
        data = self._post(
            {
                "model": OPENROUTER_MODEL,
                "messages": [
                    {
                        "role": "user",
                        "content": "Responda somente com a palavra OK.",
                    }
                ],
                "temperature": 0,
                "max_tokens": 8,
                "provider": {"require_parameters": True, "data_collection": "deny"},
            }
        )
        if not data.get("choices"):
            raise OpenRouterError("O modelo não retornou uma resposta de teste.")

    def research_scenario(
        self, dam_name: str, municipality_name: str, grid_profile: str
    ) -> dict[str, Any]:
        prompt = f"""Pesquise dados para uma simulação exploratória de ruptura de barragem.
Barragem: {dam_name}
Município potencialmente afetado: {municipality_name}
Perfil de grade solicitado: {grid_profile}

Localize fontes oficiais ou técnicas para coordenadas da barragem, capacidade total e útil,
altura, modelo digital de elevação, hidrografia, limites municipais e malha viária. Forneça
links diretos de download quando a fonte permitir. Não invente números nem URLs: use null
quando um dado não puder ser confirmado e registre a incerteza em warnings. Esta pesquisa
apenas prepara entradas; não declare segurança de barragens nem previsão oficial de danos."""
        data = self._post(
            {
                "model": OPENROUTER_MODEL,
                "messages": [
                    {
                        "role": "system",
                        "content": (
                            "Você pesquisa insumos geoespaciais com rastreabilidade. "
                            "Prefira ANA, SNISB, IBGE, INPE, Copernicus e órgãos gestores."
                        ),
                    },
                    {"role": "user", "content": prompt},
                ],
                "tools": [
                    {
                        "type": "openrouter:web_search",
                        "web_search": {
                            "max_results": 8,
                            "search_context_size": "high",
                        },
                    }
                ],
                "response_format": {"type": "json_schema", "json_schema": RESEARCH_SCHEMA},
                "temperature": 0,
                "provider": {"require_parameters": True, "data_collection": "deny"},
            }
        )
        try:
            message = data["choices"][0]["message"]
            content = message["content"]
            result = json.loads(content) if isinstance(content, str) else content
        except (KeyError, IndexError, TypeError, json.JSONDecodeError) as error:
            raise OpenRouterError(
                "O agente não devolveu a estrutura de dados esperada."
            ) from error
        if not isinstance(result, dict):
            raise OpenRouterError("O resultado da pesquisa não é um objeto válido.")
        result["model"] = data.get("model", OPENROUTER_MODEL)
        result["gridProfile"] = grid_profile
        result["annotations"] = message.get("annotations", [])
        return result
