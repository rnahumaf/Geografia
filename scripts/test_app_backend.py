from __future__ import annotations

import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

from fastapi.testclient import TestClient

from app_backend.credentials import MemoryCredentialStore
from app_backend.server import create_app
from app_backend.settings import ComputePreferenceStore


class FakeOpenRouterClient:
    tested_key: str | None = None

    def __init__(self, api_key: str) -> None:
        self.api_key = api_key

    def test_connection(self) -> None:
        FakeOpenRouterClient.tested_key = self.api_key

    def research_scenario(self, dam: str, city: str, profile: str) -> dict:
        return {
            "summary": f"{dam} → {city}",
            "datasets": [],
            "warnings": [],
            "gridProfile": profile,
        }


class AppBackendTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = TemporaryDirectory()
        self.store = MemoryCredentialStore()
        app = create_app(
            credential_store=self.store,
            compute_store=ComputePreferenceStore(
                Path(self.temporary_directory.name) / "settings.json"
            ),
            client_factory=FakeOpenRouterClient,
            serve_frontend=False,
        )
        self.client = TestClient(app)

    def tearDown(self) -> None:
        self.temporary_directory.cleanup()

    def test_key_is_saved_masked_and_persists_between_requests(self) -> None:
        api_key = "sk-or-v1-testfixture1234567890"
        response = self.client.put(
            "/api/settings/openrouter", json={"apiKey": api_key}
        )
        self.assertEqual(response.status_code, 200)
        self.assertEqual(self.store.get(), api_key)

        settings = self.client.get("/api/settings/openrouter").json()
        self.assertTrue(settings["configured"])
        self.assertEqual(settings["maskedKey"], "•••• 7890")
        self.assertNotIn(api_key, str(settings))

        test_response = self.client.post("/api/openrouter/test")
        self.assertEqual(test_response.status_code, 200)
        self.assertEqual(FakeOpenRouterClient.tested_key, api_key)

    def test_invalid_key_is_not_saved(self) -> None:
        response = self.client.put(
            "/api/settings/openrouter", json={"apiKey": "not-an-openrouter-key"}
        )
        self.assertEqual(response.status_code, 422)
        self.assertIsNone(self.store.get())

    def test_research_requires_saved_key(self) -> None:
        response = self.client.post(
            "/api/agent/research",
            json={
                "damName": "Itupararanga",
                "municipalityName": "Sorocaba",
                "gridProfile": "detailed",
            },
        )
        self.assertEqual(response.status_code, 409)

    def test_compute_preference_is_persisted(self) -> None:
        response = self.client.put(
            "/api/settings/compute", json={"preference": "cpu"}
        )
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.json()["preference"], "cpu")
        self.assertEqual(response.json()["selected"], "cpu")
        capabilities = self.client.get("/api/system/capabilities").json()
        self.assertEqual(capabilities["preference"], "cpu")


if __name__ == "__main__":
    unittest.main()
