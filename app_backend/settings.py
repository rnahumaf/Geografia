from __future__ import annotations

import json
import os
from pathlib import Path


VALID_COMPUTE_PREFERENCES = {"auto", "gpu", "cpu"}


class ComputePreferenceStore:
    def __init__(self, path: Path | None = None) -> None:
        local_app_data = Path(os.environ.get("LOCALAPPDATA", Path.home()))
        self.path = path or local_app_data / "Itupararanga Flood Lab" / "settings.json"

    def get(self) -> str:
        try:
            data = json.loads(self.path.read_text(encoding="utf-8"))
        except (FileNotFoundError, OSError, json.JSONDecodeError):
            return "auto"
        preference = data.get("computePreference", "auto")
        return preference if preference in VALID_COMPUTE_PREFERENCES else "auto"

    def set(self, preference: str) -> None:
        if preference not in VALID_COMPUTE_PREFERENCES:
            raise ValueError("Preferência de processamento inválida.")
        self.path.parent.mkdir(parents=True, exist_ok=True)
        temporary = self.path.with_suffix(".tmp")
        temporary.write_text(
            json.dumps({"computePreference": preference}, indent=2),
            encoding="utf-8",
        )
        temporary.replace(self.path)


def load_compute_preference() -> str:
    return ComputePreferenceStore().get()
