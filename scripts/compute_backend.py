from __future__ import annotations

import os
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import numpy as np

_original_add_dll_directory = getattr(os, "add_dll_directory", None)
if getattr(sys, "frozen", False) and _original_add_dll_directory is not None:
    # O PyInstaller reúne DLLs CUDA em _internal. O localizador do CUDA 13
    # deriva uma pasta bin vizinha ao executável; redirecionamos somente esse
    # caminho inexistente para o diretório interno efetivamente empacotado.
    def _frozen_add_dll_directory(path: str):
        if Path(path).is_dir():
            return _original_add_dll_directory(path)
        bundled_directory = Path(getattr(sys, "_MEIPASS", Path(sys.executable).parent))
        return _original_add_dll_directory(str(bundled_directory))

    os.add_dll_directory = _frozen_add_dll_directory

try:
    import cupy as _cupy
    _cupy_import_error: ImportError | None = None
except ImportError as error:
    _cupy = None
    _cupy_import_error = error
finally:
    if _original_add_dll_directory is not None:
        os.add_dll_directory = _original_add_dll_directory


@dataclass(frozen=True)
class ComputeBackend:
    xp: Any
    name: str
    device_name: str
    is_gpu: bool

    def to_numpy(self, value: Any) -> np.ndarray:
        if self.is_gpu:
            return self.xp.asnumpy(value)
        return np.asarray(value)

    def scalar(self, value: Any) -> float:
        if hasattr(value, "item"):
            return float(value.item())
        return float(value)


def select_compute_backend(preference: str = "auto") -> ComputeBackend:
    normalized = preference.lower().strip()
    if normalized not in {"auto", "gpu", "cpu"}:
        raise ValueError("O backend deve ser auto, gpu ou cpu.")
    if normalized != "cpu":
        try:
            if _cupy is None:
                raise ImportError(
                    f"CuPy não pôde ser carregado: {_cupy_import_error}"
                )
            cupy = _cupy
            if cupy.cuda.runtime.getDeviceCount() > 0:
                cupy.zeros(1, dtype=cupy.float32).sum().item()
                properties = cupy.cuda.runtime.getDeviceProperties(0)
                raw_name = properties.get("name", b"GPU CUDA")
                device_name = (
                    raw_name.decode("utf-8", errors="replace")
                    if isinstance(raw_name, bytes)
                    else str(raw_name)
                )
                return ComputeBackend(cupy, "CUDA (CuPy)", device_name, True)
        except (ImportError, ModuleNotFoundError, RuntimeError, OSError) as error:
            if normalized == "gpu":
                raise RuntimeError(
                    "GPU CUDA indisponível. "
                    f"Diagnóstico: {type(error).__name__}: {error}"
                )
    return ComputeBackend(np, "CPU (NumPy)", "Processador do sistema", False)


def _cuda_version_label(version: int | None) -> str | None:
    if version is None:
        return None
    major = version // 1000
    minor = (version % 1000) // 10
    return f"{major}.{minor}"


def probe_compute_backend(preference: str = "auto") -> dict[str, str | bool | int | None]:
    try:
        gpu_backend = select_compute_backend("gpu")
        gpu_available = True
        cupy_version = str(gpu_backend.xp.__version__)
        cuda_runtime = _cuda_version_label(gpu_backend.xp.cuda.runtime.runtimeGetVersion())
        cuda_driver = _cuda_version_label(gpu_backend.xp.cuda.runtime.driverGetVersion())
    except RuntimeError:
        gpu_backend = None
        gpu_available = False
        cupy_version = None
        cuda_runtime = None
        cuda_driver = None
    backend = (
        select_compute_backend("cpu")
        if preference == "cpu" or not gpu_available
        else gpu_backend
    )
    assert backend is not None
    cpu_count = None
    try:
        import os

        cpu_count = os.cpu_count()
    except OSError:
        pass
    return {
        "selected": "gpu" if backend.is_gpu else "cpu",
        "backend": backend.name,
        "device": backend.device_name,
        "gpuAvailable": gpu_available,
        "gpuDevice": gpu_backend.device_name if gpu_backend else None,
        "preference": preference,
        "policy": "GPU automática com fallback para CPU",
        "cpuLogicalCores": cpu_count,
        "cupyVersion": cupy_version,
        "cudaRuntimeVersion": cuda_runtime,
        "cudaDriverVersion": cuda_driver,
    }
