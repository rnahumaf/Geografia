from __future__ import annotations

import unittest

import numpy as np

from scripts.compute_backend import probe_compute_backend, select_compute_backend


class ComputeBackendTests(unittest.TestCase):
    def test_cpu_can_be_selected_explicitly(self) -> None:
        backend = select_compute_backend("cpu")
        self.assertFalse(backend.is_gpu)
        self.assertIs(backend.xp, np)
        self.assertEqual(backend.to_numpy(np.array([1])).tolist(), [1])

    def test_auto_probe_has_supported_selection(self) -> None:
        capabilities = probe_compute_backend()
        self.assertIn(capabilities["selected"], {"cpu", "gpu"})
        self.assertIn("backend", capabilities)
        self.assertEqual(
            capabilities["gpuAvailable"], capabilities["selected"] == "gpu"
        )


if __name__ == "__main__":
    unittest.main()
