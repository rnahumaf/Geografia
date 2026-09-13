import unittest

import numpy as np

from hydrodynamic import _hydrograph_discharge, _limit_outflow


class HydrodynamicTests(unittest.TestCase):
    def test_triangular_hydrograph_preserves_requested_volume(self) -> None:
        volume_m3 = 247.57e6
        peak_m3s = 15_000.0
        time_to_peak_s = 3_600.0
        duration_s = 2 * volume_m3 / peak_m3s
        times = np.linspace(0, duration_s, 100_001)
        flow = np.array(
            [
                _hydrograph_discharge(
                    time_s, peak_m3s, time_to_peak_s, duration_s
                )
                for time_s in times
            ]
        )
        integrated_m3 = np.trapezoid(flow, times)
        self.assertAlmostEqual(integrated_m3 / volume_m3, 1.0, places=6)

    def test_outflow_limiter_cannot_drain_more_than_available(self) -> None:
        depth = np.array([[1.0, 0.0]], dtype=np.float32)
        discharge_x = np.array([[1_000.0]], dtype=np.float32)
        discharge_y = np.empty((0, 2), dtype=np.float32)
        limited_x, _ = _limit_outflow(
            discharge_x,
            discharge_y,
            depth,
            timestep_s=1.0,
            cell_width_m=10.0,
            cell_height_m=10.0,
        )
        self.assertAlmostEqual(float(limited_x[0, 0]), 10.0, places=5)


if __name__ == "__main__":
    unittest.main()
