import { describe, expect, it } from "vitest";
import {
  DEFAULT_RELEASE_FRACTION,
  TOTAL_CAPACITY_HM3,
  USEFUL_VOLUME_HM3,
  estimateReleasedVolume,
} from "./scenario";

describe("estimateReleasedVolume", () => {
  it("reproduz o volume útil no cenário-base", () => {
    const result = estimateReleasedVolume({
      storedVolumeHm3: TOTAL_CAPACITY_HM3,
      releaseFraction: DEFAULT_RELEASE_FRACTION,
      downstreamRetentionFraction: 0,
    });

    expect(result.releasedAtDamHm3).toBeCloseTo(USEFUL_VOLUME_HM3, 6);
    expect(result.residualHm3).toBeCloseTo(TOTAL_CAPACITY_HM3 - USEFUL_VOLUME_HM3, 6);
  });

  it("limita frações ao intervalo físico de zero a um", () => {
    expect(
      estimateReleasedVolume({
        storedVolumeHm3: 100,
        releaseFraction: 2,
        downstreamRetentionFraction: -1,
      }).releasedAtDamHm3,
    ).toBe(100);
  });
});
