export const TOTAL_CAPACITY_HM3 = 302.087;
export const USEFUL_VOLUME_HM3 = 247.57;
export const DEFAULT_RELEASE_FRACTION = USEFUL_VOLUME_HM3 / TOTAL_CAPACITY_HM3;

export interface VolumeScenario {
  storedVolumeHm3: number;
  releaseFraction: number;
  downstreamRetentionFraction: number;
}

export interface VolumeEstimate {
  releasedAtDamHm3: number;
  reachesStudyAreaHm3: number;
  residualHm3: number;
}

export function estimateReleasedVolume(scenario: VolumeScenario): VolumeEstimate {
  const stored = Math.max(0, scenario.storedVolumeHm3);
  const releaseFraction = clamp01(scenario.releaseFraction);
  const retentionFraction = clamp01(scenario.downstreamRetentionFraction);
  const releasedAtDamHm3 = stored * releaseFraction;

  return {
    releasedAtDamHm3,
    reachesStudyAreaHm3: releasedAtDamHm3 * (1 - retentionFraction),
    residualHm3: stored - releasedAtDamHm3,
  };
}

export function formatHm3(value: number): string {
  return new Intl.NumberFormat("pt-BR", {
    minimumFractionDigits: 1,
    maximumFractionDigits: 2,
  }).format(value);
}

function clamp01(value: number): number {
  return Math.min(1, Math.max(0, value));
}
