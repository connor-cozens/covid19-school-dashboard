import { SchoolCase } from '@/types';
import { DemographicsData } from '@/hooks/useDemographicsLookup';
import {
  CASE_COLOR_LOW,
  CASE_COLOR_HIGH,
  CASE_LOW_THRESHOLD,
  MARKER_RADIUS_MIN,
  MARKER_RADIUS_MAX,
  MARKER_RADIUS_SCALE,
} from '@/lib/constants';

export type AggregatedSchoolCase = SchoolCase & {
  _recordCount: number;
  _demo: DemographicsData | null;
};

/**
 * Aggregates per-day school case records into one entry per school.
 *
 * Deduplication key: `school_number` when present, otherwise `school_name`.
 * Records with neither are skipped with a console warning.
 *
 * After aggregation, entries missing latitude or longitude are filtered out
 * (they cannot be placed on the map), and demographics from `demographicsLookup`
 * are attached to `_demo`.
 */
export function aggregateCases(
  filteredCases: SchoolCase[],
  demographicsLookup: Record<number, DemographicsData>
): AggregatedSchoolCase[] {
  const bySchool = new Map<string | number, AggregatedSchoolCase>();

  filteredCases.forEach(c => {
    const key = c.school_number || c.school_name;
    if (!key) {
      console.warn('InteractiveMap: skipping record with no school_number or school_name', c);
      return;
    }
    if (!bySchool.has(key)) {
      bySchool.set(key, {
        ...c,
        confirmed_student_cases: 0,
        confirmed_staff_cases: 0,
        confirmed_unspecified_cases: 0,
        total_confirmed_cases: 0,
        _recordCount: 0,
        _demo: null,
      });
    }
    const existing = bySchool.get(key)!;
    existing.confirmed_student_cases += c.confirmed_student_cases || 0;
    existing.confirmed_staff_cases += c.confirmed_staff_cases || 0;
    existing.confirmed_unspecified_cases += c.confirmed_unspecified_cases || 0;
    existing.total_confirmed_cases += c.total_confirmed_cases || 0;
    existing._recordCount += 1;
    // Use the most recent date for display
    if (!existing.collected_date || c.collected_date > existing.collected_date) {
      existing.collected_date = c.collected_date;
    }
  });

  // Attach demographics data now that lookup is loaded — stored directly on the
  // aggregated entry so popup JSX can read it without an async call or timing dependency.
  return Array.from(bySchool.values())
    .filter(s => s.latitude && s.longitude)
    .map(s => ({
      ...s,
      _demo: s.school_number
        ? (demographicsLookup[s.school_number] ?? null)
        : null,
    }));
}

/**
 * Returns the marker fill/stroke colour for a school's total case count.
 * Matches the R app thresholds: orange for 1–5 cases, red for 6+.
 */
export function getCaseMarkerColor(totalCases: number): string {
  return totalCases <= CASE_LOW_THRESHOLD ? CASE_COLOR_LOW : CASE_COLOR_HIGH;
}

/**
 * Returns the circle marker radius (pixels) for a school's total case count.
 * Scales linearly with `MARKER_RADIUS_SCALE`, clamped to [MARKER_RADIUS_MIN, MARKER_RADIUS_MAX].
 */
export function getCaseMarkerRadius(totalCases: number): number {
  return Math.max(
    MARKER_RADIUS_MIN,
    Math.min(MARKER_RADIUS_MAX, MARKER_RADIUS_MIN + totalCases * MARKER_RADIUS_SCALE)
  );
}
