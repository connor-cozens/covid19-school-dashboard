import { useShallow } from 'zustand/react/shallow';
import { DashboardStats, SchoolCase, SchoolClosure, SchoolSummary } from '@/types';
import { closureMatchesDateRange } from '@/lib/closure-filter';
import { SCHOOL_YEAR_BOUNDARY } from '@/lib/constants';
import { useDashboardStore } from './dashboard-store';

// Values derived from the store's data and filters. Each is a pure function plus a hook.
// The functions are memoized on their inputs, so when several components use the same
// hook the work runs once per filter change, not once per component.

function memoizeOne<A extends unknown[], R>(fn: (...args: A) => R): (...args: A) => R {
  let lastArgs: A | null = null;
  let lastResult: R;
  return (...args: A) => {
    if (lastArgs && args.length === lastArgs.length && args.every((a, i) => Object.is(a, lastArgs![i]))) {
      return lastResult;
    }
    lastArgs = args;
    lastResult = fn(...args);
    return lastResult;
  };
}

const isSingleDay = (startDate: string, endDate: string | null) => !endDate || startDate === endDate;

// ---------------------------------------------------------------------------
// Filtered cases
// ---------------------------------------------------------------------------

/**
 * Case records for the selected day or range (inclusive on both ends), board and
 * municipality. The date filter uses `collected_date`, the day the province took the
 * snapshot. The municipality filter uses `municipality` (the Ontario government
 * field that fills the dropdown), not `city`, which comes from the demographics join.
 */
export function filterCases(
  casesByDate: Map<string, SchoolCase[]>,
  caseDates: string[],
  startDate: string,
  endDate: string | null,
  showCases: boolean,
  board: string | undefined,
  municipality: string | undefined,
): SchoolCase[] {
  if (!showCases || !startDate) return [];
  const days = isSingleDay(startDate, endDate)
    ? [startDate]
    : caseDates.filter((d) => d >= startDate && d <= endDate!);
  const result: SchoolCase[] = [];
  for (const day of days) {
    for (const c of casesByDate.get(day) ?? []) {
      if (board && c.school_board !== board) continue;
      if (municipality && c.municipality !== municipality) continue;
      result.push(c);
    }
  }
  return result;
}

const filterCasesMemo = memoizeOne(filterCases);

export function useFilteredCases(): SchoolCase[] {
  const s = useDashboardStore(useShallow((s) => [
    s.casesByDate, s.caseDates, s.startDate, s.endDate, s.showCases, s.selectedSchoolBoard, s.selectedMunicipality,
  ] as const));
  return filterCasesMemo(...s);
}

// ---------------------------------------------------------------------------
// Filtered closures
// ---------------------------------------------------------------------------

/** Closures active in the selected day or range (see closure-filter.ts), board and city. */
export function filterClosures(
  closures: SchoolClosure[],
  startDate: string,
  endDate: string | null,
  isAnimating: boolean,
  showClosures: boolean,
  board: string | undefined,
  municipality: string | undefined,
): SchoolClosure[] {
  if (!showClosures || !startDate) return [];
  return closures.filter((c) =>
    closureMatchesDateRange(c, startDate, endDate, isAnimating) &&
    (!board || c.board_name === board) &&
    (!municipality || c.city === municipality)
  );
}

const filterClosuresMemo = memoizeOne(filterClosures);

export function useFilteredClosures(): SchoolClosure[] {
  const s = useDashboardStore(useShallow((s) => [
    s.closures, s.startDate, s.endDate, s.isAnimating, s.showClosures, s.selectedSchoolBoard, s.selectedMunicipality,
  ] as const));
  return filterClosuresMemo(...s);
}

// ---------------------------------------------------------------------------
// Statistics
// ---------------------------------------------------------------------------

/**
 * Case totals come from the province's cumulative summaries, because per-school
 * `total_confirmed_cases` is a daily active-case snapshot, not a running total.
 * Each school year's summary restarts its cumulative count, so the two years are
 * looked up separately and added:
 *   year 1: 2020-09-10 → 2021-04-26
 *   year 2: 2021-08-25 → 2021-12-22
 * Year 1 counts only when the selected range starts before the year boundary.
 */
export function computeStats(
  summaries: SchoolSummary[],
  filteredCases: SchoolCase[],
  filteredClosures: SchoolClosure[],
  startDate: string,
  endDate: string | null,
): DashboardStats {
  const lookupDate = endDate || startDate;
  const lastOnOrBefore = (rows: SchoolSummary[]) => rows.filter((s) => s.collected_date <= lookupDate).at(-1);

  const year1Row = startDate < SCHOOL_YEAR_BOUNDARY
    ? lastOnOrBefore(summaries.filter((s) => s.collected_date < SCHOOL_YEAR_BOUNDARY))
    : undefined;
  const year2Row = lastOnOrBefore(summaries.filter((s) => s.collected_date >= SCHOOL_YEAR_BOUNDARY));
  const sum = (field: keyof SchoolSummary) =>
    (Number(year1Row?.[field]) || 0) + (Number(year2Row?.[field]) || 0);

  const schoolsWithCases = new Set(filteredCases.map((c) => c.school_number).filter(Boolean)).size;
  const schoolsClosed = new Set(filteredClosures.map((c) => c.school_name).filter(Boolean)).size;

  const schoolCounts = summaries
    .map((s) => s.current_total_number_schools)
    .filter((n): n is number => typeof n === 'number' && n > 0);
  const totalSchools = schoolCounts.length > 0 ? Math.max(...schoolCounts) : null;
  const pct = (n: number) => (totalSchools ? (n / totalSchools) * 100 : null);

  return {
    totalSchools,
    schoolsWithCases,
    schoolsClosed,
    totalCases: sum('cumulative_school_related_cases'),
    studentCases: sum('cumulative_school_related_student_cases'),
    staffCases: sum('cumulative_school_related_staff_cases'),
    percentageSchoolsWithCases: pct(schoolsWithCases),
    percentageSchoolsClosed: pct(schoolsClosed),
  };
}

const computeStatsMemo = memoizeOne(computeStats);

export function useDashboardStats(): DashboardStats {
  const filteredCases = useFilteredCases();
  const filteredClosures = useFilteredClosures();
  const [summaries, startDate, endDate] = useDashboardStore(useShallow((s) => [s.summaries, s.startDate, s.endDate] as const));
  return computeStatsMemo(summaries, filteredCases, filteredClosures, startDate, endDate);
}

// ---------------------------------------------------------------------------
// Filter options
// ---------------------------------------------------------------------------

const sortedUnique = (values: (string | null | undefined)[]) =>
  Array.from(new Set(values.filter((v): v is string => Boolean(v)))).sort((a, b) => a.localeCompare(b));

/** Dropdown options: boards and municipalities that appear in the cases or closures. */
export function computeFilterOptions(cases: SchoolCase[], closures: SchoolClosure[]) {
  return {
    schoolBoards: sortedUnique([...cases.map((c) => c.school_board), ...closures.map((c) => c.board_name)]),
    municipalities: sortedUnique([...cases.map((c) => c.municipality), ...closures.map((c) => c.city)]),
  };
}

const computeFilterOptionsMemo = memoizeOne(computeFilterOptions);

export function useFilterOptions() {
  const [cases, closures] = useDashboardStore(useShallow((s) => [s.cases, s.closures] as const));
  return computeFilterOptionsMemo(cases, closures);
}
