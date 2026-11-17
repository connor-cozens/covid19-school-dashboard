import { useDashboardStore } from '@/store/dashboard-store';
import { filterCases, filterClosures, computeStats, computeFilterOptions } from '@/store/derived';
import { DashboardData } from '@/lib/data-loader';
import { SchoolCase, SchoolClosure, SchoolSummary } from '@/types';

// ---------------------------------------------------------------------------
// Factory helpers
// ---------------------------------------------------------------------------

function makeCase(overrides: Partial<SchoolCase> = {}): SchoolCase {
  return {
    collected_date: '2020-09-15',
    school_board: 'Toronto DSB',
    school_number: 123456,
    school_name: 'Test School',
    city: 'Toronto',
    municipality: 'Toronto',
    confirmed_student_cases: 2,
    confirmed_staff_cases: 1,
    confirmed_unspecified_cases: 0,
    total_confirmed_cases: 3,
    latitude: 43.7,
    longitude: -79.4,
    ...overrides,
  };
}

function makeClosure(overrides: Partial<SchoolClosure> = {}): SchoolClosure {
  return {
    school_name: 'Test School',
    date_of_closure: '2020-09-16',
    date_of_reopening: '2020-09-30',
    board_name: 'Toronto DSB',
    city: 'Toronto',
    latitude: 43.7,
    longitude: -79.4,
    ...overrides,
  };
}

function makeSummary(date: string, cumulative: number): SchoolSummary {
  return {
    collected_date: date,
    reported_date: date,
    current_schools_w_cases: 5,
    current_schools_closed: 2,
    current_total_number_schools: 3800,
    cumulative_school_related_cases: cumulative,
    cumulative_school_related_student_cases: Math.round(cumulative * 0.7),
    cumulative_school_related_staff_cases: Math.round(cumulative * 0.3),
    cumulative_school_related_unspecified_cases: 0,
  };
}

function byDate(cases: SchoolCase[]) {
  const map = new Map<string, SchoolCase[]>();
  for (const c of cases) map.set(c.collected_date, [...(map.get(c.collected_date) ?? []), c]);
  return { casesByDate: map, caseDates: Array.from(map.keys()).sort() };
}

/** filterCases with the store's defaults, overridable per test. */
function runFilterCases(
  cases: SchoolCase[],
  opts: { start?: string; end?: string | null; show?: boolean; board?: string; municipality?: string } = {},
) {
  const { casesByDate, caseDates } = byDate(cases);
  return filterCases(
    casesByDate, caseDates, opts.start ?? '2020-09-15', opts.end ?? null,
    opts.show ?? true, opts.board, opts.municipality,
  );
}

function makeData(overrides: Partial<DashboardData> = {}): DashboardData {
  return { cases: [], closures: [], summaries: [], profiles: {}, ...overrides };
}

beforeEach(() => {
  useDashboardStore.getState().setData(makeData());
  useDashboardStore.getState().resetFilters();
});

// ---------------------------------------------------------------------------
// filterCases
// ---------------------------------------------------------------------------

describe('filterCases', () => {
  test('1. single-day: only records matching startDate are returned', () => {
    const result = runFilterCases([
      makeCase({ collected_date: '2020-09-15' }),
      makeCase({ collected_date: '2020-09-16' }),
      makeCase({ collected_date: '2020-09-14' }),
    ]);
    expect(result).toHaveLength(1);
    expect(result[0].collected_date).toBe('2020-09-15');
  });

  test('2. range: records on boundary dates (startDate AND endDate) are both included', () => {
    const result = runFilterCases([
      makeCase({ collected_date: '2020-09-10' }), // exactly startDate
      makeCase({ collected_date: '2020-09-12' }), // mid-range
      makeCase({ collected_date: '2020-09-20' }), // exactly endDate
      makeCase({ collected_date: '2020-09-21' }), // after endDate: excluded
      makeCase({ collected_date: '2020-09-09' }), // before startDate: excluded
    ], { start: '2020-09-10', end: '2020-09-20' });
    expect(result).toHaveLength(3);
  });

  test('3. startDate equal to endDate is a single day', () => {
    const result = runFilterCases([
      makeCase({ collected_date: '2020-09-15' }),
      makeCase({ collected_date: '2020-09-16' }),
    ], { start: '2020-09-15', end: '2020-09-15' });
    expect(result).toHaveLength(1);
  });

  test('4. invalid or empty collected_date never matches', () => {
    const result = runFilterCases([
      makeCase({ collected_date: 'not-a-date' }),
      makeCase({ collected_date: '' }),
      makeCase({ collected_date: '2020-09-15' }),
    ], { start: '2020-09-01', end: '2020-12-31' });
    expect(result).toHaveLength(1);
  });

  test('5. selectedSchoolBoard filter works (only matching board returned)', () => {
    const result = runFilterCases([
      makeCase({ school_board: 'Toronto DSB' }),
      makeCase({ school_board: 'Ottawa DSB' }),
      makeCase({ school_board: 'Toronto DSB' }),
    ], { board: 'Toronto DSB' });
    expect(result).toHaveLength(2);
    expect(result.every((c) => c.school_board === 'Toronto DSB')).toBe(true);
  });

  test('6. selectedMunicipality filter uses the municipality field (not city)', () => {
    const result = runFilterCases([
      makeCase({ municipality: 'Toronto', city: 'Ottawa' }),
      makeCase({ municipality: 'Ottawa', city: 'Toronto' }),
    ], { municipality: 'Toronto' });
    expect(result).toHaveLength(1);
    expect(result[0].municipality).toBe('Toronto');
  });

  test('7. showCases = false → no cases', () => {
    expect(runFilterCases([makeCase(), makeCase()], { show: false })).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// filterClosures
// ---------------------------------------------------------------------------

describe('filterClosures', () => {
  test('8. single-day: closure spanning the selected day is included', () => {
    const closures = [makeClosure({ date_of_closure: '2020-09-16', date_of_reopening: '2020-09-30' })];
    expect(filterClosures(closures, '2020-09-20', null, false, true, undefined, undefined)).toHaveLength(1);
  });

  test('9. single-day: closure that ended before the day is excluded', () => {
    const closures = [makeClosure({ date_of_closure: '2020-09-01', date_of_reopening: '2020-09-14' })];
    expect(filterClosures(closures, '2020-09-20', null, false, true, undefined, undefined)).toHaveLength(0);
  });

  test('10. board filter uses board_name, municipality filter uses city', () => {
    const closures = [
      makeClosure({ board_name: 'Toronto DSB', city: 'Toronto' }),
      makeClosure({ board_name: 'Ottawa DSB', city: 'Ottawa' }),
    ];
    const byBoard = filterClosures(closures, '2020-09-20', null, false, true, 'Toronto DSB', undefined);
    expect(byBoard.map((c) => c.board_name)).toEqual(['Toronto DSB']);
    const byCity = filterClosures(closures, '2020-09-20', null, false, true, undefined, 'Ottawa');
    expect(byCity.map((c) => c.city)).toEqual(['Ottawa']);
  });

  test('showClosures = false → no closures', () => {
    expect(filterClosures([makeClosure()], '2020-09-20', null, false, false, undefined, undefined)).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// computeStats
// ---------------------------------------------------------------------------

describe('computeStats', () => {
  test('11. totalSchools = null when there are no summaries', () => {
    expect(computeStats([], [], [], '2020-09-15', null).totalSchools).toBeNull();
  });

  test('12. percentages are null when totalSchools = null', () => {
    const stats = computeStats([], [makeCase()], [], '2020-09-15', null);
    expect(stats.percentageSchoolsWithCases).toBeNull();
    expect(stats.percentageSchoolsClosed).toBeNull();
  });

  test('13. year1 only: a date before the boundary uses only the year1 summary row', () => {
    const summaries = [makeSummary('2020-09-15', 500), makeSummary('2021-08-25', 200)];
    expect(computeStats(summaries, [], [], '2020-09-15', null).totalCases).toBe(500);
  });

  test('14. year2 only: a startDate after the boundary uses only the year2 summary row', () => {
    const summaries = [makeSummary('2020-09-15', 500), makeSummary('2021-08-25', 200)];
    expect(computeStats(summaries, [], [], '2021-08-25', null).totalCases).toBe(200);
  });

  test('a range across the boundary adds both years', () => {
    const summaries = [makeSummary('2020-09-15', 500), makeSummary('2021-08-25', 200)];
    expect(computeStats(summaries, [], [], '2020-09-15', '2021-09-01').totalCases).toBe(700);
  });

  test('15. schoolsWithCases counts unique school_number values', () => {
    const cases = [makeCase({ school_number: 1001 }), makeCase({ school_number: 1001 }), makeCase({ school_number: 1002 })];
    expect(computeStats([], cases, [], '2020-09-15', null).schoolsWithCases).toBe(2);
  });

  test('percentages use the largest school count in the summaries', () => {
    const stats = computeStats([makeSummary('2020-09-15', 10)], [makeCase()], [makeClosure()], '2020-09-15', null);
    expect(stats.totalSchools).toBe(3800);
    expect(stats.percentageSchoolsWithCases).toBeCloseTo(100 / 3800);
    expect(stats.percentageSchoolsClosed).toBeCloseTo(100 / 3800);
  });
});

// ---------------------------------------------------------------------------
// computeFilterOptions
// ---------------------------------------------------------------------------

describe('computeFilterOptions', () => {
  test('boards and municipalities from cases and closures, sorted and unique', () => {
    const options = computeFilterOptions(
      [makeCase({ school_board: 'B Board', municipality: 'Toronto' }), makeCase({ school_board: 'A Board', municipality: 'Toronto' })],
      [makeClosure({ board_name: 'C Board', city: 'Ajax' })],
    );
    expect(options.schoolBoards).toEqual(['A Board', 'B Board', 'C Board']);
    expect(options.municipalities).toEqual(['Ajax', 'Toronto']);
  });
});

// ---------------------------------------------------------------------------
// Store actions
// ---------------------------------------------------------------------------

describe('setData', () => {
  test('timeline spans case dates and closure/reopening dates; startDate starts at the first', () => {
    useDashboardStore.getState().setData(makeData({
      cases: [makeCase({ collected_date: '2020-09-15' }), makeCase({ collected_date: '2020-09-10' })],
      closures: [makeClosure({ date_of_closure: '2020-09-12', date_of_reopening: '2020-10-01' })],
    }));
    const s = useDashboardStore.getState();
    expect(s.status).toBe('ready');
    expect(s.availableDates).toEqual(['2020-09-10', '2020-09-12', '2020-09-15', '2020-10-01']);
    expect(s.caseDates).toEqual(['2020-09-10', '2020-09-15']);
    expect(s.minDate).toBe('2020-09-10');
    expect(s.maxDate).toBe('2020-10-01');
    expect(s.startDate).toBe('2020-09-10');
    expect(s.endDate).toBeNull();
  });
});

describe('date clamping', () => {
  beforeEach(() => {
    useDashboardStore.getState().setData(makeData({
      cases: [makeCase({ collected_date: '2020-09-10' }), makeCase({ collected_date: '2021-06-30' })],
    }));
  });

  test('16. a date before minDate is clamped to minDate', () => {
    useDashboardStore.getState().setStartDate('2020-01-01');
    expect(useDashboardStore.getState().startDate).toBe('2020-09-10');
  });

  test('17. a date after maxDate is clamped to maxDate', () => {
    useDashboardStore.getState().setEndDate('2022-12-31');
    expect(useDashboardStore.getState().endDate).toBe('2021-06-30');
  });
});

describe('resetFilters', () => {
  test('18. after reset, selectedSchoolBoard and selectedMunicipality are undefined', () => {
    useDashboardStore.setState({ selectedSchoolBoard: 'Toronto DSB', selectedMunicipality: 'Toronto' });
    useDashboardStore.getState().resetFilters();
    const state = useDashboardStore.getState();
    expect(state.selectedSchoolBoard).toBeUndefined();
    expect(state.selectedMunicipality).toBeUndefined();
  });

  test('19. after reset, showCases, showClosures, showDemographics are all true', () => {
    useDashboardStore.setState({ showCases: false, showClosures: false, showDemographics: false });
    useDashboardStore.getState().resetFilters();
    const state = useDashboardStore.getState();
    expect(state.showCases).toBe(true);
    expect(state.showClosures).toBe(true);
    expect(state.showDemographics).toBe(true);
  });

  test('after reset, showTimeslider is false and the range is back to one day', () => {
    useDashboardStore.setState({ showTimeslider: true, startDate: '2021-01-01', endDate: '2021-02-01' });
    useDashboardStore.getState().resetFilters();
    const state = useDashboardStore.getState();
    expect(state.showTimeslider).toBe(false);
    expect(state.endDate).toBeNull();
  });
});
