import { useDashboardStore } from '@/store/dashboard-store';
import { SchoolCase, SchoolClosure, SchoolSummary } from '@/types';

// ---------------------------------------------------------------------------
// Factory helpers
// ---------------------------------------------------------------------------

function makeCase(overrides: Partial<SchoolCase> = {}): SchoolCase {
  return {
    collected_date: '2020-09-15',
    reported_date: '2020-09-15',
    school_board: 'Toronto DSB',
    school_number: 123456,
    school_name: 'Test School',
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
    new_total_school_related_cases: 10,
    new_school_related_student_cases: 7,
    new_school_related_staff_cases: 3,
    new_school_related_unspecified_cases: 0,
    recent_total_school_related_cases: 50,
    recent_school_related_student_cases: 35,
    recent_school_related_staff_cases: 15,
    recent_school_related_unspecified_cases: 0,
    past_total_school_related_cases: 100,
    past_school_related_student_cases: 70,
    past_school_related_staff_cases: 30,
    past_school_related_unspecified_cases: 0,
    cumulative_school_related_cases: cumulative,
    cumulative_school_related_student_cases: Math.round(cumulative * 0.7),
    cumulative_school_related_staff_cases: Math.round(cumulative * 0.3),
    cumulative_school_related_unspecified_cases: 0,
  };
}

// ---------------------------------------------------------------------------
// Reset store before each test
// ---------------------------------------------------------------------------

beforeEach(() => {
  useDashboardStore.setState({
    schoolCases: [],
    schoolCasesWithDemographics: [],
    schoolClosures: [],
    schoolSummaries: [],
    filteredCases: [],
    filteredClosures: [],
    isLoading: false,
    error: null,
    availableDates: [],
    minDate: null,
    maxDate: null,
    selectedDate: new Date('2020-09-15'),
    showCases: true,
    showClosures: true,
    showDemographics: true,
    selectedSchoolBoard: undefined,
    selectedMunicipality: undefined,
    activeTab: 'map',
    isMapFullscreen: false,
    isAnimating: false,
    startDate: new Date('2020-09-15'),
    endDate: null,
    showTimeslider: false,
    dashboardStats: {
      totalSchools: null,
      schoolsWithCases: 0,
      schoolsClosed: 0,
      totalCases: 0,
      studentCases: 0,
      staffCases: 0,
      percentageSchoolsWithCases: null,
      percentageSchoolsClosed: null,
    },
  });
});

// ---------------------------------------------------------------------------
// updateFilteredData — cases
// ---------------------------------------------------------------------------

describe('updateFilteredData — cases', () => {
  test('1. single-day: only records matching startDate are returned', () => {
    useDashboardStore.setState({
      schoolCasesWithDemographics: [
        makeCase({ collected_date: '2020-09-15' }),
        makeCase({ collected_date: '2020-09-16' }),
        makeCase({ collected_date: '2020-09-14' }),
      ],
      startDate: new Date('2020-09-15'),
      endDate: null,
    });
    useDashboardStore.getState().updateFilteredData();
    const { filteredCases } = useDashboardStore.getState();
    expect(filteredCases).toHaveLength(1);
    expect(filteredCases[0].collected_date).toBe('2020-09-15');
  });

  test('2. range: records on boundary dates (startDate AND endDate) are both included', () => {
    useDashboardStore.setState({
      schoolCasesWithDemographics: [
        makeCase({ collected_date: '2020-09-10' }), // exactly startDate
        makeCase({ collected_date: '2020-09-12' }), // mid-range
        makeCase({ collected_date: '2020-09-20' }), // exactly endDate
        makeCase({ collected_date: '2020-09-21' }), // after endDate — excluded
        makeCase({ collected_date: '2020-09-09' }), // before startDate — excluded
      ],
      startDate: new Date('2020-09-10'),
      endDate: new Date('2020-09-20'),
    });
    useDashboardStore.getState().updateFilteredData();
    const { filteredCases } = useDashboardStore.getState();
    expect(filteredCases).toHaveLength(3);
  });

  test('3. invalid collected_date ("not-a-date") is skipped without crashing', () => {
    const warnSpy = jest.spyOn(console, 'warn').mockImplementation(() => {});
    useDashboardStore.setState({
      schoolCasesWithDemographics: [
        makeCase({ collected_date: 'not-a-date' }),
        makeCase({ collected_date: '2020-09-15' }),
      ],
      startDate: new Date('2020-09-15'),
      endDate: null,
    });
    expect(() => useDashboardStore.getState().updateFilteredData()).not.toThrow();
    const { filteredCases } = useDashboardStore.getState();
    expect(filteredCases).toHaveLength(1);
    expect(warnSpy).toHaveBeenCalled();
    warnSpy.mockRestore();
  });

  test('4. empty string collected_date is skipped', () => {
    useDashboardStore.setState({
      schoolCasesWithDemographics: [
        makeCase({ collected_date: '' }),
        makeCase({ collected_date: '2020-09-15' }),
      ],
      startDate: new Date('2020-09-15'),
      endDate: null,
    });
    useDashboardStore.getState().updateFilteredData();
    const { filteredCases } = useDashboardStore.getState();
    expect(filteredCases).toHaveLength(1);
  });

  test('5. selectedSchoolBoard filter works (only matching board returned)', () => {
    useDashboardStore.setState({
      schoolCasesWithDemographics: [
        makeCase({ school_board: 'Toronto DSB' }),
        makeCase({ school_board: 'Ottawa DSB' }),
        makeCase({ school_board: 'Toronto DSB' }),
      ],
      startDate: new Date('2020-09-15'),
      endDate: null,
      selectedSchoolBoard: 'Toronto DSB',
    });
    useDashboardStore.getState().updateFilteredData();
    const { filteredCases } = useDashboardStore.getState();
    expect(filteredCases).toHaveLength(2);
    expect(filteredCases.every(c => c.school_board === 'Toronto DSB')).toBe(true);
  });

  test('6. selectedMunicipality filter uses case_.municipality field (not city)', () => {
    useDashboardStore.setState({
      schoolCasesWithDemographics: [
        makeCase({ municipality: 'Toronto' }),
        makeCase({ municipality: 'Ottawa' }),
      ],
      startDate: new Date('2020-09-15'),
      endDate: null,
      selectedMunicipality: 'Toronto',
    });
    useDashboardStore.getState().updateFilteredData();
    const { filteredCases } = useDashboardStore.getState();
    expect(filteredCases).toHaveLength(1);
    expect(filteredCases[0].municipality).toBe('Toronto');
  });

  test('7. showCases = false → filteredCases is empty', () => {
    useDashboardStore.setState({
      schoolCasesWithDemographics: [
        makeCase({ collected_date: '2020-09-15' }),
        makeCase({ collected_date: '2020-09-15' }),
      ],
      startDate: new Date('2020-09-15'),
      endDate: null,
      showCases: false,
    });
    useDashboardStore.getState().updateFilteredData();
    const { filteredCases } = useDashboardStore.getState();
    expect(filteredCases).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// updateFilteredData — closures
// ---------------------------------------------------------------------------

describe('updateFilteredData — closures', () => {
  test('8. single-day: closure spanning the selected day is included', () => {
    useDashboardStore.setState({
      schoolClosures: [
        makeClosure({ date_of_closure: '2020-09-16', date_of_reopening: '2020-09-30' }),
      ],
      startDate: new Date('2020-09-20'),
      endDate: null,
      showClosures: true,
    });
    useDashboardStore.getState().updateFilteredData();
    const { filteredClosures } = useDashboardStore.getState();
    expect(filteredClosures).toHaveLength(1);
  });

  test('9. single-day: closure that ended before the day is excluded', () => {
    useDashboardStore.setState({
      schoolClosures: [
        makeClosure({ date_of_closure: '2020-09-01', date_of_reopening: '2020-09-14' }),
      ],
      startDate: new Date('2020-09-20'),
      endDate: null,
      showClosures: true,
    });
    useDashboardStore.getState().updateFilteredData();
    const { filteredClosures } = useDashboardStore.getState();
    expect(filteredClosures).toHaveLength(0);
  });

  test('10. board filter on closures works with board_name field', () => {
    useDashboardStore.setState({
      schoolClosures: [
        makeClosure({ board_name: 'Toronto DSB' }),
        makeClosure({ board_name: 'Ottawa DSB' }),
      ],
      startDate: new Date('2020-09-20'),
      endDate: null,
      showClosures: true,
      selectedSchoolBoard: 'Toronto DSB',
    });
    useDashboardStore.getState().updateFilteredData();
    const { filteredClosures } = useDashboardStore.getState();
    expect(filteredClosures).toHaveLength(1);
    expect(filteredClosures[0].board_name).toBe('Toronto DSB');
  });
});

// ---------------------------------------------------------------------------
// updateDashboardStats
// ---------------------------------------------------------------------------

describe('updateDashboardStats', () => {
  test('11. totalSchools = null when schoolSummaries is empty', () => {
    useDashboardStore.setState({
      schoolSummaries: [],
      filteredCases: [],
      filteredClosures: [],
      startDate: new Date('2020-09-15'),
      endDate: null,
    });
    useDashboardStore.getState().updateDashboardStats();
    expect(useDashboardStore.getState().dashboardStats.totalSchools).toBeNull();
  });

  test('12. percentageSchoolsWithCases = null when totalSchools = null', () => {
    useDashboardStore.setState({
      schoolSummaries: [],
      filteredCases: [makeCase()],
      filteredClosures: [],
      startDate: new Date('2020-09-15'),
      endDate: null,
    });
    useDashboardStore.getState().updateDashboardStats();
    expect(useDashboardStore.getState().dashboardStats.percentageSchoolsWithCases).toBeNull();
    expect(useDashboardStore.getState().dashboardStats.percentageSchoolsClosed).toBeNull();
  });

  test('13. year1 only: date before boundary uses only year1 summary row', () => {
    // year1 row: 2020-09-15 (< 2021-07-01), cumulative = 500
    // year2 row: 2021-08-25 (>= 2021-07-01), cumulative = 200
    // startDate is before the boundary → year1 included; year2 lookup date also before boundary so no year2 row
    useDashboardStore.setState({
      schoolSummaries: [
        makeSummary('2020-09-15', 500),
        makeSummary('2021-08-25', 200),
      ],
      filteredCases: [],
      filteredClosures: [],
      startDate: new Date('2020-09-15'),
      endDate: null,
    });
    useDashboardStore.getState().updateDashboardStats();
    const stats = useDashboardStore.getState().dashboardStats;
    // lookupDate = '2020-09-15': year1 row matches (2020-09-15 <= 2020-09-15);
    // year2 row '2021-08-25' > '2020-09-15' so no year2 contribution
    expect(stats.totalCases).toBe(500);
  });

  test('14. year2 only: startDate after boundary uses only year2 summary row', () => {
    useDashboardStore.setState({
      schoolSummaries: [
        makeSummary('2020-09-15', 500),
        makeSummary('2021-08-25', 200),
      ],
      filteredCases: [],
      filteredClosures: [],
      startDate: new Date('2021-08-25'),
      endDate: null,
    });
    useDashboardStore.getState().updateDashboardStats();
    const stats = useDashboardStore.getState().dashboardStats;
    // startDate '2021-08-25' >= YEAR_BOUNDARY '2021-07-01' → year1 contribution is null (0)
    // year2Row lookup: year2 row '2021-08-25' <= '2021-08-25' → cumulative = 200
    expect(stats.totalCases).toBe(200);
  });

  test('15. schoolsWithCases counts unique school_number values (two records same school → 1)', () => {
    useDashboardStore.setState({
      schoolSummaries: [],
      filteredCases: [
        makeCase({ school_number: 1001 }),
        makeCase({ school_number: 1001 }), // duplicate
        makeCase({ school_number: 1002 }),
      ],
      filteredClosures: [],
      startDate: new Date('2020-09-15'),
      endDate: null,
    });
    useDashboardStore.getState().updateDashboardStats();
    expect(useDashboardStore.getState().dashboardStats.schoolsWithCases).toBe(2);
  });
});

// ---------------------------------------------------------------------------
// setSelectedDate clamping
// ---------------------------------------------------------------------------

describe('setSelectedDate clamping', () => {
  test('16. date before minDate is clamped to minDate', () => {
    useDashboardStore.setState({
      minDate: '2020-09-10',
      maxDate: '2021-06-30',
    });
    useDashboardStore.getState().setSelectedDate(new Date('2020-01-01'));
    expect(useDashboardStore.getState().selectedDate).toEqual(new Date('2020-09-10'));
  });

  test('17. date after maxDate is clamped to maxDate', () => {
    useDashboardStore.setState({
      minDate: '2020-09-10',
      maxDate: '2021-06-30',
    });
    useDashboardStore.getState().setSelectedDate(new Date('2022-12-31'));
    expect(useDashboardStore.getState().selectedDate).toEqual(new Date('2021-06-30'));
  });
});

// ---------------------------------------------------------------------------
// resetFilters
// ---------------------------------------------------------------------------

describe('resetFilters', () => {
  test('18. after reset, selectedSchoolBoard and selectedMunicipality are undefined', () => {
    useDashboardStore.setState({
      selectedSchoolBoard: 'Toronto DSB',
      selectedMunicipality: 'Toronto',
    });
    useDashboardStore.getState().resetFilters();
    const state = useDashboardStore.getState();
    expect(state.selectedSchoolBoard).toBeUndefined();
    expect(state.selectedMunicipality).toBeUndefined();
  });

  test('19. after reset, showCases, showClosures, showDemographics are all true', () => {
    useDashboardStore.setState({
      showCases: false,
      showClosures: false,
      showDemographics: false,
    });
    useDashboardStore.getState().resetFilters();
    const state = useDashboardStore.getState();
    expect(state.showCases).toBe(true);
    expect(state.showClosures).toBe(true);
    expect(state.showDemographics).toBe(true);
  });

  test('after reset, showTimeslider is false', () => {
    useDashboardStore.setState({ showTimeslider: true });
    useDashboardStore.getState().resetFilters();
    expect(useDashboardStore.getState().showTimeslider).toBe(false);
  });
});
