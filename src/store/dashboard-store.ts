import { create } from 'zustand';
import { SchoolCase, SchoolClosure, SchoolSummary, DashboardStats } from '@/types';
import { closureMatchesDateRange } from '@/lib/closure-filter';
import { getClosureBoardName, getClosureDate, getReopeningDate } from '@/lib/closure-fields';
import { SCHOOL_YEAR_BOUNDARY } from '@/lib/constants';

interface DashboardState {
  // Data
  schoolCases: SchoolCase[];
  schoolClosures: SchoolClosure[];
  schoolSummaries: SchoolSummary[];
  schoolCasesWithDemographics: SchoolCase[]; // new property
  isLoading: boolean;
  error: string | null;

  // Date Range
  availableDates: string[];
  minDate: string | null;
  maxDate: string | null;

  // UI State
  selectedDate: Date;
  showCases: boolean;
  showClosures: boolean;
  showDemographics: boolean;
  selectedSchoolBoard?: string;
  selectedMunicipality?: string;
  activeTab: 'map' | 'summary' | 'about' | 'team' | 'media' | 'data-sources';
  isMapFullscreen: boolean;

  // Computed State
  filteredCases: SchoolCase[];
  filteredClosures: SchoolClosure[];
  dashboardStats: DashboardStats;

  // Actions
  setData: (data: {
    schoolCases: SchoolCase[];
    schoolClosures: SchoolClosure[];
    schoolSummaries: SchoolSummary[];
  }) => void;
  setLoading: (loading: boolean) => void;
  setError: (error: string | null) => void;
  setSelectedDate: (date: Date) => void;
  setShowCases: (show: boolean) => void;
  setShowClosures: (show: boolean) => void;
  setShowDemographics: (show: boolean) => void;
  setSelectedSchoolBoard: (board?: string) => void;
  setSelectedMunicipality: (municipality?: string) => void;
  setActiveTab: (tab: 'map' | 'summary' | 'about' | 'team' | 'media' | 'data-sources') => void;
  setIsMapFullscreen: (fullscreen: boolean) => void;
  resetFilters: () => void;
  updateFilteredData: () => void;
  updateDashboardStats: () => void;
  setSchoolCasesWithDemographics: (data: SchoolCase[], schoolClosuresData?: SchoolClosure[]) => void; // new action
  setAvailableDates: (dates: string[]) => void;
  startDate: Date;
  endDate: Date | null;
  isAnimating: boolean;
  showTimeslider: boolean;
  setStartDate: (date: Date) => void;
  setEndDate: (date: Date) => void;
  setIsAnimating: (animating: boolean) => void;
  setShowTimeslider: (show: boolean) => void;
}

export const useDashboardStore = create<DashboardState>((set, get) => {
  // Helper to get initial date based on availableDates
  const getInitialDate = () => {
    const { availableDates } = get();
    if (availableDates && availableDates.length > 0) {
      return new Date(availableDates[0]);
    }
    const date = new Date();
    date.setHours(0, 0, 0, 0);
    return date;
  };

  return {
    // Initial state
    schoolCases: [],
    schoolClosures: [],
    schoolSummaries: [],
    schoolCasesWithDemographics: [], // new property
    isLoading: false,
    error: null,
    availableDates: [],
    minDate: null,
    maxDate: null,
    selectedDate: new Date(),
    showCases: true,
    showClosures: true,
    showDemographics: true,
    selectedSchoolBoard: undefined,
    selectedMunicipality: undefined,
    activeTab: 'map',
    isMapFullscreen: false,
    isAnimating: false,
    filteredCases: [],
    filteredClosures: [],
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
    startDate: new Date(),
    endDate: null,
    showTimeslider: false,

    // Actions
    setData: (data) => {
      set({ 
        schoolCases: data.schoolCases,
        schoolClosures: data.schoolClosures,
        schoolSummaries: data.schoolSummaries,
        isLoading: false,
        error: null
      });
      get().updateFilteredData();
      get().updateDashboardStats();
    },

    setLoading: (loading) => set({ isLoading: loading }),

    setError: (error) => set({ error, isLoading: false }),

    setSelectedDate: (date) => {
      const { minDate, maxDate } = get();
      let clampedDate = date;
      if (minDate && date < new Date(minDate)) clampedDate = new Date(minDate);
      if (maxDate && date > new Date(maxDate)) clampedDate = new Date(maxDate);
      set({ selectedDate: clampedDate });
      get().updateFilteredData();
      get().updateDashboardStats();
    },

    setShowCases: (show) => {
      set({ showCases: show });
      get().updateFilteredData();
    },

    setShowClosures: (show) => {
      set({ showClosures: show });
      get().updateFilteredData();
    },

    setShowDemographics: (show) => set({ showDemographics: show }),

    setSelectedSchoolBoard: (board) => {
      set({ selectedSchoolBoard: board });
      get().updateFilteredData();
      get().updateDashboardStats();
    },

    setSelectedMunicipality: (municipality) => {
      set({ selectedMunicipality: municipality });
      get().updateFilteredData();
      get().updateDashboardStats();
    },

    setActiveTab: (tab) => set({ activeTab: tab }),

    setIsMapFullscreen: (fullscreen) => set({ isMapFullscreen: fullscreen }),

    resetFilters: () => {
      set({
        startDate: getInitialDate(),
        endDate: getInitialDate(),
        showCases: true,
        showClosures: true,
        showDemographics: true,
        showTimeslider: false,
        selectedSchoolBoard: undefined,
        selectedMunicipality: undefined,
      });
      get().updateFilteredData();
      get().updateDashboardStats();
    },

    // Computed actions
    updateFilteredData: () => {
      const { schoolCasesWithDemographics, schoolClosures, startDate, endDate, showCases, showClosures, selectedSchoolBoard, selectedMunicipality, isAnimating } = get();
      let filteredCases: SchoolCase[] = [];
      let filteredClosures: SchoolClosure[] = [];

      // DATE FILTER SEMANTICS:
      // - The primary filter field for case records is `collected_date` (the date the Ontario
      //   government reported the snapshot, not `reported_date`).
      // - Single-day mode: exact equality (midnight UTC — both sides are midnight UTC dates so
      //   there are no timezone surprises as long as dates are parsed consistently).
      // - Range mode: inclusive on both ends (startDate <= caseDate <= endDate).
      //   A record whose collected_date equals exactly startDate or endDate IS included.
      // - Closure filter: date_of_closure <= selectedDate <= date_of_reopening (see closure-filter.ts).

      // If endDate is not set or equals startDate, treat as single day
      const isSingleDay = !endDate || startDate.getTime() === endDate.getTime();
      if (showCases) {
        filteredCases = schoolCasesWithDemographics.filter((case_: SchoolCase) => {
          if (!(typeof case_.collected_date === 'string' && case_.collected_date.trim() !== '')) return false;
          const caseDate = new Date(case_.collected_date);
          if (isNaN(caseDate.getTime())) {
            console.warn('updateFilteredData: skipping record with invalid collected_date', {
              school_name: case_.school_name,
              collected_date: case_.collected_date,
            });
            return false;
          }
          const isDateMatch = isSingleDay
            ? caseDate.getTime() === startDate.getTime()
            : caseDate >= startDate && caseDate <= endDate;
          const isBoardMatch = !selectedSchoolBoard || case_.school_board === selectedSchoolBoard;
          // `municipality` is from the Ontario gov CSV and drives the filter dropdown.
          // Do NOT use `city` here — that field comes from the demographics join and is
          // used for display in closure popups only. Mixing them was the root cause of bug #3.
          const isMunicipalityMatch = !selectedMunicipality || case_.municipality === selectedMunicipality;
          return isDateMatch && isBoardMatch && isMunicipalityMatch;
        });
      }
      if (showClosures) {
        filteredClosures = schoolClosures.filter(closure => {
          const overlaps = closureMatchesDateRange(closure, startDate, endDate, isAnimating);
          const isBoardMatch = !selectedSchoolBoard || getClosureBoardName(closure) === selectedSchoolBoard;
          const isMunicipalityMatch = !selectedMunicipality || closure.city === selectedMunicipality;
          return overlaps && isBoardMatch && isMunicipalityMatch;
        });
      }
      set({ filteredCases, filteredClosures });
    },

    updateDashboardStats: () => {
      const { filteredCases, filteredClosures, schoolSummaries, endDate, startDate } = get();

      // Use cumulative totals from the summary CSV for accurate case counts.
      // total_confirmed_cases in per-school records is a daily active-case snapshot, not cumulative.
      //
      // The two summary CSVs each reset their cumulative counter at the start of the school year:
      //   year1: 2020-09-10 → 2021-04-26  (max 15,002)
      //   year2: 2021-08-25 → 2021-12-22  (max 11,961)
      // We must sum both contributions independently rather than treating the merged array as one
      // running cumulative.
      const YEAR_BOUNDARY = SCHOOL_YEAR_BOUNDARY;
      const lookupDate = (endDate || startDate).toISOString().split('T')[0];
      const startDateStr = startDate.toISOString().split('T')[0];

      const year1Rows = schoolSummaries.filter(s => String(s.collected_date) < YEAR_BOUNDARY);
      const year2Rows = schoolSummaries.filter(s => String(s.collected_date) >= YEAR_BOUNDARY);

      // Only include year1 if the selected range starts before the year boundary
      const year1Row = startDateStr < YEAR_BOUNDARY
        ? year1Rows.filter(s => String(s.collected_date) <= lookupDate).at(-1)
        : null;
      const year2Row = year2Rows.filter(s => String(s.collected_date) <= lookupDate).at(-1);

      const totalCases = (year1Row?.cumulative_school_related_cases ?? 0) +
                         (year2Row?.cumulative_school_related_cases ?? 0);
      const studentCases = (year1Row?.cumulative_school_related_student_cases ?? 0) +
                           (year2Row?.cumulative_school_related_student_cases ?? 0);
      const staffCases = (year1Row?.cumulative_school_related_staff_cases ?? 0) +
                         (year2Row?.cumulative_school_related_staff_cases ?? 0);

      const uniqueSchoolsWithCases = new Set(filteredCases.map(c => c.school_number).filter(Boolean)).size;
      const uniqueSchoolsClosed = new Set(filteredClosures.map(c => c.school_name || c['School Name']).filter(Boolean)).size;

      // Derive total schools from summary CSV — null if unavailable
      let totalSchools: number | null = null;
      if (schoolSummaries.length > 0) {
        const validCounts = schoolSummaries
          .map(s => s.current_total_number_schools)
          .filter((n): n is number => typeof n === 'number' && !isNaN(n) && n > 0);
        if (validCounts.length > 0) {
          totalSchools = Math.max(...validCounts);
        } else {
          console.warn('updateDashboardStats: schoolSummaries present but no valid current_total_number_schools values found');
        }
      }

      const stats: DashboardStats = {
        totalSchools,
        schoolsWithCases: uniqueSchoolsWithCases,
        schoolsClosed: uniqueSchoolsClosed,
        totalCases,
        studentCases,
        staffCases,
        percentageSchoolsWithCases: totalSchools !== null && totalSchools > 0
          ? (uniqueSchoolsWithCases / totalSchools) * 100
          : null,
        percentageSchoolsClosed: totalSchools !== null && totalSchools > 0
          ? (uniqueSchoolsClosed / totalSchools) * 100
          : null,
      };

      set({ dashboardStats: stats });
    },

    // INITIALIZATION CONTRACT:
    // This action must be called AFTER setData() has populated schoolClosures, OR
    // the caller must pass schoolClosuresData explicitly (as DataProvider does).
    // Reason: the date-range calculation merges case dates with closure dates so the
    // timeline slider spans both datasets. If schoolClosures is empty when this runs,
    // closure dates will be missing from availableDates and the slider will be too short.
    // See DataProvider.tsx for the required call order.
    setSchoolCasesWithDemographics: (data, schoolClosuresData) => {
      // Extract unique, sorted dates from cases
      const caseDates = Array.from(new Set(data.map((d: SchoolCase) => d.collected_date)))
        .filter((date): date is string => Boolean(date));
      // Extract unique, sorted dates from closures (including both closure and reopening dates)
      const closuresToUse = schoolClosuresData || get().schoolClosures;
      const closureDates = Array.from(new Set(closuresToUse.flatMap((c: SchoolClosure) => [
        getClosureDate(c),
        getReopeningDate(c),
      ]).filter((date): date is string => Boolean(date))));
      // Combine and sort all dates
      const allDates = Array.from(new Set([...caseDates, ...closureDates])).sort();
      const minDate = allDates.length > 0 ? allDates[0] : null;
      const maxDate = allDates.length > 0 ? allDates[allDates.length - 1] : null;
      set({ schoolCasesWithDemographics: data, availableDates: allDates, minDate, maxDate });
      // Set startDate and selectedDate to minDate if not already set or out of range
      const { startDate, selectedDate } = get();
      if (minDate && maxDate) {
        const minDateObj = new Date(minDate);
        const maxDateObj = new Date(maxDate);
        const updates: Partial<DashboardState> = {};
        
        if (!startDate || startDate < minDateObj || startDate > maxDateObj) {
          updates.startDate = minDateObj;
        }
        
        if (!selectedDate || selectedDate < minDateObj || selectedDate > maxDateObj) {
          updates.selectedDate = minDateObj;
        }
        
        if (Object.keys(updates).length > 0) {
          set(updates);
        }
      }
      get().updateFilteredData();
      get().updateDashboardStats();
    },
    setAvailableDates: (dates) => {
      // Also consider closure dates for min/max (including both closure and reopening dates)
      const { schoolClosures } = get();
      const closureDates = Array.from(new Set(schoolClosures.flatMap((c: SchoolClosure) => [
        getClosureDate(c),
        getReopeningDate(c),
      ].filter((date): date is string => Boolean(date)))));
      const allDates = Array.from(new Set([...dates, ...closureDates])).sort();
      const minDate = allDates.length > 0 ? allDates[0] : null;
      const maxDate = allDates.length > 0 ? allDates[allDates.length - 1] : null;
      set({ availableDates: allDates, minDate, maxDate });
    },
    setStartDate: (date) => {
      set({ startDate: date });
      // Use setTimeout to prevent UI blocking
      setTimeout(() => {
        get().updateFilteredData();
        get().updateDashboardStats();
      }, 0);
    },
    setEndDate: (date) => {
      set({ endDate: date });
      // Use setTimeout to prevent UI blocking
      setTimeout(() => {
        get().updateFilteredData();
        get().updateDashboardStats();
      }, 0);
    },
    setIsAnimating: (animating) => set({ isAnimating: animating }),
    setShowTimeslider: (show) => set({ showTimeslider: show }),
  };
}); 