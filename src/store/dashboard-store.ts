import { create } from 'zustand';
import { SchoolCase, SchoolClosure, SchoolProfiles, SchoolSummary } from '@/types';
import { DashboardData, loadDashboardData } from '@/lib/data-loader';

// The store holds only loaded data and the user's choices. Everything shown that
// depends on both (filtered records, statistics, filter options) is derived in
// src/store/derived.ts, so no action has to remember to recompute anything.
// Dates are ISO 'YYYY-MM-DD' strings.

export interface Filters {
  /** First (or only) selected day. */
  startDate: string;
  /** Last selected day; null or equal to startDate means a single day. */
  endDate: string | null;
  showCases: boolean;
  showClosures: boolean;
  showDemographics: boolean;
  showTimeslider: boolean;
  selectedSchoolBoard?: string;
  selectedMunicipality?: string;
  /** True while the date-range animation plays; closures then show as of endDate. */
  isAnimating: boolean;
}

interface DashboardState extends Filters {
  status: 'idle' | 'loading' | 'ready' | 'error';
  error: string | null;

  cases: SchoolCase[];
  /** Case records grouped by collected_date, for fast date filtering. */
  casesByDate: Map<string, SchoolCase[]>;
  /** Sorted dates that have case records. */
  caseDates: string[];
  closures: SchoolClosure[];
  summaries: SchoolSummary[];
  profiles: SchoolProfiles;
  /** Sorted union of case dates and closure/reopening dates: the timeline's steps. */
  availableDates: string[];
  minDate: string | null;
  maxDate: string | null;

  isMapFullscreen: boolean;

  loadData: () => Promise<void>;
  setData: (data: DashboardData) => void;
  setStartDate: (date: string) => void;
  setEndDate: (date: string | null) => void;
  setShowCases: (show: boolean) => void;
  setShowClosures: (show: boolean) => void;
  setShowDemographics: (show: boolean) => void;
  setShowTimeslider: (show: boolean) => void;
  setSelectedSchoolBoard: (board?: string) => void;
  setSelectedMunicipality: (municipality?: string) => void;
  setIsAnimating: (animating: boolean) => void;
  setIsMapFullscreen: (fullscreen: boolean) => void;
  resetFilters: () => void;
}

const DEFAULT_FILTERS: Omit<Filters, 'startDate'> = {
  endDate: null,
  showCases: true,
  showClosures: true,
  showDemographics: true,
  showTimeslider: false,
  selectedSchoolBoard: undefined,
  selectedMunicipality: undefined,
  isAnimating: false,
};

function groupByDate(cases: SchoolCase[]): Map<string, SchoolCase[]> {
  const byDate = new Map<string, SchoolCase[]>();
  for (const c of cases) {
    const list = byDate.get(c.collected_date);
    if (list) list.push(c);
    else byDate.set(c.collected_date, [c]);
  }
  return byDate;
}

export const useDashboardStore = create<DashboardState>((set, get) => {
  /** Keeps a date inside the loaded data's range. */
  const clamp = (date: string) => {
    const { minDate, maxDate } = get();
    if (minDate && date < minDate) return minDate;
    if (maxDate && date > maxDate) return maxDate;
    return date;
  };

  return {
    status: 'idle',
    error: null,
    cases: [],
    casesByDate: new Map(),
    caseDates: [],
    closures: [],
    summaries: [],
    profiles: {},
    availableDates: [],
    minDate: null,
    maxDate: null,
    startDate: '',
    ...DEFAULT_FILTERS,
    isMapFullscreen: false,

    loadData: async () => {
      set({ status: 'loading', error: null });
      try {
        get().setData(await loadDashboardData());
      } catch (err) {
        set({ status: 'error', error: err instanceof Error ? err.message : String(err) });
      }
    },

    setData: ({ cases, closures, summaries, profiles }) => {
      const casesByDate = groupByDate(cases);
      const caseDates = Array.from(casesByDate.keys()).sort();
      const closureDates = closures.flatMap((c) => [c.date_of_closure, c.date_of_reopening]);
      const availableDates = Array.from(
        new Set([...caseDates, ...closureDates].filter((d): d is string => Boolean(d)))
      ).sort();
      const minDate = availableDates[0] ?? null;
      const maxDate = availableDates.at(-1) ?? null;
      set({
        status: 'ready',
        error: null,
        cases,
        casesByDate,
        caseDates,
        closures,
        summaries,
        profiles,
        availableDates,
        minDate,
        maxDate,
        startDate: minDate ?? '',
        endDate: null,
      });
    },

    setStartDate: (date) => set({ startDate: clamp(date) }),
    setEndDate: (date) => set({ endDate: date === null ? null : clamp(date) }),
    setShowCases: (showCases) => set({ showCases }),
    setShowClosures: (showClosures) => set({ showClosures }),
    setShowDemographics: (showDemographics) => set({ showDemographics }),
    setShowTimeslider: (showTimeslider) => set({ showTimeslider }),
    setSelectedSchoolBoard: (selectedSchoolBoard) => set({ selectedSchoolBoard }),
    setSelectedMunicipality: (selectedMunicipality) => set({ selectedMunicipality }),
    setIsAnimating: (isAnimating) => set({ isAnimating }),
    setIsMapFullscreen: (isMapFullscreen) => set({ isMapFullscreen }),

    resetFilters: () => set({ ...DEFAULT_FILTERS, startDate: get().minDate ?? '' }),
  };
});
