import { useEffect } from 'react';
import { useDashboardStore } from '@/store/dashboard-store';

// Keeps the filters in the query string of the map and summary pages, so a refresh
// or a shared link opens the same view: /summary/?date=2020-11-10&end=2020-11-20&board=...
// The filters live in the store, so they carry over when moving between the two pages.

const ISO_DATE = /^\d{4}-\d{2}-\d{2}$/;

/** Filters need the data's date range, so they're applied once it has loaded. */
function applyFilters(params: URLSearchParams) {
  const s = useDashboardStore.getState();
  const date = params.get('date');
  if (date && ISO_DATE.test(date)) s.setStartDate(date);
  const end = params.get('end');
  if (end && ISO_DATE.test(end)) s.setEndDate(end);
  const board = params.get('board');
  if (board) s.setSelectedSchoolBoard(board);
  const municipality = params.get('municipality');
  if (municipality) s.setSelectedMunicipality(municipality);
}

function buildParams(): URLSearchParams {
  const s = useDashboardStore.getState();
  const params = new URLSearchParams();
  if (s.startDate && s.startDate !== s.minDate) params.set('date', s.startDate);
  if (s.endDate && s.endDate !== s.startDate && !s.isAnimating) params.set('end', s.endDate);
  if (s.selectedSchoolBoard) params.set('board', s.selectedSchoolBoard);
  if (s.selectedMunicipality) params.set('municipality', s.selectedMunicipality);
  return params;
}

export function useUrlState() {
  const status = useDashboardStore((s) => s.status);

  useEffect(() => {
    if (status !== 'ready') return;
    applyFilters(new URLSearchParams(window.location.search));
    const sync = () => {
      const query = buildParams().toString();
      const url = `${window.location.pathname}${query ? `?${query}` : ''}${window.location.hash}`;
      if (url !== `${window.location.pathname}${window.location.search}${window.location.hash}`) {
        window.history.replaceState(null, '', url);
      }
    };
    sync();
    return useDashboardStore.subscribe(sync);
  }, [status]);
}
