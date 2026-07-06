'use client';

import { useEffect, useState } from 'react';
import { useDashboardStore } from '@/store/dashboard-store';
import { loadSchoolData, loadCombinedSchoolCases } from '@/lib/csv-loader';

export default function DataProvider({ children }: { children: React.ReactNode }) {
  const { setData, setLoading, setError, setSchoolCasesWithDemographics, error } = useDashboardStore();
  const [retryCount, setRetryCount] = useState(0);

  useEffect(() => {
    const loadData = async () => {
      try {
        setLoading(true);
        setError(null);
        const [data, combinedCases] = await Promise.all([
          loadSchoolData(),
          loadCombinedSchoolCases()
        ]);
        // ORDER MATTERS: setData must run before setSchoolCasesWithDemographics so that
        // schoolClosures is populated in the store. We pass data.schoolClosures explicitly
        // as the second argument so setSchoolCasesWithDemographics can merge closure dates
        // into availableDates without reading from a potentially-empty store slice.
        setData(data);
        setSchoolCasesWithDemographics(combinedCases, data.schoolClosures);
      } catch (err) {
        setError(err instanceof Error ? err.message : String(err));
      } finally {
        setLoading(false);
      }
    };
    loadData();
  }, [setData, setLoading, setError, setSchoolCasesWithDemographics, retryCount]);

  // Render a retry button when there's a load error
  // (Dashboard.tsx renders the full error UI, but we expose a retry trigger via a hidden button
  //  that Dashboard can detect via the store error state + this retry mechanism)
  // Actually: render the error UI here since DataProvider wraps everything
  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50">
        <div className="bg-white rounded-lg shadow-lg p-8 max-w-lg w-full mx-4">
          <h2 className="text-xl font-bold text-red-800 mb-2">Failed to load dashboard data</h2>
          <p className="text-red-600 text-sm mb-4">{error}</p>
          <p className="text-gray-500 text-xs mb-6">
            This may be a temporary network issue. Check the browser console for details.
          </p>
          <button
            onClick={() => setRetryCount(c => c + 1)}
            className="w-full bg-blue-600 text-white py-2 px-4 rounded hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-blue-500"
          >
            Retry
          </button>
        </div>
      </div>
    );
  }

  return <>{children}</>;
}
