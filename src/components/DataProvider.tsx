'use client';

import { useEffect } from 'react';
import { useDashboardStore } from '@/store/dashboard-store';
import { useUrlState } from '@/hooks/useUrlState';

export default function DataProvider({ children }: { children: React.ReactNode }) {
  const status = useDashboardStore((s) => s.status);
  const error = useDashboardStore((s) => s.error);
  const loadData = useDashboardStore((s) => s.loadData);

  useEffect(() => {
    if (useDashboardStore.getState().status === 'idle') loadData();
  }, [loadData]);

  useUrlState();

  if (status === 'error') {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50">
        <div className="bg-white rounded-lg shadow-lg p-8 max-w-lg w-full mx-4">
          <h2 className="text-xl font-bold text-red-800 mb-2">Failed to load dashboard data</h2>
          <p className="text-red-600 text-sm mb-4">{error}</p>
          <p className="text-gray-500 text-xs mb-6">
            This may be a temporary network issue. Check the browser console for details.
          </p>
          <button
            onClick={() => loadData()}
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
