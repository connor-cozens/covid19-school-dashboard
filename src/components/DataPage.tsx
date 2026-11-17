'use client';

import { ReactNode } from 'react';
import { useDashboardStore } from '@/store/dashboard-store';
import DataProvider from './DataProvider';
import LoadingSpinner from './ui/LoadingSpinner';

/**
 * Wraps the pages that show the data (map and summary): starts the data load,
 * shows a spinner until it's ready, and keeps the filters in the URL.
 * The content pages don't use it, so they never download the data.
 */
export default function DataPage({ children }: { children: ReactNode }) {
  const status = useDashboardStore((s) => s.status);
  return (
    <DataProvider>
      {status === 'ready' ? children : <LoadingSpinner />}
    </DataProvider>
  );
}
