import type { Metadata } from 'next';
import DataPage from '@/components/DataPage';
import ErrorBoundary from '@/components/ErrorBoundary';
import SummaryView from '@/components/summary/SummaryView';

export const metadata: Metadata = { title: 'Summary & Analytics' };

export default function SummaryPage() {
  return (
    <DataPage>
      <ErrorBoundary><SummaryView /></ErrorBoundary>
    </DataPage>
  );
}
