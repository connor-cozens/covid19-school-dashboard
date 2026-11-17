import type { Metadata } from 'next';
import DataSourcesView from '@/components/DataSourcesView';

export const metadata: Metadata = { title: 'Data Sources' };

export default function Page() {
  return <DataSourcesView />;
}
