import DataPage from '@/components/DataPage';
import ErrorBoundary from '@/components/ErrorBoundary';
import MapView from '@/components/map/MapView';

export default function MapPage() {
  return (
    <DataPage>
      <ErrorBoundary><MapView /></ErrorBoundary>
    </DataPage>
  );
}
