'use client';

import { useDashboardStore } from '@/store/dashboard-store';
import MapView from './map/MapView';
import SummaryView from './summary/SummaryView';
import AboutView from './about/AboutView';
import TeamView from './team/TeamView';
import MediaView from './media/MediaView';
import LoadingSpinner from './ui/LoadingSpinner';
import DataSourcesView from './DataSourcesView';
import ErrorBoundary from './ErrorBoundary';

export default function Dashboard() {
  const activeTab = useDashboardStore((s) => s.activeTab);
  const status = useDashboardStore((s) => s.status);

  // Content pages don't need the data; the map and summary wait for it.
  // Load errors are shown by DataProvider.
  if (status !== 'ready' && (activeTab === 'map' || activeTab === 'summary')) {
    return <LoadingSpinner />;
  }

  const renderContent = () => {
    switch (activeTab) {
      case 'map':
        return <ErrorBoundary><MapView /></ErrorBoundary>;
      case 'summary':
        return <SummaryView />;
      case 'about':
        return <AboutView />;
      case 'team':
        return <TeamView />;
      case 'media':
        return <MediaView />;
      case 'data-sources':
        return <DataSourcesView />;
      default:
        return <ErrorBoundary><MapView /></ErrorBoundary>;
    }
  };

  return (
    <div className="w-full">
      {renderContent()}
    </div>
  );
} 