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
  const { activeTab, isLoading, error } = useDashboardStore();

  if (isLoading) {
    return <LoadingSpinner />;
  }

  if (error) {
    return (
      <div className="bg-red-50 border border-red-200 rounded-lg p-6">
        <h3 className="text-lg font-medium text-red-800">Error Loading Data</h3>
        <p className="text-red-600 mt-2">{error}</p>
      </div>
    );
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