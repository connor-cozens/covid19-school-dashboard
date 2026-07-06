'use client';

import { useDashboardStore } from '@/store/dashboard-store';
import { MapIcon, ChartBarIcon, InformationCircleIcon, UserGroupIcon, NewspaperIcon } from '@heroicons/react/24/outline';
import clsx from 'clsx';

const navigation = [
  { name: 'Interactive Map', href: 'map', icon: MapIcon },
  { name: 'Summary & Analytics', href: 'summary', icon: ChartBarIcon },
  { name: 'Data Sources', href: 'data-sources', icon: InformationCircleIcon },
  { name: 'About', href: 'about', icon: InformationCircleIcon },
  { name: 'Media', href: 'media', icon: NewspaperIcon },
  { name: 'Team', href: 'team', icon: UserGroupIcon },
];

export default function Header() {
  const { activeTab, setActiveTab } = useDashboardStore();

  return (
    <header className="bg-white shadow-sm border-b border-gray-200">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex justify-between items-center h-16">
          <div className="flex items-center">
            <h1 className="text-xl font-semibold text-gray-900">
              COVID-19 School Dashboard
            </h1>
          </div>
          
          <nav className="flex space-x-8">
            {navigation.map((item) => (
              <button
                key={item.name}
                onClick={() => setActiveTab(item.href as 'map' | 'summary' | 'about' | 'team' | 'media' | 'data-sources')}
                className={clsx(
                  'flex items-center px-3 py-2 text-sm font-medium rounded-md transition-colors',
                  activeTab === item.href
                    ? 'bg-blue-100 text-blue-700'
                    : 'text-gray-500 hover:text-gray-700 hover:bg-gray-50'
                )}
              >
                <item.icon className="h-5 w-5 mr-2" />
                {item.name}
              </button>
            ))}
          </nav>
        </div>
      </div>
    </header>
  );
} 