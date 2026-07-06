'use client';

import dynamic from 'next/dynamic';
import { useDashboardStore } from '@/store/dashboard-store';
import FilterPanel from '../ui/FilterPanel';
import FullscreenMapOverlay from './FullscreenMapOverlay';
import { CASE_COLOR_LOW, CASE_COLOR_HIGH, CASE_COLOR_CLOSURE } from '@/lib/constants';

// Dynamically import InteractiveMap with SSR disabled
const InteractiveMap = dynamic(() => import('./InteractiveMap'), { ssr: false });

export default function MapView() {
  const { dashboardStats, startDate, endDate, isMapFullscreen } = useDashboardStore();

  return (
    <div>
      {/* Fullscreen overlay — breaks out of layout via fixed positioning */}
      <FullscreenMapOverlay />
      <div className="mb-4">
        <h2 className="text-2xl font-bold text-black">Interactive Map</h2>
        <p className="text-black">
          View COVID-19 cases and school closures across Ontario
        </p>
      </div>
      
      <FilterPanel />
      
      <div className="mb-4 p-4 bg-blue-50 rounded-lg">
        <div className="flex flex-wrap items-center justify-between text-sm">
          <div className="flex items-center gap-4">
            <span className="font-medium">Selected Date:</span>
            <span className="text-blue-700">{startDate.toISOString().slice(0, 10)}</span>
          </div>
          <div className="flex items-center gap-4">
            <span className="font-medium">Schools with Cases:</span>
            <span className="text-red-600 font-semibold">{dashboardStats.schoolsWithCases}</span>
            <span className="font-medium">School Closures:</span>
            <span className="text-purple-600 font-semibold">{dashboardStats.schoolsClosed}</span>
          </div>
        </div>
      </div>
      
      {!isMapFullscreen && <InteractiveMap className="mb-6" />}
      
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="bg-white rounded-lg shadow p-4">
          <h3 className="font-semibold text-lg mb-2">Legend</h3>
          <ul className="text-sm text-black space-y-1">
            <li className="flex items-center gap-2"><svg width="14" height="14" viewBox="0 0 14 14"><circle cx="7" cy="7" r="6" fill={CASE_COLOR_LOW} /></svg><span><span className="font-medium">Schools with 1–5 cases</span></span></li>
            <li className="flex items-center gap-2"><svg width="14" height="14" viewBox="0 0 14 14"><circle cx="7" cy="7" r="6" fill={CASE_COLOR_HIGH} /></svg><span><span className="font-medium">Schools with 6+ cases</span></span></li>
            <li className="flex items-center gap-2"><svg width="14" height="14" viewBox="0 0 14 14"><circle cx="7" cy="7" r="6" fill={CASE_COLOR_CLOSURE} /></svg><span><span className="font-medium">School closures</span></span></li>
            <li>• <span className="font-medium">Click markers</span> to see detailed information</li>
            <li>• <span className="font-medium">Use filters above</span> to change the date and view</li>
          </ul>
        </div>
        
        <div className="bg-white rounded-lg shadow p-4">
          <h3 className="font-semibold text-lg mb-2">Data Summary</h3>
          <p className="text-xs text-gray-500 mb-2">
            {endDate && endDate.getTime() !== startDate.getTime()
              ? `Data showing ${startDate.toISOString().slice(0, 10)} – ${endDate.toISOString().slice(0, 10)}`
              : `Data showing as of ${startDate.toISOString().slice(0, 10)}`}
          </p>
          <div className="text-sm text-black space-y-2">
            <p>
              <span className="font-medium">Total Cases:</span> {dashboardStats.totalCases.toLocaleString()}
            </p>
            <p>
              <span className="font-medium">Student Cases:</span> {dashboardStats.studentCases.toLocaleString()}
            </p>
            <p>
              <span className="font-medium">Staff Cases:</span> {dashboardStats.staffCases.toLocaleString()}
            </p>
            <p>
              <span className="font-medium">Schools with Cases:</span> {dashboardStats.schoolsWithCases.toLocaleString()}
            </p>
          </div>
        </div>
      </div>
    </div>
  );
} 