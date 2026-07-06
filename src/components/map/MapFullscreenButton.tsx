'use client';

import { ArrowsPointingOutIcon, ArrowsPointingInIcon } from '@heroicons/react/24/outline';
import { useDashboardStore } from '@/store/dashboard-store';

export default function MapFullscreenButton() {
  const { isMapFullscreen, setIsMapFullscreen } = useDashboardStore();

  return (
    <button
      onClick={() => setIsMapFullscreen(!isMapFullscreen)}
      className="absolute top-4 right-16 z-[500] bg-white border border-gray-300 rounded p-1.5 shadow hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-blue-500"
      aria-label={isMapFullscreen ? 'Exit fullscreen' : 'Enter fullscreen'}
      title={isMapFullscreen ? 'Exit fullscreen' : 'Enter fullscreen'}
    >
      {isMapFullscreen ? (
        <ArrowsPointingInIcon className="h-5 w-5 text-gray-700" />
      ) : (
        <ArrowsPointingOutIcon className="h-5 w-5 text-gray-700" />
      )}
    </button>
  );
}
