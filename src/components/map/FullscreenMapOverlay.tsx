'use client';

import { useEffect } from 'react';
import dynamic from 'next/dynamic';
import { XMarkIcon } from '@heroicons/react/24/outline';
import { useDashboardStore } from '@/store/dashboard-store';
import FloatingFilterPanel from './FloatingFilterPanel';

// Dynamically import InteractiveMap with SSR disabled (same pattern as MapView)
const InteractiveMap = dynamic(() => import('./InteractiveMap'), { ssr: false });

export default function FullscreenMapOverlay() {
  const { isMapFullscreen, setIsMapFullscreen } = useDashboardStore();

  // Lock body scroll while overlay is open
  useEffect(() => {
    if (isMapFullscreen) {
      document.body.style.overflow = 'hidden';
    } else {
      document.body.style.overflow = '';
    }
    return () => {
      document.body.style.overflow = '';
    };
  }, [isMapFullscreen]);

  // Close on Escape key
  useEffect(() => {
    if (!isMapFullscreen) return;
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === 'Escape') {
        setIsMapFullscreen(false);
      }
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [isMapFullscreen, setIsMapFullscreen]);

  if (!isMapFullscreen) return null;

  return (
    <div className="fixed inset-0 z-[1000] bg-white">
      {/* Full-screen map */}
      <InteractiveMap className="h-screen w-screen" isFullscreen />

      {/* Floating filter panel */}
      <FloatingFilterPanel />

      {/* Close button */}
      <button
        onClick={() => setIsMapFullscreen(false)}
        className="fixed top-4 right-4 z-[1001] bg-white border border-gray-300 rounded p-1.5 shadow hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-blue-500"
        aria-label="Exit fullscreen"
        title="Exit fullscreen (Escape)"
      >
        <XMarkIcon className="h-5 w-5 text-gray-700" />
      </button>
    </div>
  );
}
