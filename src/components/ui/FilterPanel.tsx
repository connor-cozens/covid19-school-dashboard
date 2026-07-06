'use client';

import { useDashboardStore } from '@/store/dashboard-store';
import { CalendarIcon, FunnelIcon } from '@heroicons/react/24/outline';
import { useRef, useState } from 'react';
import { PlayIcon, PauseIcon } from '@heroicons/react/24/solid';
import React from 'react';
import { useFilterOptions } from '@/hooks/useFilterOptions';
import TimeSlider from './TimeSlider';

export default function FilterPanel({ compact = false }: { compact?: boolean }) {
  const {
    startDate,
    endDate,
    setStartDate,
    setEndDate,
    setIsAnimating,
    showCases,
    setShowCases,
    showClosures,
    setShowClosures,
    selectedSchoolBoard,
    setSelectedSchoolBoard,
    selectedMunicipality,
    setSelectedMunicipality,
    resetFilters,
    minDate,
    maxDate,
    showDemographics,
    setShowDemographics,
    showTimeslider,
    setShowTimeslider,
  } = useDashboardStore();

  const { schoolBoards, municipalities, isLoading: filterOptionsLoading, error: filterOptionsError } = useFilterOptions();

  // Animation state (only used in date-input mode)
  const [isPlaying, setIsPlaying] = useState(false);
  const [speed, setSpeed] = useState<'normal' | 'fast'>('normal');
  const animationRef = useRef<number | null>(null);

  const speedMs = speed === 'fast' ? 100 : 300;

  const addDays = (date: Date, days: number) => {
    const result = new Date(date);
    result.setDate(result.getDate() + days);
    return result;
  };

  const handlePlayPause = () => {
    if (isPlaying) {
      setIsPlaying(false);
      setIsAnimating(false);
      if (animationRef.current) cancelAnimationFrame(animationRef.current);
    } else {
      setIsPlaying(true);
      setIsAnimating(true);
      let current = startDate;
      let lastFrameTime = 0;
      setEndDate(current);

      const tick = (timestamp: number) => {
        if (timestamp - lastFrameTime >= speedMs) {
          const next = addDays(current, 1);
          if (maxDate && next > new Date(maxDate)) {
            setIsPlaying(false);
            setIsAnimating(false);
            return;
          }
          setEndDate(next);
          current = next;
          lastFrameTime = timestamp;
        }
        animationRef.current = requestAnimationFrame(tick);
      };

      animationRef.current = requestAnimationFrame(tick);
    }
  };

  const handleReset = () => {
    setIsPlaying(false);
    setIsAnimating(false);
    if (animationRef.current) cancelAnimationFrame(animationRef.current);
    setEndDate(startDate);
  };

  const handleToggleTimeslider = (checked: boolean) => {
    // Cancel any running date-input animation before switching modes
    if (isPlaying) {
      setIsPlaying(false);
      if (animationRef.current) cancelAnimationFrame(animationRef.current);
    }
    setShowTimeslider(checked);
    if (!checked) {
      // Restore point-in-time: endDate = startDate
      setEndDate(startDate);
    }
  };

  React.useEffect(() => {
    return () => {
      if (animationRef.current) cancelAnimationFrame(animationRef.current);
    };
  }, []);

  return (
    <div className="bg-white rounded-lg shadow p-4 mb-6">
      <div className="flex items-center justify-between mb-4">
        <h3 className="text-lg font-medium text-gray-900 flex items-center">
          <FunnelIcon className="h-5 w-5 mr-2" />
          Filters
        </h3>
        <button
          onClick={resetFilters}
          className="text-sm text-blue-600 hover:text-blue-800"
        >
          Reset All
        </button>
      </div>

      {filterOptionsError && (
        <div className="mb-4 p-3 bg-red-50 border border-red-200 rounded-md">
          <p className="text-sm text-red-600">
            Error loading filter options: {filterOptionsError}
          </p>
        </div>
      )}

      <div className={compact ? 'grid grid-cols-1 gap-4' : 'grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4'}>
        {/* Date Controls */}
        <div>
          {showTimeslider ? (
            <>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                <CalendarIcon className="h-4 w-4 inline mr-1" />
                Timeline
              </label>
              <TimeSlider />
            </>
          ) : (
            <>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                <CalendarIcon className="h-4 w-4 inline mr-1" />
                Start Date
              </label>
              <div className="flex flex-wrap items-center gap-2">
                <input
                  type="date"
                  value={startDate.toISOString().split('T')[0]}
                  onChange={(e) => { const d = new Date(e.target.value); if (!isNaN(d.getTime())) setStartDate(d); }}
                  min={minDate || undefined}
                  max={endDate ? endDate.toISOString().split('T')[0] : maxDate || undefined}
                  className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
                  disabled={isPlaying}
                />
                <button
                  type="button"
                  onClick={handlePlayPause}
                  className="p-2 rounded-full border border-gray-300 bg-gray-50 hover:bg-blue-100 transition-colors flex items-center justify-center"
                  aria-label={isPlaying ? 'Pause animation' : 'Play animation'}
                >
                  {isPlaying ? (
                    <PauseIcon className="h-5 w-5 text-blue-600" />
                  ) : (
                    <PlayIcon className="h-5 w-5 text-blue-600" />
                  )}
                </button>
                <button
                  type="button"
                  onClick={handleReset}
                  className="p-2 rounded-full border border-gray-300 bg-gray-50 hover:bg-red-100 transition-colors flex items-center justify-center"
                  aria-label="Reset animation"
                  disabled={isPlaying}
                >
                  <svg className="h-5 w-5 text-red-600" fill="none" stroke="currentColor" strokeWidth="2" viewBox="0 0 24 24"><path strokeLinecap="round" strokeLinejoin="round" d="M4 4v5h.582M19.418 19A9 9 0 104.582 5" /></svg>
                </button>
                <button
                  type="button"
                  onClick={() => setSpeed(speed === 'normal' ? 'fast' : 'normal')}
                  className="p-2 rounded-full border border-gray-300 bg-gray-50 hover:bg-green-100 transition-colors flex items-center justify-center text-xs font-semibold"
                  aria-label="Toggle animation speed"
                >
                  {speed === 'normal' ? 'Normal' : 'Fast'}
                </button>
              </div>
              <label className="block text-sm font-medium text-gray-700 mb-1 mt-2">
                <CalendarIcon className="h-4 w-4 inline mr-1" />
                End Date
              </label>
              <input
                type="date"
                value={endDate ? endDate.toISOString().split('T')[0] : ''}
                onChange={(e) => { const d = new Date(e.target.value); if (!isNaN(d.getTime())) setEndDate(d); }}
                min={startDate ? startDate.toISOString().split('T')[0] : minDate || undefined}
                max={maxDate || undefined}
                className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
                disabled={isPlaying}
              />
            </>
          )}
        </div>

        {/* School Board Filter */}
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            School Board
          </label>
          <select
            value={selectedSchoolBoard || ''}
            onChange={(e) => setSelectedSchoolBoard(e.target.value || undefined)}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            disabled={filterOptionsLoading}
          >
            <option value="">
              {filterOptionsLoading ? 'Loading...' : 'All School Boards'}
            </option>
            {schoolBoards.map((board) => (
              <option key={board} value={board}>{board}</option>
            ))}
          </select>
        </div>

        {/* Municipality Filter */}
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            Municipality
          </label>
          <select
            value={selectedMunicipality || ''}
            onChange={(e) => setSelectedMunicipality(e.target.value || undefined)}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            disabled={filterOptionsLoading}
          >
            <option value="">
              {filterOptionsLoading ? 'Loading...' : 'All Municipalities'}
            </option>
            {municipalities.map((municipality) => (
              <option key={municipality} value={municipality}>{municipality}</option>
            ))}
          </select>
          {selectedMunicipality && showClosures && (
            <p className="mt-1 text-xs text-amber-700 bg-amber-50 border border-amber-200 rounded px-2 py-1">
              Some closure records have no location data and won&apos;t appear in this filter.
            </p>
          )}
        </div>

        {/* Data Type & View Options */}
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            Data Types & View Options
          </label>
          <div className="space-y-2">
            <label className="flex items-center">
              <input
                type="checkbox"
                checked={showCases}
                onChange={(e) => setShowCases(e.target.checked)}
                className="mr-2"
              />
              <span className="text-sm">Show Cases</span>
            </label>
            <label className="flex items-center">
              <input
                type="checkbox"
                checked={showClosures}
                onChange={(e) => setShowClosures(e.target.checked)}
                className="mr-2"
              />
              <span className="text-sm">Show School Closures</span>
            </label>
            <label className="flex items-center">
              <input
                type="checkbox"
                checked={showDemographics}
                onChange={(e) => setShowDemographics(e.target.checked)}
                className="mr-2"
              />
              <span className="text-sm">Show Demographics</span>
            </label>
            <label className="flex items-center">
              <input
                type="checkbox"
                checked={showTimeslider}
                onChange={(e) => handleToggleTimeslider(e.target.checked)}
                className="mr-2"
              />
              <span className="text-sm">Show Timeslider</span>
            </label>
          </div>
        </div>
      </div>
    </div>
  );
}
