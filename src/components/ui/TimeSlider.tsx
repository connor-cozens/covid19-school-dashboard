'use client';

import { useDashboardStore } from '@/store/dashboard-store';
import { useRef, useState, useEffect, useCallback } from 'react';
import { PlayIcon, PauseIcon } from '@heroicons/react/24/solid';

/**
 * TimeSlider — point-in-time horizontal slider across availableDates.
 *
 * Maps a range input index to the availableDates array so the slider
 * skips weekends and summer gaps (only real data dates are reachable).
 * Sets startDate = endDate = selectedDate for point-in-time filtering.
 */
export default function TimeSlider() {
  const {
    availableDates,
    startDate,
    setStartDate,
    setEndDate,
  } = useDashboardStore();

  const [isPlaying, setIsPlaying] = useState(false);
  const [speed, setSpeed] = useState<'normal' | 'fast'>('normal');
  const animationRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const speedMs = speed === 'fast' ? 150 : 400;

  // Derive current slider index from startDate
  const currentDateStr = startDate.toISOString().split('T')[0];
  const currentIndex = availableDates.indexOf(currentDateStr);
  const sliderIndex = currentIndex >= 0 ? currentIndex : 0;

  const setDateByIndex = useCallback((index: number) => {
    if (availableDates.length === 0) return;
    const clamped = Math.max(0, Math.min(index, availableDates.length - 1));
    const date = new Date(availableDates[clamped]);
    setStartDate(date);
    setEndDate(date);
  }, [availableDates, setStartDate, setEndDate]);

  const stopAnimation = useCallback(() => {
    setIsPlaying(false);
    if (animationRef.current !== null) {
      clearTimeout(animationRef.current);
      animationRef.current = null;
    }
  }, []);

  const tick = useCallback((index: number) => {
    const next = index + 1;
    if (next >= availableDates.length) {
      setIsPlaying(false);
      return;
    }
    setDateByIndex(next);
    animationRef.current = setTimeout(() => tick(next), speedMs);
  }, [availableDates.length, setDateByIndex, speedMs]);

  const handlePlayPause = () => {
    if (isPlaying) {
      stopAnimation();
    } else {
      setIsPlaying(true);
      animationRef.current = setTimeout(() => tick(sliderIndex), speedMs);
    }
  };

  // Stop on unmount
  useEffect(() => {
    return () => {
      if (animationRef.current !== null) clearTimeout(animationRef.current);
    };
  }, []);

  if (availableDates.length === 0) {
    return <p className="text-sm text-gray-500">Loading dates…</p>;
  }

  const minLabel = availableDates[0];
  const maxLabel = availableDates[availableDates.length - 1];

  return (
    <div className="space-y-2">
      {/* Current date label */}
      <div className="flex items-center justify-between">
        <span className="text-xs text-gray-500">{minLabel}</span>
        <span className="text-sm font-semibold text-blue-700">{currentDateStr}</span>
        <span className="text-xs text-gray-500">{maxLabel}</span>
      </div>

      {/* Slider */}
      <input
        type="range"
        min={0}
        max={availableDates.length - 1}
        value={sliderIndex}
        onChange={(e) => {
          stopAnimation();
          setDateByIndex(Number(e.target.value));
        }}
        className="w-full accent-blue-600 cursor-pointer"
        aria-label="Timeline scrubber"
      />

      {/* Controls */}
      <div className="flex items-center gap-2">
        <button
          type="button"
          onClick={handlePlayPause}
          className="p-2 rounded-full border border-gray-300 bg-gray-50 hover:bg-blue-100 transition-colors flex items-center justify-center"
          aria-label={isPlaying ? 'Pause animation' : 'Play animation'}
        >
          {isPlaying ? (
            <PauseIcon className="h-4 w-4 text-blue-600" />
          ) : (
            <PlayIcon className="h-4 w-4 text-blue-600" />
          )}
        </button>
        <button
          type="button"
          onClick={() => setSpeed(speed === 'normal' ? 'fast' : 'normal')}
          className="px-2 py-1 rounded border border-gray-300 bg-gray-50 hover:bg-green-100 transition-colors text-xs font-semibold"
          aria-label="Toggle animation speed"
        >
          {speed === 'normal' ? 'Normal' : 'Fast'}
        </button>
        <span className="text-xs text-gray-400">
          {sliderIndex + 1} / {availableDates.length}
        </span>
      </div>
    </div>
  );
}
