'use client';

import { useState } from 'react';
import { useDashboardStore } from '@/store/dashboard-store';
import { usePlayback } from '@/hooks/usePlayback';
import { PlayIcon, PauseIcon } from '@heroicons/react/24/solid';

/**
 * TimeSlider — point-in-time horizontal slider across availableDates.
 *
 * Maps a range input index to the availableDates array so the slider
 * skips weekends and summer gaps (only real data dates are reachable).
 * Sets startDate (with no endDate) for point-in-time filtering.
 */
export default function TimeSlider() {
  const availableDates = useDashboardStore((s) => s.availableDates);
  const startDate = useDashboardStore((s) => s.startDate);
  const setStartDate = useDashboardStore((s) => s.setStartDate);
  const setEndDate = useDashboardStore((s) => s.setEndDate);

  const [speed, setSpeed] = useState<'normal' | 'fast'>('normal');

  const currentDateStr = startDate;
  const currentIndex = availableDates.indexOf(startDate);
  const sliderIndex = currentIndex >= 0 ? currentIndex : 0;

  const setDateByIndex = (index: number) => {
    if (availableDates.length === 0) return;
    const date = availableDates[Math.max(0, Math.min(index, availableDates.length - 1))];
    setStartDate(date);
    setEndDate(null);
  };

  const playback = usePlayback(() => {
    const { availableDates: dates, startDate: current } = useDashboardStore.getState();
    const next = dates.indexOf(current) + 1;
    if (next <= 0 || next >= dates.length) return false;
    setStartDate(dates[next]);
    setEndDate(null);
    return true;
  }, speed === 'fast' ? 150 : 400);
  const isPlaying = playback.isPlaying;
  const stopAnimation = playback.pause;
  const handlePlayPause = () => (isPlaying ? playback.pause() : playback.play());

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
