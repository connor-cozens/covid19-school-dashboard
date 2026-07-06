'use client';

import { useState } from 'react';
import { ChevronDownIcon, ChevronUpIcon } from '@heroicons/react/24/outline';
import FilterPanel from '../ui/FilterPanel';

export default function FloatingFilterPanel() {
  const [isExpanded, setIsExpanded] = useState(false);

  return (
    <div className="fixed top-20 left-4 z-[1001] bg-white rounded-lg shadow-xl border border-gray-200 w-80">
      {/* Header — always visible */}
      <button
        onClick={() => setIsExpanded(prev => !prev)}
        className="w-full flex items-center justify-between px-4 py-2 font-semibold text-sm text-gray-800 hover:bg-gray-50 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500"
        aria-expanded={isExpanded}
        aria-controls="floating-filter-panel-body"
      >
        <span>Filters</span>
        {isExpanded ? (
          <ChevronUpIcon className="h-4 w-4 text-gray-600" />
        ) : (
          <ChevronDownIcon className="h-4 w-4 text-gray-600" />
        )}
      </button>

      {/* Collapsible body */}
      {isExpanded && (
        <div
          id="floating-filter-panel-body"
          className="max-h-[70vh] overflow-y-auto border-t border-gray-200 px-2 py-2"
        >
          <FilterPanel compact />
        </div>
      )}
    </div>
  );
}
