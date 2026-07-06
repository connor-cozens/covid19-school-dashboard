'use client';

import { useState, useMemo, useEffect } from 'react';
import { useDashboardStore } from '@/store/dashboard-store';
import { SchoolCase, SchoolClosure } from '@/types';
import { MagnifyingGlassIcon } from '@heroicons/react/24/outline';

interface SchoolDataTableProps {
  className?: string;
}

export default function SchoolDataTable({ className = '' }: SchoolDataTableProps) {
  const { filteredCases, filteredClosures } = useDashboardStore();
  const [searchTerm, setSearchTerm] = useState('');
  const [page, setPage] = useState(0);
  const PAGE_SIZE = 50;

  // Combine and process data
  const combinedData = useMemo(() => {
    const cases = filteredCases.map(c => ({
      ...c,
      type: 'case' as const,
      displayName: c.school_name,
      date: c.collected_date,
    }));
    
    const closures = filteredClosures.map(c => ({
      ...c,
      type: 'closure' as const,
      displayName: c['School Name'],
      date: c['Date of Closure'],
    }));

    return [...cases, ...closures];
  }, [filteredCases, filteredClosures]);

  // Filter data based on search term
  const filteredData = useMemo(() => {
    if (!searchTerm) return combinedData;
    return combinedData.filter(item =>
      (item.displayName?.toLowerCase() || '').includes(searchTerm.toLowerCase()) ||
      (item.type === 'case' ? item.school_board : item['board name'])?.toLowerCase().includes(searchTerm.toLowerCase()) ||
      (item.type === 'case' ? item.municipality : item.city)?.toLowerCase().includes(searchTerm.toLowerCase())
    );
  }, [combinedData, searchTerm]);

  // Reset to first page whenever the filtered set changes
  useEffect(() => { setPage(0); }, [filteredData]);

  const pageCount = Math.ceil(filteredData.length / PAGE_SIZE);
  const pagedData = useMemo(
    () => filteredData.slice(page * PAGE_SIZE, (page + 1) * PAGE_SIZE),
    [filteredData, page]
  );

  const renderCaseRow = (item: SchoolCase & { type: 'case'; displayName: string | undefined; date: string | undefined }, index: number) => (
    <tr key={`case-${item.school_number ?? 'unknown'}-${item.collected_date ?? 'no-date'}-${index}`} className="border-b hover:bg-gray-50">
      <td className="px-4 py-3 text-sm">
        <div>
          <div className="font-medium text-gray-900">{item.displayName || 'Unknown School'}</div>
          <div className="text-gray-500">{item.school_board || 'Unknown Board'}</div>
        </div>
      </td>
      <td className="px-4 py-3 text-sm text-gray-900">{item.municipality}</td>
      <td className="px-4 py-3 text-sm text-center">
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
          Case
        </span>
      </td>
      <td className="px-4 py-3 text-sm text-center font-medium">{item.total_confirmed_cases}</td>
      <td className="px-4 py-3 text-sm text-center">{item.confirmed_student_cases}</td>
      <td className="px-4 py-3 text-sm text-center">{item.confirmed_staff_cases}</td>
      <td className="px-4 py-3 text-sm text-gray-500">{item.date ? new Date(item.date).toLocaleDateString() : 'N/A'}</td>
    </tr>
  );

  const renderClosureRow = (item: SchoolClosure & { type: 'closure'; displayName: string | undefined; date: string | undefined }, index: number) => (
    <tr key={`closure-${item['School Name'] ?? 'unknown'}-${item['Date of Closure'] ?? 'no-date'}-${index}`} className="border-b hover:bg-gray-50">
      <td className="px-4 py-3 text-sm">
        <div>
          <div className="font-medium text-gray-900">{item.displayName || 'Unknown School'}</div>
          <div className="text-gray-500">{item['board name'] || 'Unknown Board'}</div>
        </div>
      </td>
      <td className="px-4 py-3 text-sm text-gray-900">{item.city}</td>
      <td className="px-4 py-3 text-sm text-center">
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-purple-100 text-purple-800">
          Closure
        </span>
      </td>
      <td className="px-4 py-3 text-sm text-center">-</td>
      <td className="px-4 py-3 text-sm text-center">-</td>
      <td className="px-4 py-3 text-sm text-center">-</td>
      <td className="px-4 py-3 text-sm text-gray-500">
        {item['Date of Closure'] ? new Date(item['Date of Closure']).toLocaleDateString() : 'N/A'} - {item['Date of Reopening'] ? new Date(item['Date of Reopening']).toLocaleDateString() : 'N/A'}
      </td>
    </tr>
  );

  return (
    <div className={`bg-white rounded-lg shadow ${className}`}>
      <div className="p-6 border-b border-gray-200">
        <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
          <div>
            <h3 className="text-lg font-semibold text-gray-900">School Data</h3>
            <p className="text-sm text-gray-600">
              Detailed information about schools with COVID-19 cases and closures
            </p>
          </div>
          
          <div className="flex items-center gap-4">
            <div className="relative">
              <MagnifyingGlassIcon className="h-5 w-5 absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400" />
              <input
                type="text"
                placeholder="Search schools..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="pl-10 pr-4 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              />
            </div>
          </div>
        </div>
      </div>

      <div className="overflow-x-auto">
        <table className="min-w-full divide-y divide-gray-200">
          <thead className="bg-gray-50">
            <tr>
              <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                School
              </th>
              <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Location
              </th>
              <th className="px-4 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider">
                Type
              </th>
              <th className="px-4 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider">
                Total Cases
              </th>
              <th className="px-4 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider">
                Student Cases
              </th>
              <th className="px-4 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider">
                Staff Cases
              </th>
              <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Date
              </th>
            </tr>
          </thead>
          <tbody className="bg-white divide-y divide-gray-200">
            {pagedData.length > 0 ? (
              pagedData.map((item, idx) =>
                item.type === 'case' ? renderCaseRow(item, idx) : renderClosureRow(item, idx)
              )
            ) : (
              <tr>
                <td colSpan={7} className="px-4 py-8 text-center text-gray-500">
                  {searchTerm ? 'No schools found matching your search.' : 'No data available for the selected filters.'}
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>

      <div className="px-6 py-4 bg-gray-50 border-t border-gray-200">
        <div className="flex items-center justify-between text-sm text-gray-600">
          <span>
            Showing {page * PAGE_SIZE + 1}–{Math.min((page + 1) * PAGE_SIZE, filteredData.length)} of {filteredData.length.toLocaleString()} records
          </span>
          <div className="flex items-center gap-2">
            <button
              onClick={() => setPage(p => Math.max(0, p - 1))}
              disabled={page === 0}
              className="px-3 py-1 rounded border border-gray-300 disabled:opacity-40 hover:bg-gray-100"
            >
              Prev
            </button>
            <span>Page {page + 1} of {Math.max(1, pageCount)}</span>
            <button
              onClick={() => setPage(p => Math.min(pageCount - 1, p + 1))}
              disabled={page >= pageCount - 1}
              className="px-3 py-1 rounded border border-gray-300 disabled:opacity-40 hover:bg-gray-100"
            >
              Next
            </button>
          </div>
        </div>
      </div>
    </div>
  );
} 