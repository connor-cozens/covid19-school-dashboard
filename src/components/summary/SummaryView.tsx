'use client';

import { useDashboardStore } from '@/store/dashboard-store';
import FilterPanel from '../ui/FilterPanel';
import TimeSeriesChart from '../charts/TimeSeriesChart';
import BarChart from '../charts/BarChart';
import PieChart from '../charts/PieChart';
import SchoolDataTable from '../tables/SchoolDataTable';

export default function SummaryView() {
  const { dashboardStats } = useDashboardStore();

  return (
    <div>
      <div className="mb-4">
        <h2 className="text-2xl font-bold text-gray-900">Summary & Analytics</h2>
        <p className="text-gray-600">
          Key statistics and trends for COVID-19 in Ontario schools. Use the date filters above to explore different time periods.
        </p>
      </div>
      
      <FilterPanel />
      
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
        <div className="bg-blue-50 p-4 rounded-lg">
          <h3 className="text-sm font-medium text-blue-600">Total Cases</h3>
          <p className="text-2xl font-bold text-blue-900">{dashboardStats.totalCases.toLocaleString()}</p>
        </div>
        <div className="bg-red-50 p-4 rounded-lg">
          <h3 className="text-sm font-medium text-red-600">Student Cases</h3>
          <p className="text-2xl font-bold text-red-900">{dashboardStats.studentCases.toLocaleString()}</p>
        </div>
        <div className="bg-yellow-50 p-4 rounded-lg">
          <h3 className="text-sm font-medium text-yellow-600">Staff Cases</h3>
          <p className="text-2xl font-bold text-yellow-900">{dashboardStats.staffCases.toLocaleString()}</p>
        </div>
        <div className="bg-green-50 p-4 rounded-lg">
          <h3 className="text-sm font-medium text-green-600">Schools with Cases</h3>
          <p className="text-2xl font-bold text-green-900">{dashboardStats.schoolsWithCases.toLocaleString()}</p>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
        <div className="bg-purple-50 p-4 rounded-lg">
          <h3 className="text-sm font-medium text-purple-600">Schools Closed</h3>
          <p className="text-2xl font-bold text-purple-900">{dashboardStats.schoolsClosed.toLocaleString()}</p>
        </div>
        <div className="bg-orange-50 p-4 rounded-lg">
          <h3 className="text-sm font-medium text-orange-600">% Schools with Cases</h3>
          <p className="text-2xl font-bold text-orange-900">{dashboardStats.percentageSchoolsWithCases !== null ? `${dashboardStats.percentageSchoolsWithCases.toFixed(1)}%` : 'Data unavailable'}</p>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
        <TimeSeriesChart title="COVID-19 Trends Over Time" />
        <BarChart title="Cases by School Board" />
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
        <PieChart title="Case Distribution by Type" />
        <div className="bg-white rounded-lg shadow p-6">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Quick Statistics</h3>
          <div className="space-y-4">
            <div className="flex justify-between items-center p-3 bg-gray-50 rounded">
              <span className="font-medium">Schools with Cases:</span>
              <span className="text-lg font-bold text-red-600">{dashboardStats.schoolsWithCases.toLocaleString()}</span>
            </div>
            <div className="flex justify-between items-center p-3 bg-gray-50 rounded">
              <span className="font-medium">School Closures:</span>
              <span className="text-lg font-bold text-purple-600">{dashboardStats.schoolsClosed.toLocaleString()}</span>
            </div>
            <div className="flex justify-between items-center p-3 bg-gray-50 rounded">
              <span className="font-medium">Avg Cases per Affected School:</span>
              <span className="text-lg font-bold text-blue-600">
                {dashboardStats.schoolsWithCases > 0 ? (dashboardStats.totalCases / dashboardStats.schoolsWithCases).toFixed(1) : 0}
              </span>
            </div>
            <div className="flex justify-between items-center p-3 bg-gray-50 rounded">
              <span className="font-medium">% Schools Closed:</span>
              <span className="text-lg font-bold text-green-600">{dashboardStats.percentageSchoolsClosed !== null ? `${dashboardStats.percentageSchoolsClosed.toFixed(1)}%` : 'Data unavailable'}</span>
            </div>
          </div>
        </div>
      </div>
      
      <SchoolDataTable className="mb-6" />
    </div>
  );
} 