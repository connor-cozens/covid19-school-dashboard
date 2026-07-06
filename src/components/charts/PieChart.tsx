'use client';

import { PieChart as RechartsPieChart, Pie, Cell, ResponsiveContainer, Tooltip, Legend } from 'recharts';
import { useDashboardStore } from '@/store/dashboard-store';

interface PieChartProps {
  title: string;
  className?: string;
}

export default function PieChart({ title, className = '' }: PieChartProps) {
  const { filteredCases } = useDashboardStore();

  // Calculate totals
  const totalStudentCases = filteredCases.reduce((sum, c) => sum + c.confirmed_student_cases, 0);
  const totalStaffCases = filteredCases.reduce((sum, c) => sum + c.confirmed_staff_cases, 0);
  const totalUnspecifiedCases = filteredCases.reduce((sum, c) => sum + c.confirmed_unspecified_cases, 0);

  const data = [
    { name: 'Student Cases', value: totalStudentCases, color: '#3b82f6' },
    { name: 'Staff Cases', value: totalStaffCases, color: '#ef4444' },
    { name: 'Unspecified Cases', value: totalUnspecifiedCases, color: '#6b7280' },
  ].filter(item => item.value > 0); // Only show categories with data

  const COLORS = data.map(item => item.color);

  return (
    <div className={`bg-white rounded-lg shadow p-6 ${className}`}>
      <h3 className="text-lg font-semibold text-gray-900 mb-4">{title}</h3>
      
      <div className="h-80">
        {data.length === 0 ? (
          <div className="flex items-center justify-center h-full text-gray-400 text-sm">
            No data for the selected filters
          </div>
        ) : (
        <ResponsiveContainer width="100%" height="100%">
          <RechartsPieChart>
            <Pie
              data={data}
              cx="50%"
              cy="50%"
              labelLine={false}
              label={({ name, percent }) => `${name} ${((percent || 0) * 100).toFixed(0)}%`}
              outerRadius={80}
              fill="#8884d8"
              dataKey="value"
            >
              {data.map((entry, index) => (
                <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
              ))}
            </Pie>
            <Tooltip 
              contentStyle={{
                backgroundColor: 'white',
                border: '1px solid #e5e7eb',
                borderRadius: '8px',
                boxShadow: '0 4px 6px -1px rgba(0, 0, 0, 0.1)',
              }}
              formatter={(value: number) => [value, 'Cases']}
            />
            <Legend />
          </RechartsPieChart>
        </ResponsiveContainer>
        )}
      </div>
      
      <div className="mt-4 text-sm text-gray-600">
        <p>Distribution of COVID-19 cases by type for the selected date range.</p>
      </div>
    </div>
  );
} 