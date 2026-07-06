'use client';

import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';
import { useDashboardStore } from '@/store/dashboard-store';
import { format, parseISO } from 'date-fns';
import { useMemo } from 'react';

interface TimeSeriesChartProps {
  title: string;
  className?: string;
}

export default function TimeSeriesChart({ title, className = '' }: TimeSeriesChartProps) {
  const { filteredCases } = useDashboardStore();

  // Process data for the chart - create time series from filtered case data
  const chartData = useMemo(() => {
    // Group cases by date
    const casesByDate = new Map();

    // Process filtered cases to reflect active filters
    filteredCases.forEach(case_ => {
      if (typeof case_.collected_date === 'string' && case_.collected_date.trim() !== '') {
        const date = case_.collected_date;
        if (!casesByDate.has(date)) {
          casesByDate.set(date, {
            date: format(parseISO(date), 'MMM dd'),
            fullDate: date,
            'Total Cases': 0,
            'Student Cases': 0,
            'Staff Cases': 0,
            'Schools with Cases': new Set(), // Use Set to track unique schools
            'Schools Closed': 0,
          });
        }
        const dayData = casesByDate.get(date);
        dayData['Total Cases'] += case_.total_confirmed_cases || 0;
        dayData['Student Cases'] += case_.confirmed_student_cases || 0;
        dayData['Staff Cases'] += case_.confirmed_staff_cases || 0;
        if (case_.school_number) {
          dayData['Schools with Cases'].add(case_.school_number); // Add unique school number
        }
      }
    });

    // Convert to array and sort by date
    const sortedData = Array.from(casesByDate.values())
      .sort((a, b) => new Date(a.fullDate).getTime() - new Date(b.fullDate).getTime());

    // Calculate cumulative values
    let cumulativeCases = 0;
    let cumulativeSchools = 0;
    
    return sortedData.map(day => {
      cumulativeCases += day['Total Cases'];
      const uniqueSchoolsToday = day['Schools with Cases'].size;
      cumulativeSchools += uniqueSchoolsToday;
      
      return {
        date: day.date,
        fullDate: day.fullDate,
        'Total Cases': day['Total Cases'],
        'Student Cases': day['Student Cases'],
        'Staff Cases': day['Staff Cases'],
        'Schools with Cases': uniqueSchoolsToday,
        'Cumulative Cases': cumulativeCases,
        'Cumulative Schools with Cases': cumulativeSchools,
      };
    });
  }, [filteredCases]);

  return (
    <div className={`bg-white rounded-lg shadow p-6 ${className}`}>
      <h3 className="text-lg font-semibold text-gray-900 mb-4">{title}</h3>
      
      <div className="h-80">
        {chartData.length === 0 ? (
          <div className="flex items-center justify-center h-full text-gray-400 text-sm">
            No data for the selected filters
          </div>
        ) : (
        <ResponsiveContainer width="100%" height="100%">
          <LineChart data={chartData}>
            <CartesianGrid strokeDasharray="3 3" stroke="#f0f0f0" />
            <XAxis 
              dataKey="date" 
              stroke="#6b7280"
              fontSize={12}
              tickLine={false}
            />
            <YAxis 
              stroke="#6b7280"
              fontSize={12}
              tickLine={false}
              axisLine={false}
            />
            <Tooltip 
              contentStyle={{
                backgroundColor: 'white',
                border: '1px solid #e5e7eb',
                borderRadius: '8px',
                boxShadow: '0 4px 6px -1px rgba(0, 0, 0, 0.1)',
              }}
              labelStyle={{ fontWeight: 'bold' }}
            />
            <Legend />
            <Line
              type="monotone"
              dataKey="Total Cases"
              stroke="#3b82f6"
              strokeWidth={2}
              dot={chartData.length <= 60 ? { fill: '#3b82f6', strokeWidth: 2, r: 4 } : false}
              activeDot={{ r: 6 }}
            />
            <Line
              type="monotone"
              dataKey="Cumulative Cases"
              stroke="#ef4444"
              strokeWidth={2}
              dot={chartData.length <= 60 ? { fill: '#ef4444', strokeWidth: 2, r: 4 } : false}
              activeDot={{ r: 6 }}
            />
            <Line
              type="monotone"
              dataKey="Schools with Cases"
              stroke="#f59e0b"
              strokeWidth={2}
              dot={chartData.length <= 60 ? { fill: '#f59e0b', strokeWidth: 2, r: 4 } : false}
              activeDot={{ r: 6 }}
            />
          </LineChart>
        </ResponsiveContainer>
        )}
      </div>
      
      <div className="mt-4 text-sm text-gray-600">
        <p>Showing data for the selected date range. Hover over lines to see detailed values.</p>
      </div>
    </div>
  );
} 