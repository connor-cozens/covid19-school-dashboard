'use client';

import { BarChart as RechartsBarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';
import { useDashboardStore } from '@/store/dashboard-store';
import { useMemo } from 'react';

interface BarChartProps {
  title: string;
  className?: string;
}

export default function BarChart({ title, className = '' }: BarChartProps) {
  const { filteredCases } = useDashboardStore();

  // Process data for the chart - group by school board
  const chartData = useMemo(() => {
  const schoolBoardData = filteredCases.reduce((acc, schoolCase) => {
    const board = schoolCase.school_board;
    if (!acc[board]) {
      acc[board] = {
        schoolBoard: board,
        totalCases: 0,
        studentCases: 0,
        staffCases: 0,
        schools: new Set(),
      };
    }
    
    acc[board].totalCases += schoolCase.total_confirmed_cases;
    acc[board].studentCases += schoolCase.confirmed_student_cases;
    acc[board].staffCases += schoolCase.confirmed_staff_cases;
    acc[board].schools.add(schoolCase.school_name);
    
    return acc;
  }, {} as Record<string, {
    schoolBoard: string;
    totalCases: number;
    studentCases: number;
    staffCases: number;
    schools: Set<string>;
  }>);

  return Object.values(schoolBoardData)
    .map((data) => ({
      schoolBoard: data.schoolBoard.length > 30 ? data.schoolBoard.substring(0, 30) + '...' : data.schoolBoard,
      fullName: data.schoolBoard,
      'Total Cases': data.totalCases,
      'Student Cases': data.studentCases,
      'Staff Cases': data.staffCases,
      'Schools Affected': data.schools.size,
    }))
    .sort((a, b) => b['Total Cases'] - a['Total Cases'])
    .slice(0, 10); // Show top 10 school boards
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
          <RechartsBarChart data={chartData} margin={{ top: 20, right: 10, left: 10, bottom: 5 }}>
            <CartesianGrid strokeDasharray="3 3" stroke="#f0f0f0" />
            <XAxis 
              dataKey="schoolBoard" 
              stroke="#6b7280"
              fontSize={11}
              angle={-45}
              textAnchor="end"
              height={80}
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
            <Legend verticalAlign="top" wrapperStyle={{ paddingBottom: '16px' }} />
            <Bar 
              dataKey="Total Cases" 
              fill="#3b82f6" 
              radius={[4, 4, 0, 0]}
            />
            <Bar 
              dataKey="Student Cases" 
              fill="#f59e0b" 
              radius={[4, 4, 0, 0]}
            />
            <Bar 
              dataKey="Staff Cases" 
              fill="#ef4444" 
              radius={[4, 4, 0, 0]}
            />
          </RechartsBarChart>
        </ResponsiveContainer>
        )}
      </div>
      
      <div className="mt-4 text-sm text-gray-600">
        <p>Showing top 10 school boards by total cases. Hover over bars to see detailed values.</p>
      </div>
    </div>
  );
} 