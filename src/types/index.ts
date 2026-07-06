// Core data types for the COVID-19 School Dashboard

export interface SchoolSummary {
  collected_date: string;
  reported_date: string;
  current_schools_w_cases: number;
  current_schools_closed: number;
  current_total_number_schools: number;
  new_total_school_related_cases: number;
  new_school_related_student_cases: number;
  new_school_related_staff_cases: number;
  new_school_related_unspecified_cases: number;
  recent_total_school_related_cases: number;
  recent_school_related_student_cases: number;
  recent_school_related_staff_cases: number;
  recent_school_related_unspecified_cases: number;
  past_total_school_related_cases: number;
  past_school_related_student_cases: number;
  past_school_related_staff_cases: number;
  past_school_related_unspecified_cases: number;
  cumulative_school_related_cases: number;
  cumulative_school_related_student_cases: number;
  cumulative_school_related_staff_cases: number;
  cumulative_school_related_unspecified_cases: number;
}

export interface SchoolCase {
  collected_date: string;
  reported_date: string;
  school_board: string;
  school_number: number;
  school_name: string;
  municipality: string;
  confirmed_student_cases: number;
  confirmed_staff_cases: number;
  confirmed_unspecified_cases: number;
  total_confirmed_cases: number;
  latitude?: number;
  longitude?: number;
}

export interface SchoolDemographics {
  'school name': string;
  'school level': string;
  'board name': string;
  'school language': string;
  city: string;
  enrolment: number;
  'percentage of school-aged children who live in low-income households': number;
  'percentage of students whose first language is not english': number;
  'percentage of students who are new to canada from a non-english speaking country': number;
  'percentage of students whose first language is not french': number;
  'percentage of students who are new to canada from a non-french speaking country': number;
  'percentage of students receiving special education services': number;
  latitude?: number;
  longitude?: number;
}

export interface SchoolClosure {
  // New/camelCase fields
  board_number?: string;
  board_name?: string;
  school_number?: number;
  school_name?: string;
  date_of_closure?: string;
  date_of_reopening?: string;
  reason_for_closure?: string;
  // Set to true when clean_school_closures.js corrected an invalid reopening date.
  // original_reopening_date holds the original value from the source Excel file.
  reopening_date_corrected?: boolean;
  original_reopening_date?: string;
  city?: string;
  demographics?: Record<string, string | number>;
  latitude?: number;
  longitude?: number;
  // Legacy fields (for backward compatibility)
  'School Name'?: string;
  'Date of Closure'?: string;
  'Date of Reopening'?: string;
  'Reason for Closure'?: string;
  'board name'?: string;
  'school level'?: string;
  'school language'?: string;
  enrolment?: number;
  'percentage of school-aged children who live in low-income households'?: number;
  'percentage of students whose first language is not english'?: number;
  'percentage of students who are new to canada from a non-english speaking country'?: number;
  'percentage of students whose first language is not french'?: number;
  'percentage of students who are new to canada from a non-french speaking country'?: number;
  'percentage of students receiving special education services'?: number;
}

export interface MapMarker {
  id: string;
  type: 'case' | 'closure';
  latitude: number;
  longitude: number;
  schoolName: string;
  data: SchoolCase | SchoolClosure;
  popupContent: string;
}

export interface TimeRange {
  start: Date;
  end: Date;
}

export interface FilterOptions {
  showCases: boolean;
  showClosures: boolean;
  showDemographics: boolean;
  selectedDate: Date;
  selectedSchoolBoard?: string;
  selectedMunicipality?: string;
}

export interface DashboardStats {
  totalSchools: number | null;       // null when summary CSV cannot produce a valid count
  schoolsWithCases: number;
  schoolsClosed: number;
  totalCases: number;
  studentCases: number;
  staffCases: number;
  percentageSchoolsWithCases: number | null;  // null when totalSchools is null
  percentageSchoolsClosed: number | null;     // null when totalSchools is null
}

export interface ChartDataPoint {
  date: string;
  value: number;
  label: string;
}

export interface WeeklySummary {
  timeframe: '7-day' | '14-day';
  startDate: string;
  endDate: string;
  newCases: number;
  newSchoolsWithCases: number;
  newSchoolsClosed: number;
  cumulativeCases: number;
  cumulativeSchoolsWithCases: number;
  cumulativeSchoolsClosed: number;
} 