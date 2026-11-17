// Core data types for the COVID-19 School Dashboard.
// Dates are ISO calendar dates ('YYYY-MM-DD') everywhere: they compare correctly as
// strings and have no timezone.

/** One row of the province-wide daily summary (public/data/summaries.json). */
export interface SchoolSummary {
  collected_date: string;
  reported_date: string;
  current_schools_w_cases: number | null;
  current_schools_closed: number | null;
  current_total_number_schools: number | null;
  cumulative_school_related_cases: number | null;
  cumulative_school_related_student_cases: number | null;
  cumulative_school_related_staff_cases: number | null;
  cumulative_school_related_unspecified_cases: number | null;
  [column: string]: string | number | null;
}

/** One school's active-case snapshot on one day (decoded from public/data/cases.json). */
export interface SchoolCase {
  collected_date: string;
  school_number: number | null;
  school_name: string;
  school_board: string;
  latitude: number | null;
  longitude: number | null;
  /** From the demographics join; shown for display only. */
  city: string | null;
  /** Ontario government administrative region; drives the municipality filter. */
  municipality: string;
  confirmed_student_cases: number;
  confirmed_staff_cases: number;
  confirmed_unspecified_cases: number | null;
  total_confirmed_cases: number;
}

/** One school closure (public/data/school_closures.json, built by clean_school_closures.js). */
export interface SchoolClosure {
  board_number?: string;
  board_name?: string;
  school_number?: number;
  school_name?: string;
  date_of_closure?: string;
  date_of_reopening?: string;
  reason_for_closure?: string;
  // Set when clean_school_closures.js corrected an invalid reopening date;
  // original_reopening_date holds the value from the source spreadsheet.
  reopening_date_corrected?: boolean;
  original_reopening_date?: string;
  city?: string;
  latitude?: number;
  longitude?: number;
  has_demographics?: boolean;
}

export interface SchoolDemographics {
  percentage_of_students_whose_first_language_is_not_english: number | null;
  percentage_of_students_whose_first_language_is_not_french: number | null;
  percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country: number | null;
  percentage_of_students_who_are_new_to_canada_from_a_non_french_speaking_country: number | null;
  percentage_of_students_receiving_special_education_services: number | null;
  percentage_of_students_identified_as_gifted: number | null;
  percentage_of_school_aged_children_who_live_in_low_income_households: number | null;
  percentage_of_students_whose_parents_have_no_degree__diploma_or_certificate: number | null;
}

/** School details shown in map popups (decoded from public/data/school_profiles.json). */
export interface SchoolProfile {
  school_level: string | null;
  school_language: string | null;
  grade_range: string | null;
  enrolment: number | null;
  demographics: SchoolDemographics;
}

export type SchoolProfiles = Record<number, SchoolProfile>;

export interface DashboardStats {
  totalSchools: number | null;       // null when the summaries cannot produce a valid count
  schoolsWithCases: number;
  schoolsClosed: number;
  totalCases: number;
  studentCases: number;
  staffCases: number;
  percentageSchoolsWithCases: number | null;  // null when totalSchools is null
  percentageSchoolsClosed: number | null;     // null when totalSchools is null
}
