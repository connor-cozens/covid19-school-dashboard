// Field order for the compact files written by scripts/build_data.js. The files list
// these names alongside the data, so the app decodes them without its own copy.

const PLACE_FIELDS = ['school_number', 'school_name', 'school_board', 'latitude', 'longitude', 'city', 'municipality'];

const CASE_COUNT_FIELDS = [
  'confirmed_student_cases',
  'confirmed_staff_cases',
  'confirmed_unspecified_cases',
  'total_confirmed_cases',
];

const PROFILE_FIELDS = ['school_level', 'school_language', 'grade_range', 'enrolment'];

const DEMOGRAPHIC_FIELDS = [
  'percentage_of_students_whose_first_language_is_not_english',
  'percentage_of_students_whose_first_language_is_not_french',
  'percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country',
  'percentage_of_students_who_are_new_to_canada_from_a_non_french_speaking_country',
  'percentage_of_students_receiving_special_education_services',
  'percentage_of_students_identified_as_gifted',
  'percentage_of_school_aged_children_who_live_in_low_income_households',
  'percentage_of_students_whose_parents_have_no_degree__diploma_or_certificate',
];

module.exports = { PLACE_FIELDS, CASE_COUNT_FIELDS, PROFILE_FIELDS, DEMOGRAPHIC_FIELDS };
