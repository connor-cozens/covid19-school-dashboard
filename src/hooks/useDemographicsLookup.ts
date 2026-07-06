import { useState, useEffect, useCallback } from 'react';

export interface DemographicsData {
  school_number: number;
  school_name: string;
  school_id: string;
  board_number: string;
  board_name: string;
  board_type: string;
  latitude: number;
  longitude: number;
  city: string;
  province: string;
  postal_code: string;
  street: string;
  municipality: string;
  school_type: string;
  school_level: string;
  school_language: string;
  grade_range: string;
  school_special_condition_code: string;
  enrolment: number;
  demographics: {
    percentage_of_students_whose_first_language_is_not_english: number;
    percentage_of_students_whose_first_language_is_not_french: number;
    percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country: number;
    percentage_of_students_who_are_new_to_canada_from_a_non_french_speaking_country: number;
    percentage_of_students_receiving_special_education_services: number;
    percentage_of_students_identified_as_gifted: number;
    percentage_of_school_aged_children_who_live_in_low_income_households: number;
    percentage_of_students_whose_parents_have_no_degree__diploma_or_certificate: number;
  };
}

interface DemographicsLookup {
  [schoolNumber: number]: DemographicsData;
}

export function useDemographicsLookup() {
  const [demographicsLookup, setDemographicsLookup] = useState<DemographicsLookup>({});
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const loadDemographics = async () => {
      try {
        setLoading(true);
        const response = await fetch('/data/school_demographics_lookup.json');
        if (!response.ok) {
          throw new Error(`Failed to load demographics: ${response.statusText}`);
        }
        const data = await response.json();
        setDemographicsLookup(data);
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Failed to load demographics data');
        console.error('Error loading demographics lookup:', err);
      } finally {
        setLoading(false);
      }
    };

    loadDemographics();
  }, []);

  // useCallback so the function reference is stable — prevents aggregatedCases useMemo
  // in InteractiveMap from recalculating every time a parent re-renders.
  const getDemographics = useCallback(
    (schoolNumber: number): DemographicsData | null => demographicsLookup[schoolNumber] ?? null,
    [demographicsLookup]
  );

  return {
    demographicsLookup,
    getDemographics,
    loading,
    error,
  };
} 