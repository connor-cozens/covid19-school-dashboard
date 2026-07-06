import { useState, useEffect } from 'react';

interface FilterOptions {
  schoolBoards: string[];
  municipalities: string[];
  isLoading: boolean;
  error: string | null;
}

export function useFilterOptions(): FilterOptions {
  const [schoolBoards, setSchoolBoards] = useState<string[]>([]);
  const [municipalities, setMunicipalities] = useState<string[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const loadFilterOptions = async () => {
      try {
        setIsLoading(true);
        setError(null);

        // Load both files in parallel
        const [schoolBoardsResponse, municipalitiesResponse] = await Promise.all([
          fetch('/data/school_boards.json'),
          fetch('/data/municipalities.json')
        ]);

        if (!schoolBoardsResponse.ok) {
          throw new Error(`Failed to load school boards: ${schoolBoardsResponse.statusText}`);
        }

        if (!municipalitiesResponse.ok) {
          throw new Error(`Failed to load municipalities: ${municipalitiesResponse.statusText}`);
        }

        const [schoolBoardsData, municipalitiesData] = await Promise.all([
          schoolBoardsResponse.json(),
          municipalitiesResponse.json()
        ]);

        setSchoolBoards(schoolBoardsData);
        setMunicipalities(municipalitiesData);
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Failed to load filter options');
        console.error('Error loading filter options:', err);
      } finally {
        setIsLoading(false);
      }
    };

    loadFilterOptions();
  }, []);

  return {
    schoolBoards,
    municipalities,
    isLoading,
    error
  };
} 