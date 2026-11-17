import { useCallback, useEffect, useRef, useState } from 'react';

/**
 * Runs `step` every `intervalMs` while playing. `step` returns false to stop
 * (for example at the last date). The latest `step` is always used, so it can
 * read current props and state without restarting the timer.
 */
export function usePlayback(step: () => boolean, intervalMs: number) {
  const [isPlaying, setIsPlaying] = useState(false);
  const stepRef = useRef(step);
  useEffect(() => { stepRef.current = step; });

  useEffect(() => {
    if (!isPlaying) return;
    const id = setInterval(() => {
      if (!stepRef.current()) setIsPlaying(false);
    }, intervalMs);
    return () => clearInterval(id);
  }, [isPlaying, intervalMs]);

  const play = useCallback(() => setIsPlaying(true), []);
  const pause = useCallback(() => setIsPlaying(false), []);
  return { isPlaying, play, pause };
}
