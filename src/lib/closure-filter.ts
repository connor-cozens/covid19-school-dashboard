import { SchoolClosure } from '@/types';
import { getClosureDate, getReopeningDate } from '@/lib/closure-fields';

/**
 * Determines whether a school closure should be shown for a given date context.
 *
 * Two filter modes:
 *
 * 1. Single-day or animation mode (`isSingleDay || isAnimating`):
 *    Show if the closure was active ON the asOf date.
 *    - asOf = startDate (single day) or endDate (animation frame)
 *    - Matches R app behaviour: `Date of Closure <= selected_date <= Date of Reopening`
 *    - During animation this makes closures appear and disappear as the frame advances.
 *
 * 2. Static range mode (!isSingleDay && !isAnimating):
 *    Show if the closure overlapped the [startDate, endDate] range at all.
 *    - Standard interval overlap: closureDate <= endDate && reopeningDate >= startDate
 *    - A closure disappears only when BOTH its dates are before startDate.
 */
export function closureMatchesDateRange(
  closure: SchoolClosure,
  startDate: Date,
  endDate: Date | null,
  isAnimating: boolean,
): boolean {
  const closureDateStr = getClosureDate(closure);
  const reopeningDateStr = getReopeningDate(closure);

  if (typeof closureDateStr !== 'string' || closureDateStr.trim() === '') return false;

  const closureDate = new Date(closureDateStr);
  const reopeningDate = new Date(reopeningDateStr);

  if (isNaN(closureDate.getTime())) return false;

  const isSingleDay = !endDate || startDate.getTime() === endDate.getTime();

  if (isSingleDay || isAnimating) {
    const asOfDate = isSingleDay ? startDate : endDate!;
    return (
      closureDate <= asOfDate &&
      (isNaN(reopeningDate.getTime()) || reopeningDate >= asOfDate)
    );
  }

  // Static range: interval overlap
  return (
    closureDate <= endDate! &&
    (isNaN(reopeningDate.getTime()) || reopeningDate >= startDate)
  );
}
