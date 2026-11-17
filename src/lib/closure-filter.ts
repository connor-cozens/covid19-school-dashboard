import { SchoolClosure } from '@/types';

const ISO_DATE = /^\d{4}-\d{2}-\d{2}$/;

/**
 * Determines whether a school closure should be shown for a given date context.
 * All dates are ISO 'YYYY-MM-DD' strings, compared as strings.
 *
 * Two filter modes:
 *
 * 1. Single-day or animation mode (`isSingleDay || isAnimating`):
 *    Show if the closure was active ON the asOf date.
 *    - asOf = startDate (single day) or endDate (animation frame)
 *    - Matches the R app: `Date of Closure <= selected_date <= Date of Reopening`
 *    - During animation this makes closures appear and disappear as the frame advances.
 *
 * 2. Static range mode (!isSingleDay && !isAnimating):
 *    Show if the closure overlapped the [startDate, endDate] range at all.
 *    - Standard interval overlap: closureDate <= endDate && reopeningDate >= startDate
 *
 * A missing or invalid reopening date means the closure has no upper bound.
 */
export function closureMatchesDateRange(
  closure: SchoolClosure,
  startDate: string,
  endDate: string | null,
  isAnimating: boolean,
): boolean {
  const closureDate = closure.date_of_closure ?? '';
  if (!ISO_DATE.test(closureDate)) return false;

  const reopeningDate = ISO_DATE.test(closure.date_of_reopening ?? '') ? closure.date_of_reopening! : null;
  const isSingleDay = !endDate || startDate === endDate;

  if (isSingleDay || isAnimating) {
    const asOf = isSingleDay ? startDate : endDate!;
    return closureDate <= asOf && (reopeningDate === null || reopeningDate >= asOf);
  }

  return closureDate <= endDate! && (reopeningDate === null || reopeningDate >= startDate);
}
