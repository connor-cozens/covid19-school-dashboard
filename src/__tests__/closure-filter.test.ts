import { closureMatchesDateRange } from '@/lib/closure-filter';
import { SchoolClosure } from '@/types';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Build a minimal SchoolClosure using camelCase fields. */
function makeClosure(closureDate: string, reopeningDate: string): SchoolClosure {
  return { date_of_closure: closureDate, date_of_reopening: reopeningDate };
}

/** Build a SchoolClosure using legacy Title-Case field names. */
function makeClosureLegacy(closureDate: string, reopeningDate: string): SchoolClosure {
  return { 'Date of Closure': closureDate, 'Date of Reopening': reopeningDate };
}

const d = (s: string) => new Date(s);

// ---------------------------------------------------------------------------
// Single-day mode (endDate === null or startDate === endDate)
// ---------------------------------------------------------------------------

describe('single-day mode (endDate null)', () => {
  const closure = makeClosure('2020-09-16', '2020-09-30');

  test('shows closure on its first day', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-16'), null, false)).toBe(true);
  });

  test('shows closure mid-period', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-20'), null, false)).toBe(true);
  });

  test('shows closure on its last day (reopening date)', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-30'), null, false)).toBe(true);
  });

  test('hides closure before it starts', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-15'), null, false)).toBe(false);
  });

  test('hides closure after it has reopened', () => {
    expect(closureMatchesDateRange(closure, d('2020-10-01'), null, false)).toBe(false);
  });
});

describe('single-day mode (startDate === endDate)', () => {
  const closure = makeClosure('2020-09-16', '2020-09-30');

  test('shows closure when start equals end and date is within closure', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-20'), d('2020-09-20'), false)).toBe(true);
  });

  test('hides closure when start equals end and date is after reopening', () => {
    expect(closureMatchesDateRange(closure, d('2020-10-01'), d('2020-10-01'), false)).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// Static range mode (!isAnimating, startDate !== endDate)
// ---------------------------------------------------------------------------

describe('static range mode — interval overlap', () => {
  // Closure: 2020-09-16 → 2020-09-30
  const closure = makeClosure('2020-09-16', '2020-09-30');

  test('shows closure when range fully contains it', () => {
    // [Sep 10 → Nov 10] contains [Sep 16 → Sep 30]
    expect(closureMatchesDateRange(closure, d('2020-09-10'), d('2020-11-10'), false)).toBe(true);
  });

  test('shows closure when range starts during it', () => {
    // [Sep 20 → Nov 10] overlaps [Sep 16 → Sep 30]
    expect(closureMatchesDateRange(closure, d('2020-09-20'), d('2020-11-10'), false)).toBe(true);
  });

  test('shows closure when range ends during it', () => {
    // [Sep 10 → Sep 20] overlaps [Sep 16 → Sep 30]
    expect(closureMatchesDateRange(closure, d('2020-09-10'), d('2020-09-20'), false)).toBe(true);
  });

  test('shows closure when range is fully inside it', () => {
    // [Sep 18 → Sep 22] is contained by [Sep 16 → Sep 30]
    expect(closureMatchesDateRange(closure, d('2020-09-18'), d('2020-09-22'), false)).toBe(true);
  });

  test('hides closure when range ends before it starts', () => {
    // [Sep 01 → Sep 15] is entirely before [Sep 16 → Sep 30]
    expect(closureMatchesDateRange(closure, d('2020-09-01'), d('2020-09-15'), false)).toBe(false);
  });

  test('hides closure when BOTH closure dates are before startDate', () => {
    // User's explicit requirement: start=Oct 10, end=Nov 10 → Sep 16–Sep 30 should NOT show
    expect(closureMatchesDateRange(closure, d('2020-10-10'), d('2020-11-10'), false)).toBe(false);
  });

  test('hides closure when range starts after it reopened', () => {
    expect(closureMatchesDateRange(closure, d('2020-10-01'), d('2020-11-01'), false)).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// Animation mode (isAnimating = true) — uses asOf = endDate
// ---------------------------------------------------------------------------

describe('animation mode — asOf endDate', () => {
  const closure = makeClosure('2020-09-16', '2020-09-30');

  test('hides closure when animation frame is before it starts', () => {
    // startDate fixed at Sep 10; current frame (endDate) = Sep 15
    expect(closureMatchesDateRange(closure, d('2020-09-10'), d('2020-09-15'), true)).toBe(false);
  });

  test('shows closure when animation frame reaches its start date', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-10'), d('2020-09-16'), true)).toBe(true);
  });

  test('shows closure mid-animation', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-10'), d('2020-09-20'), true)).toBe(true);
  });

  test('shows closure on its reopening date', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-10'), d('2020-09-30'), true)).toBe(true);
  });

  test('hides closure once animation frame passes its reopening date', () => {
    // This is the key regression: with a fixed startDate, interval overlap would keep
    // the closure visible forever; asOf semantics correctly removes it.
    expect(closureMatchesDateRange(closure, d('2020-09-10'), d('2020-10-01'), true)).toBe(false);
  });

  test('closure stays hidden well past its reopening date', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-10'), d('2021-01-01'), true)).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// Legacy field names
// ---------------------------------------------------------------------------

describe('legacy Title-Case field names', () => {
  const closure = makeClosureLegacy('2020-09-16', '2020-09-30');

  test('single-day: shows closure using legacy fields', () => {
    expect(closureMatchesDateRange(closure, d('2020-09-20'), null, false)).toBe(true);
  });

  test('static range: hides closure using legacy fields when out of range', () => {
    expect(closureMatchesDateRange(closure, d('2020-10-10'), d('2020-11-10'), false)).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// Missing / invalid data
// ---------------------------------------------------------------------------

describe('missing or invalid data', () => {
  test('returns false when closure date is missing', () => {
    const closure: SchoolClosure = { date_of_reopening: '2020-09-30' };
    expect(closureMatchesDateRange(closure, d('2020-09-20'), null, false)).toBe(false);
  });

  test('returns false when closure date is an invalid string', () => {
    const closure = makeClosure('not-a-date', '2020-09-30');
    expect(closureMatchesDateRange(closure, d('2020-09-20'), null, false)).toBe(false);
  });

  test('treats missing reopening date as "no upper bound" (always open)', () => {
    const closure: SchoolClosure = { date_of_closure: '2020-09-16' };
    // No reopening date — closure should show for any date after it opened
    expect(closureMatchesDateRange(closure, d('2021-06-01'), null, false)).toBe(true);
  });

  test('treats empty-string reopening date as "no upper bound"', () => {
    const closure = makeClosure('2020-09-16', '');
    expect(closureMatchesDateRange(closure, d('2022-01-01'), null, false)).toBe(true);
  });
});
