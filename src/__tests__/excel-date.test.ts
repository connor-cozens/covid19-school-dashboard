// Tests for the pure algorithmic logic in scripts/clean_school_closures.js.
// Because the functions are not exported (the script calls main() directly),
// we replicate the exact function bodies here and test the contract.
// Any change to the script logic must be mirrored here.

// ---------------------------------------------------------------------------
// Inline reimplementation — must match scripts/clean_school_closures.js exactly
// ---------------------------------------------------------------------------

const DEFAULT_CLOSURE_DURATION_DAYS = 14;

function excelDateToISO(val: unknown): string {
  if (!val) return '';
  if (val instanceof Date) return val.toISOString().slice(0, 10);
  if (typeof val === 'number') {
    if (isNaN(val)) return '';
    const ms = (val - 25569) * 86400 * 1000;
    return new Date(ms).toISOString().slice(0, 10);
  }
  const d = new Date(val as string);
  return isNaN(d.getTime()) ? '' : d.toISOString().slice(0, 10);
}

interface ReopenResult {
  reopenDate: string;
  corrected: boolean;
  originalReopenDate: string | null;
}

function resolveReopenDate(closureDate: string, rawReopenDate: string): ReopenResult {
  if (!closureDate) return { reopenDate: '', corrected: false, originalReopenDate: null };

  const addDefault = (): string => {
    const d = new Date(closureDate);
    d.setDate(d.getDate() + DEFAULT_CLOSURE_DURATION_DAYS);
    return d.toISOString().slice(0, 10);
  };

  // Missing reopen date — apply default silently (not a correction, just a fill)
  if (!rawReopenDate) {
    return { reopenDate: addDefault(), corrected: false, originalReopenDate: null };
  }

  // Reopen on or before closure — source data error (year typo or invalid entry)
  if (rawReopenDate <= closureDate) {
    const corrected = addDefault();
    return { reopenDate: corrected, corrected: true, originalReopenDate: rawReopenDate };
  }

  return { reopenDate: rawReopenDate, corrected: false, originalReopenDate: null };
}

// ---------------------------------------------------------------------------
// excelDateToISO tests
// ---------------------------------------------------------------------------

describe('excelDateToISO', () => {
  test('serial 44197 maps to 2021-01-01 (no +1 offset)', () => {
    expect(excelDateToISO(44197)).toBe('2021-01-01');
  });

  test('serial 44228 maps to 2021-02-01 (31 days after 44197)', () => {
    expect(excelDateToISO(44228)).toBe('2021-02-01');
  });

  test('null returns empty string', () => {
    expect(excelDateToISO(null)).toBe('');
  });

  test('NaN returns empty string', () => {
    expect(excelDateToISO(NaN)).toBe('');
  });

  test('ISO date string passes through unchanged', () => {
    expect(excelDateToISO('2021-03-15')).toBe('2021-03-15');
  });

  test('invalid string returns empty string', () => {
    expect(excelDateToISO('not-a-date')).toBe('');
  });

  test('Date object is converted to ISO date string', () => {
    // Use UTC noon to avoid timezone edge-cases
    expect(excelDateToISO(new Date('2021-01-01T12:00:00Z'))).toBe('2021-01-01');
  });
});

// ---------------------------------------------------------------------------
// resolveReopenDate tests
// ---------------------------------------------------------------------------

describe('resolveReopenDate', () => {
  test('missing reopen date fills with closure + 14 days (corrected=false)', () => {
    const result = resolveReopenDate('2021-12-01', '');
    expect(result.reopenDate).toBe('2021-12-15');
    expect(result.corrected).toBe(false);
    expect(result.originalReopenDate).toBeNull();
  });

  test('valid reopen date after closure passes through unchanged', () => {
    const result = resolveReopenDate('2021-12-01', '2021-12-20');
    expect(result.reopenDate).toBe('2021-12-20');
    expect(result.corrected).toBe(false);
    expect(result.originalReopenDate).toBeNull();
  });

  test('reopen date preceding closure is corrected to closure + 14 days (year typo case)', () => {
    // Reopen 2021-01-03 precedes closure 2021-12-15 — likely year typo in source Excel
    const result = resolveReopenDate('2021-12-15', '2021-01-03');
    expect(result.corrected).toBe(true);
    expect(result.originalReopenDate).toBe('2021-01-03');
    expect(result.reopenDate).toBe('2021-12-29');
  });

  test('reopen date equal to closure date is corrected (corrected=true)', () => {
    const result = resolveReopenDate('2021-12-15', '2021-12-15');
    expect(result.corrected).toBe(true);
    expect(result.reopenDate).toBe('2021-12-29');
    expect(result.originalReopenDate).toBe('2021-12-15');
  });
});
