/**
 * Accessor helpers for SchoolClosure records.
 *
 * The closure JSON is produced by clean_school_closures.js with camelCase
 * field names (date_of_closure, board_name, etc.).  Historically the raw
 * Excel data used Title Case names ('Date of Closure', 'board name', etc.)
 * and some records may still carry both.  These helpers consolidate the
 * fallback logic in one place so call sites don't repeat the || chain.
 */

import { SchoolClosure } from '@/types';

export const getClosureDate = (c: SchoolClosure): string =>
  c.date_of_closure || c['Date of Closure'] || '';

export const getReopeningDate = (c: SchoolClosure): string =>
  c.date_of_reopening || c['Date of Reopening'] || '';

export const getClosureSchoolName = (c: SchoolClosure): string =>
  c.school_name || c['School Name'] || '';

export const getClosureBoardName = (c: SchoolClosure): string =>
  c.board_name || c['board name'] || '';
