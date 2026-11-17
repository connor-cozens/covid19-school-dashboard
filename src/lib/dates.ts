/** Adds days to an ISO 'YYYY-MM-DD' date, in UTC so no timezone can shift the day. */
export function addDays(isoDate: string, days: number): string {
  const d = new Date(`${isoDate}T00:00:00Z`);
  d.setUTCDate(d.getUTCDate() + days);
  return d.toISOString().slice(0, 10);
}

/** Formats an ISO date for display, e.g. "Nov 10, 2020", without a timezone shift. */
export function formatDate(isoDate: string | null | undefined): string {
  if (!isoDate) return 'N/A';
  const d = new Date(`${isoDate}T00:00:00Z`);
  if (isNaN(d.getTime())) return 'N/A';
  return d.toLocaleDateString(undefined, { year: 'numeric', month: 'short', day: 'numeric', timeZone: 'UTC' });
}
