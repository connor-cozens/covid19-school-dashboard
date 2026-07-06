/**
 * Shared constants for the COVID-19 Ontario School Dashboard.
 * Centralises magic numbers so they stay consistent across map rendering,
 * legend display, data filtering, and KPI calculations.
 */

// ---------------------------------------------------------------------------
// Map geography
// ---------------------------------------------------------------------------

/** Leaflet LatLngBounds corners constraining pan/zoom to Ontario. */
export const ONTARIO_BOUNDS = [[41.5, -95.5], [57.5, -74.0]] as [[number, number], [number, number]];

export const MAP_DEFAULT_CENTER: [number, number] = [43.6532, -79.3832]; // Toronto
export const MAP_DEFAULT_ZOOM = 7;
export const MAP_MIN_ZOOM = 5;
export const MAP_MAX_ZOOM = 15;

// ---------------------------------------------------------------------------
// Case marker colours  (must match legend in MapView)
// ---------------------------------------------------------------------------

/** Orange — schools with 1–5 confirmed cases. Matches R app colour. */
export const CASE_COLOR_LOW = '#ff7a00';

/** Red — schools with 6+ confirmed cases. Matches R app colour. */
export const CASE_COLOR_HIGH = '#d62728';

/** Purple — school closure markers. */
export const CASE_COLOR_CLOSURE = '#9333ea';

/** Case count threshold: at or below → low (orange); above → high (red). */
export const CASE_LOW_THRESHOLD = 5;

// ---------------------------------------------------------------------------
// Case marker sizing
// ---------------------------------------------------------------------------

export const MARKER_RADIUS_MIN = 8;
export const MARKER_RADIUS_MAX = 20;
export const MARKER_RADIUS_SCALE = 0.6;

// ---------------------------------------------------------------------------
// Data / date boundaries
// ---------------------------------------------------------------------------

/**
 * ISO date string separating school year 1 (2020-21) from year 2 (2021-22).
 * Used by updateDashboardStats() to sum cumulative totals independently
 * for each year rather than treating the merged summary array as one
 * running cumulative.
 */
export const SCHOOL_YEAR_BOUNDARY = '2021-07-01';
