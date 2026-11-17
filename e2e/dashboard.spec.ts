import { test, expect, type Page } from '@playwright/test';

// Smoke tests for the built dashboard. They check that every view renders with
// data, not exact numbers — CI runs against synthetic closures data
// (tests/fixtures/school_closures.sample.json), so counts differ from production.

// Errors from third-party hosts (OpenStreetMap tiles, cdnjs marker icons) are
// ignored so a flaky external CDN can't fail a dependency PR.
function trackPageErrors(page: Page): string[] {
  const errors: string[] = [];
  const isFirstParty = (url: string) => !url || url.startsWith(new URL(page.url() || 'http://localhost').origin);
  page.on('pageerror', (err) => errors.push(`pageerror: ${err.message}`));
  page.on('console', (msg) => {
    if (msg.type() === 'error' && isFirstParty(msg.location().url)) errors.push(`console: ${msg.text()}`);
  });
  page.on('response', (res) => {
    if (res.status() >= 400 && isFirstParty(res.url())) errors.push(`HTTP ${res.status()}: ${res.url()}`);
  });
  return errors;
}

async function openDashboard(page: Page) {
  await page.goto('/');
  // DataProvider shows this card if any data file fails to load or parse
  await expect(page.getByText('Failed to load dashboard data')).toHaveCount(0);
  await expect(page.getByRole('heading', { name: 'Interactive Map' })).toBeVisible();
}

function statValue(page: Page, label: string) {
  // MapView renders "<label>:" followed by its value in the next span
  return page.locator(`span:text-is("${label}:") + span`).first();
}

test('interactive map renders case and closure markers', async ({ page }) => {
  const errors = trackPageErrors(page);
  await openDashboard(page);

  await expect(page.locator('.leaflet-container')).toBeVisible();
  // The map uses preferCanvas, so CircleMarkers are drawn onto a canvas in the overlay pane
  await expect(page.locator('.leaflet-overlay-pane canvas')).toBeVisible();
  await expect(statValue(page, 'Schools with Cases')).not.toHaveText('0');

  // Pick a date with both cases and a closure in the fixture data
  await page.locator('input[type="date"]').first().fill('2020-11-16');
  await expect(statValue(page, 'Selected Date')).toHaveText('2020-11-16');
  await expect(statValue(page, 'Schools with Cases')).not.toHaveText('0');
  await expect(statValue(page, 'School Closures')).not.toHaveText('0');

  expect(errors).toEqual([]);
});

test('summary view renders statistics and charts', async ({ page }) => {
  const errors = trackPageErrors(page);
  await openDashboard(page);

  await page.getByRole('button', { name: 'Summary & Analytics' }).click();
  await expect(page.getByRole('heading', { name: 'Summary & Analytics' })).toBeVisible();
  await expect(page.getByRole('heading', { name: 'Total Cases' })).toBeVisible();
  // Recharts renders each chart into an SVG surface
  await expect(page.locator('svg.recharts-surface').nth(1)).toBeVisible();

  expect(errors).toEqual([]);
});

const staticViews = [
  { tab: 'Data Sources', heading: 'Data Sources & Code' },
  { tab: 'About', heading: 'About This Site' },
  { tab: 'Media', heading: /Research applications and media coverage/ },
  { tab: 'Team', heading: 'The Team and Contact' },
];

for (const { tab, heading } of staticViews) {
  test(`${tab} view renders`, async ({ page }) => {
    const errors = trackPageErrors(page);
    await openDashboard(page);

    await page.getByRole('button', { name: tab, exact: true }).click();
    await expect(page.getByRole('heading', { name: heading })).toBeVisible();
    await expect(page.getByText('Something went wrong')).toHaveCount(0);

    expect(errors).toEqual([]);
  });
}
