import { test, expect } from "@playwright/test";
import { fetchZones, zoneRow } from "./topology";

test("backend serves devices.json as JSON", async ({ request }) => {
  const response = await request.get("/devices.json");

  expect(response.status()).toBe(200);
  // lustre_http only decodes the body if it is served as JSON.
  expect(response.headers()["content-type"]).toContain("application/json");
});

test("every zone in devices.json gets a control", async ({ page, request }) => {
  const zones = await fetchZones(request);
  expect(zones.length).toBeGreaterThan(0);

  await page.goto("/");

  for (const { zone } of zones) {
    await expect(zoneRow(page, zone)).toBeVisible();
  }
  // Nothing extra, and nothing left behind from a hardcoded list.
  await expect(page.locator("[data-zone]")).toHaveCount(zones.length);
});

test("zones from every node appear in one flat list", async ({
  page,
  request,
}) => {
  const zones = await fetchZones(request);
  const nodes = new Set(zones.map((z) => z.node));
  expect(nodes.size).toBeGreaterThan(1);

  await page.goto("/");

  // The user shouldn't see any node grouping: every zone is a sibling control
  // in the same list, regardless of which node it hangs off.
  const allSiblings = await page
    .locator("[data-zone]")
    .evaluateAll((rows) =>
      rows.every((row) => row.parentElement === rows[0].parentElement),
    );
  expect(allSiblings).toBe(true);
});

test("shows an error when devices.json can't be loaded", async ({ page }) => {
  await page.route("**/devices.json", (route) => route.abort());

  await page.goto("/");

  await expect(page.getByText(/Couldn't load devices.json/)).toBeVisible();
  await expect(page.locator("[data-zone]")).toHaveCount(0);
});
