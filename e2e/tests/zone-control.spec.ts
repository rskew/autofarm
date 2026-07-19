import { test, expect } from "@playwright/test";
import {
  fetchZones,
  receiveMessage,
  sentMessages,
  stubWebSocket,
  zoneRow,
} from "./topology";

test.beforeEach(async ({ page }) => {
  await stubWebSocket(page);
});

test("turning a zone on addresses that zone's own node", async ({
  page,
  request,
}) => {
  const zones = await fetchZones(request);
  await page.goto("/");

  for (const { node, solenoid, zone } of zones) {
    await zoneRow(page, zone).locator('[data-action="on"]').click();

    // Default is 20 minutes.
    expect(await sentMessages(page)).toContain(`n${node}s${solenoid}on 1200`);
  }
});

test("turning a zone off addresses that zone's own node", async ({
  page,
  request,
}) => {
  const zones = await fetchZones(request);
  await page.goto("/");

  for (const { node, solenoid, zone } of zones) {
    await zoneRow(page, zone).locator('[data-action="off"]').click();

    expect(await sentMessages(page)).toContain(`n${node}s${solenoid}off`);
  }
});

test("the on duration is sent in seconds", async ({ page, request }) => {
  const [first] = await fetchZones(request);
  await page.goto("/");

  const row = zoneRow(page, first.zone);
  await row.locator('[data-field="minutes"]').fill("5");
  await row.locator('[data-action="on"]').click();

  expect(await sentMessages(page)).toContain(
    `n${first.node}s${first.solenoid}on 300`,
  );
});

test("a report from one node doesn't move another node's same-numbered solenoid", async ({
  page,
  request,
}) => {
  const zones = await fetchZones(request);

  // Solenoid numbers restart at 1 on each node, so the same solenoid number
  // exists on more than one node. Routing on the solenoid number alone would
  // flip the wrong zone.
  const collisions = zones.filter(
    (z) => zones.filter((other) => other.solenoid === z.solenoid).length > 1,
  );
  expect(collisions.length).toBeGreaterThan(1);
  const [reporting, unaffected] = collisions;

  await page.goto("/");
  await expect(zoneRow(page, reporting.zone)).toBeVisible();

  await receiveMessage(page, `n${reporting.node}s${reporting.solenoid}on`);

  await expect(
    zoneRow(page, reporting.zone).locator('[data-state="on"]'),
  ).toBeVisible();
  await expect(
    zoneRow(page, unaffected.zone).locator('[data-state="on"]'),
  ).toHaveCount(0);
});

test("a zone reflects the on and off state reported by its node", async ({
  page,
  request,
}) => {
  const [first] = await fetchZones(request);
  const address = `n${first.node}s${first.solenoid}`;
  await page.goto("/");
  await expect(zoneRow(page, first.zone)).toBeVisible();

  await receiveMessage(page, `${address}on`);
  await expect(
    zoneRow(page, first.zone).locator('[data-state="on"]'),
  ).toBeVisible();

  await receiveMessage(page, `${address}off`);
  await expect(
    zoneRow(page, first.zone).locator('[data-state="off"]'),
  ).toBeVisible();
});

test("an unrecognised message is shown in the message log", async ({
  page,
}) => {
  await page.goto("/");

  await receiveMessage(page, "INFO Received LoRa message");

  await expect(page.locator("textarea")).toHaveValue(
    /INFO Received LoRa message/,
  );
});
