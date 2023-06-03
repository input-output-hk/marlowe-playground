import { Then } from '@cucumber/cucumber';
import { ValidAccessibilityRoles } from '../../env/global';
import { ScenarioWorld } from '../setup/world'
import { waitFor } from '../../support/wait-for-behavior';
import { expect } from '@playwright/test';


Then(
  /^I should (not |)see a "([^"]*)" with "([^"]*)" text$/,
  async function(this: ScenarioWorld, negate: string, role: ValidAccessibilityRoles, name: string) {

    const {
      screen: { page },
    } = this;

    await waitFor(async () => {
        const locator = await page.getByRole(role, { name, exact: true });
      if (negate === "not") {
        // TODO: need to figure out how to assert that an element is not visible
        return expect(locator.count()).toEqual(0);
      } else {
        const isElementVisible = await locator.isVisible();
        return isElementVisible;
      }
    });
  }
);

Then(
  /^I should see a "([^"]*)" with "([^"]*)" text within the "([^"]*)" "([^"]*)"$/,
  async function(this: ScenarioWorld, role: ValidAccessibilityRoles, name: string, parentName: string, parentRole: ValidAccessibilityRoles) {

    const {
      screen: { page },
    } = this;


    // NOTE: This locator uses html accessibility roles and names to find elements.
    // If your test is not finding an element, please verify that the role and name are correct.
    await waitFor(async () => {
      const locator = await page.getByRole(parentRole, {name: parentName, exact: true}).getByRole(role, { name, exact: true });
      const isElementVisible = await locator.isVisible();
      return isElementVisible;
    });
  }
);