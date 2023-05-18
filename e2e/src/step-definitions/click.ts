import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from './setup/world';
import { clickElement } from '../support/html-behavior';
import { waitFor } from '../support/wait-for-behavior';
import { ValidAccessibilityRoles } from '../env/global';

When(
  /^I click the "([^"]*)" with "([^"]*)" text$/,
  async function(this: ScenarioWorld, role: ValidAccessibilityRoles, name: string) {
    const {
      screen: { page },
      globalConfig
    } = this;

    try {

    await waitFor(async() => {
      // NOTE: This locator uses html accessibility roles and names to find elements.
      // If your test is not finding an element, please verify that the role and name are correct.
      const locator = await page.getByRole(role, { name, exact: true });
      const result = await locator.isVisible();

      if (result) await clickElement(locator);
    })
    } catch (e) {
      console.log("error: ", e);
    }
  }
)