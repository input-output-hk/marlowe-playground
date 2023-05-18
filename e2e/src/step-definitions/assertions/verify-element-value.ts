import { Then } from '@cucumber/cucumber';
import { ValidAccessibilityRoles } from '../../env/global';
import { ScenarioWorld } from '../setup/world'
import { waitFor } from '../../support/wait-for-behavior';

Then(
  /^the "([^"]*)" should contain "([^"]*)" text$/,
  async function(this: ScenarioWorld, role: ValidAccessibilityRoles, name: string) {
    const {
      screen: { page },
      globalConfig
    } = this;
    await waitFor(async() => {
      const locator = await page.getByRole(role, { name, exact: true });
      const elementText = await locator.textContent();
      return elementText?.includes(name);
    })
  }
);

Then(
  /^the "([^"]*)" for "([^"]*)" should contain "([^"]*)" text$/,
  async function(this: ScenarioWorld, role: ValidAccessibilityRoles, name: string, expectedText: string) {
    const {
      screen: { page },
      globalConfig
    } = this;
    await waitFor(async() => {
      const locator = await page.getByRole(role, { name, exact: true });
      const elementText = await locator.textContent();
      return elementText?.includes(expectedText);
    })
  }
);
