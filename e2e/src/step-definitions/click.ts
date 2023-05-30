import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from './setup/world';
import { ValidAccessibilityRoles } from '../env/global';

When(
  /^I click the "([^"]*)" with "([^"]*)" text$/,
  async function(this: ScenarioWorld, role: ValidAccessibilityRoles, name: string) {
    const {
      screen: { page },
      globalConfig
    } = this;

    const locator = await page.getByRole(role, { name, exact: true });
    const result = await locator.isVisible();
    if (result) await locator.click();
  }
);

When(
  /^I click the "([^"]*)" (?:button|link)$/,
  async function(this: ScenarioWorld, name: string, role: ValidAccessibilityRoles) {
    const {
      screen: { page },
      globalConfig
    } = this;

    const locator = await page.getByRole(role, { name, exact: true });
    const result = await locator.isVisible();
    if (result) await locator.click();
  }
)