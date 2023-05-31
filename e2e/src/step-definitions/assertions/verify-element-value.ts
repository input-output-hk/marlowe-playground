import { Then } from '@cucumber/cucumber';
import { ElementKey, ValidAccessibilityRoles } from '../../env/global';
import { ScenarioWorld } from '../setup/world'
import { waitFor } from '../../support/wait-for-behavior';
import { getElementLocator } from '../../support/web-element-helper';

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
  /^the "([^"]*)" input should contain "([^"]*)" value$/,
  async function(this: ScenarioWorld, elementKey: ElementKey, expectedValue: string) {
    const {
      screen: { page },
      globalConfig
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;

    await waitFor(async() => {
      const locator = await page.getByRole(role as ValidAccessibilityRoles, { name, exact: true });
      const actualValue = await locator.inputValue();
      return actualValue == expectedValue;
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

Then(
  /^the "([^"]*)" with "([^"]*)" text should have "([^"]*)" class$/, 
  async function (this: ScenarioWorld, role: ValidAccessibilityRoles, name: string, expectedClass: string) {
    const {
      screen: { page },
      globalConfig
    } = this;
    await waitFor(async() => {
      const locator = await page.getByRole(role, { name, exact: true });
      const result = await locator.isVisible();
      if (result) {
        const classNames = await locator.getAttribute('class');
        if (classNames) {
          const classNamesArray = classNames.split(' ');
          return classNamesArray.includes(expectedClass);
        }
      }
    })
});