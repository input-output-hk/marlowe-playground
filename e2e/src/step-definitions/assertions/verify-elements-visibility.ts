import { Then } from '@cucumber/cucumber';
import { ElementKey } from '../../env/global';
import { getElementLocator } from '../../support/web-element-helper';
import { queries } from 'playwright-testing-library';
import { ScenarioWorld } from '../setup/world'
import { waitFor } from '../../support/wait-for-behavior';

Then(
  /^I should see "([^"]*)" text$/,
  async function(this: ScenarioWorld, elementKey: ElementKey) {

    const {
      screen: { page, document },
      globalConfig
    } = this;

    console.log("ElementKey: ", elementKey)
    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;

    console.log("ROLE: ", role)
    console.log("NAMEL ", name)

    await waitFor(async() => {
      console.log("INSIDE WAIT BEGIN")
      const locator = await queries.getByRole(document, role, { name })
      console.log("LOCATOR: ", locator)
      const isElementVisible = await locator.isVisible()
      console.log("isVISIBLE: ", isElementVisible)
      return isElementVisible;
    })
  }
);