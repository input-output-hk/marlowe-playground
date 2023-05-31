import { When } from "@cucumber/cucumber";
import { ScenarioWorld } from './setup/world';
import { getElementLocator } from '../support/web-element-helper';
import { ElementKey } from '../env/global';
import { ValidAccessibilityRoles } from '../env/global';
import { waitFor } from "../support/wait-for-behavior";
import {
  inputValue,
} from '../support/html-behavior';

When(
  /^I fill in the "([^"]*)" input with "([^"]*)"$/,
  async function(this: ScenarioWorld, elementKey: ElementKey, input: string) {
    const {
      screen: { page },
      globalConfig
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;

    await waitFor(async() => {
      const locator = await page.getByRole(role as ValidAccessibilityRoles, { name })
      const result = await locator.isVisible();

      if (result) {
        await inputValue(locator, input);
        await page.pause();
        return result;
      }
    });
  }
)

// When(
//   /^I fill in the "playground editor" input with "([^"]*)" contract code$/,
//   async function(this: ScenarioWorld, fixtureKey: FixtureKey) {
//     const {
//       screen: { page },
//       globalConfig
//     } = this;


//     console.log(`I fill in the playground editor input with ${fixtureKey}`);

//     const elementIdentifier = getElementLocator(page, "playground editor", globalConfig);
//     const { role, name } = elementIdentifier;
//     const document = await getDocument(page);

//     const fixture = getFixtureText(fixtureKey, globalConfig);
//     await waitFor(async() => {
//       const locator = await queries.getByRole(document, role, { name })
//       const result = await locator.isVisible();

//       if (result) {
//         // NOTE: Need to add this exact amount of new line characters to have the beginning of the
//         // code show on the editor. Otherwise, the first few lines of the code show in the editor
//         // and cause syntax errors.
//         const fixtureForInput ="\n\n\n\n\n\n\n\n\n\n\n\n " + fixture

//         await inputValue(locator, fixtureForInput)
//         return result;
//       }
//     });
//   }
// )

// When(
//   /^I select the "([^"]*)" option from the "([^"]*)"$/,
//   async function(this: ScenarioWorld, option: string, elementKey: ElementKey) {
//     const {
//       screen: { page },
//       globalConfig
//     } = this;

//     console.log(`I select the ${option} option from the ${elementKey}`)
//     const document = await getDocument(page);

//     const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
//     const { role, name } = elementIdentifier;

//     await waitFor(async() => {
//       const locator = await queries.getByRole(document, role, { name })
//       const result = await locator.isVisible();

//       if (result) {
//         await selectValue(locator, option)
//         return result;
//       }
//     })
//   }
// )

// When(
//   /^I copy the "([^"]*)" text$/,
//   async function(this: ScenarioWorld, elementKey: ElementKey) {
//     const {
//       screen: { page },
//       globalConfig
//     } = this;

//     const document = await getDocument(page);

//     const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
//     const { role, name } = elementIdentifier;

//     await waitFor(async() => {
//       const locator = await queries.getByRole(document, role, { name })
//       const result = await locator.isVisible();

//       if (result) {
//         this[elementKey] = await locator.innerText()
//         return result;
//       }
//     })
//   }
// )

// When(
//   /^I fill in the "([^"]*)" input with "([^"]*)" from the clipboard$/,
//   async function(this: ScenarioWorld, elementKey: ElementKey, clipboardInput: string) {
//     const {
//       screen: { page },
//       globalConfig
//     } = this;


//     console.log(`I fill in the ${elementKey} input with ${clipboardInput} from the clipboard`);

//     const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
//     const { role, name } = elementIdentifier;
//     const document = await getDocument(page);

//     const input = this[clipboardInput]

//     await waitFor(async() => {
//       const locator = await queries.getByRole(document, role, { name })
//       const result = await locator.isVisible();

//       if (result) {
//         await inputValue(locator, input)
//         return result;
//       }
//     });
//   }
// )