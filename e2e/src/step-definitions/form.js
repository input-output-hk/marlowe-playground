import { When } from "@cucumber/cucumber";
import { getElementLocator } from '../support/web-element-helper.js';
import { waitFor, waitForRoleVisible } from "../support/wait-for-behavior.js";
import { inputValue } from '../support/html-behavior.js';
import { MarloweJSON } from "@marlowe.io/adapter/codec";
When(/^I fill in the "([^"]*)" input with "([^"]*)"$/, async function (elementKey, input) {
    const { page } = this.getScreen();
    const { globalConfig } = this;
    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;
    await waitFor(async () => {
        const locator = page.getByRole(role, { name });
        const result = await locator.isVisible();
        if (result) {
            await inputValue(locator, input);
            return result;
        }
    });
});
When(/^I unblur the "([^"]*)" input$/, async function (elementKey) {
    const { page } = this.getScreen();
    const { globalConfig } = this;
    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;
    await waitFor(async () => {
        const locator = page.getByRole(role, { name });
        const result = await locator.isVisible();
        if (result) {
            await locator.blur();
            return result;
        }
    });
});
When('I find the line in the {string} editor containing {string}', async function (editorName, codeExample) {
    const { page } = this.getScreen();
    await waitFor(async () => {
        await page.getByRole('code').locator('div').filter({ hasText: codeExample }).nth(4).click();
        return true;
    });
});
When('I press {string} on the keyboard {string} times', async function (keyName, numberOfTimes) {
    const { page } = this.getScreen();
    await waitFor(async () => {
        const times = parseInt(numberOfTimes, 10);
        for (let i = 0; i < times; i++) {
            await page.keyboard.press(keyName);
        }
        return true;
    });
});
When(/^I enter the json of the contract "([^"]*)" into the "([^"]*)" field$/, async function (contractName, name) {
    const { page } = this.getScreen();
    const contractInfo = this.getContractInfo(contractName);
    const locator = await waitForRoleVisible(page, "textbox", name);
    const json = MarloweJSON.stringify(contractInfo.contract);
    await inputValue(locator, json);
});
// Generic step - should we keep it?
When('I enter the contents of {string} into the {string} field', async function (fileName, name) {
    const { page } = this.getScreen();
    const { globalStateManager } = this;
    const role = "textbox";
    await waitFor(async () => {
        const locator = page.getByRole(role, { name });
        const result = await locator.isVisible();
        const input = globalStateManager.getValue(fileName);
        if (result) {
            await inputValue(locator, input);
            return result;
        }
    });
});
When('I select {string} from the {string} dropdown', async function (option, name) {
    const { page } = this.getScreen();
    await waitFor(async () => {
        const locator = page.locator(`select.${name}`);
        const result = await locator.isVisible();
        if (result) {
            await locator.selectOption(option);
            return result;
        }
    });
});
