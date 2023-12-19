import { Then } from '@cucumber/cucumber';
import { waitFor } from '../../support/wait-for-behavior.js';
import { expect } from '@playwright/test';
Then(/^I should (not |)see a "([^"]*)" with "([^"]*)" text$/, async function (negate, role, name) {
    const { page } = this.getScreen();
    await waitFor(async () => {
        const locator = page.getByRole(role, { name, exact: true });
        if (negate === "not") {
            // TODO: need to figure out how to assert that an element is not visible
            return expect(locator.count()).toEqual(0);
        }
        else {
            const isElementVisible = await locator.isVisible();
            return isElementVisible;
        }
    }, { label: `I should ${negate}see a ${role} with ${name} text` });
});
Then(/^I should see the first "([^"]*)" showing "([^"]*)" text$/, async function (role, name) {
    const { page } = this.getScreen();
    await waitFor(async () => {
        const tableLocator = page.locator('table');
        const textContent = await tableLocator.locator(`${role}:nth-of-type(1)`).nth(0).textContent();
        if (!!textContent) {
            return textContent.includes(name);
        }
    }, { timeout: 440000, label: `I should see the first ${role} showing ${name} text` });
});
function sleep(seconds) {
    return new Promise(resolve => setTimeout(resolve, seconds * 1000));
}
Then(/^I should see a "([^"]*)" with "([^"]*)" text within the "([^"]*)" "([^"]*)"$/, async function (role, name, parentName, parentRole) {
    const { page } = this.getScreen();
    // NOTE: This locator uses html accessibility roles and names to find elements.
    // If your test is not finding an element, please verify that the role and name are correct.
    await waitFor(async () => {
        const locator = page.getByRole(parentRole, { name: parentName, exact: true }).getByRole(role, { name, exact: true });
        const isElementVisible = await locator.isVisible();
        return isElementVisible;
    });
});
Then(/^I should see "([^"]*)" text$/, async function (text) {
    const { page } = this.getScreen();
    // NOTE: This locator uses html accessibility roles and names to find elements.
    // If your test is not finding an element, please verify that the role and name are correct.
    await waitFor(async () => {
        const locator = await page.getByText(text);
        const isElementVisible = await locator.isVisible();
        return isElementVisible;
    });
});
