import { When } from "@cucumber/cucumber";
import { waitForTestIdVisible } from "../support/wait-for-behavior.js";
When(/^I should see (error|success) toast$/, async function (toastType) {
    const { page } = this.getScreen();
    await waitForTestIdVisible(page, "toast-" + toastType + "-msg", 600000);
});
When(/^I should see (error|success) toast and close it$/, async function (toastType) {
    const { page } = this.getScreen();
    const toast = await waitForTestIdVisible(page, "toast-" + toastType + "-msg", 60000);
    const closeButton = toast.locator(".btn-close");
    await closeButton.click();
});
