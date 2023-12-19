import { waitFor, waitForRoleVisible, waitForSelectorVisible, waitForTestIdVisible } from "../../support/wait-for-behavior.js";
import { inputValue } from '../../support/html-behavior.js';
import { Bech32 } from '../../cardano.js';
var SPENDING_PASSWORD = "Runner test";
export const configure = async function (page, mnemonic, walletURL) {
    await page.goto(`${walletURL}/app.html`);
    const readAddress = async () => {
        const button = await waitForRoleVisible(page, "button", "Receive");
        await button.click();
        const addressContainer = await waitForTestIdVisible(page, "address-card-address");
        const address = await addressContainer.innerText();
        return Bech32.fromString(address);
    };
    const buttonRestore = page.getByRole("button", { name: "Restore", exact: true });
    const buttonRestoreVisible = await waitFor(async () => {
        return await buttonRestore.isVisible();
    }, { timeout: 1000, onTimeout: () => { return false; } });
    if (!buttonRestoreVisible) {
        return await readAddress();
    }
    else {
        await buttonRestore.click();
    }
    var locator;
    locator = await waitForRoleVisible(page, "button", "OK");
    await locator.click();
    locator = await waitForRoleVisible(page, "checkbox", "I accept the Terms of Use");
    await locator.click();
    locator = await waitForRoleVisible(page, "button", "Next");
    await locator.click();
    locator = await waitForRoleVisible(page, "button", "Agree");
    await locator.click();
    locator = await waitForTestIdVisible(page, "wallet-setup-register-name-input");
    await inputValue(locator, "Runner test");
    locator = await waitForRoleVisible(page, "button", "Next");
    await locator.click();
    locator = await waitForTestIdVisible(page, "wallet-setup-password-step-password");
    await inputValue(locator, SPENDING_PASSWORD);
    locator = await waitForTestIdVisible(page, "wallet-setup-password-step-confirm-password");
    await inputValue(locator, SPENDING_PASSWORD);
    locator = await waitForRoleVisible(page, "button", "Next");
    await locator.click();
    locator = await waitForRoleVisible(page, "button", "Next");
    await locator.click();
    for (let i = 0; i < 8; i++) {
        locator = await waitForSelectorVisible(page, `#mnemonic-word-${i + 1}`);
        await inputValue(locator, mnemonic[i]);
    }
    locator = await waitForRoleVisible(page, "button", "Next");
    await locator.click();
    for (let i = 8; i < 16; i++) {
        locator = await waitForSelectorVisible(page, `#mnemonic-word-${i + 1}`);
        await inputValue(locator, mnemonic[i]);
    }
    locator = await waitForRoleVisible(page, "button", "Next");
    await locator.click();
    for (let i = 16; i < 24; i++) {
        locator = await waitForSelectorVisible(page, `#mnemonic-word-${i + 1}`);
        await inputValue(locator, mnemonic[i]);
    }
    locator = await waitForRoleVisible(page, "button", "Next");
    await locator.click();
    locator = await waitForRoleVisible(page, "button", "Go to my wallet");
    await locator.click();
    locator = await waitForRoleVisible(page, "button", "Got it");
    await locator.click();
    locator = await waitForTestIdVisible(page, "user-avatar");
    await locator.click();
    locator = page.getByTestId('header-menu').getByTestId('header-menu-network-choice-label');
    await locator.waitFor({ state: "visible" });
    await locator.click();
    locator = page.getByTestId('header-menu').getByTestId("network-preprod-radio-button");
    await locator.waitFor({ state: "visible" });
    await locator.click();
    // Let's confirm that the network was fully reloaded
    await waitFor(async () => {
        const networkPill = await waitForTestIdVisible(page, 'network-pill');
        const innerText = await networkPill.innerText();
        if (innerText == 'Preprod') {
            return true;
        }
    }, { label: "Preprod network pill" });
    return await readAddress();
};
export const authorizeApp = async function (page, triggerAuthorization, isAuthorizedCheck) {
    const walletPopupPromise = new Promise(resolve => page.context().once('page', resolve));
    await triggerAuthorization();
    const grantAccess = (async () => {
        const page = await walletPopupPromise;
        await page.reload();
        let locator;
        locator = await waitForRoleVisible(page, "button", "Authorize");
        await locator.click();
        locator = await waitForRoleVisible(page, "button", "Always");
        await locator.click();
    })();
    // Playwright `waitFor` doesn't support aborting the promise
    // so the loosing one will run till the timeout and the result
    // will be ignored.
    await Promise.any([isAuthorizedCheck(page), grantAccess]);
    await isAuthorizedCheck(page);
};
export const signTx = async (walletPopupWrapper) => {
    var locator;
    let possibleWalletPopup = walletPopupWrapper.getPage();
    if (possibleWalletPopup === undefined) {
        throw new Error("Wallet popup was probably already closed");
    }
    let walletPopup = possibleWalletPopup;
    locator = await waitForRoleVisible(walletPopup, "button", "Confirm");
    await locator.click();
    locator = await waitForTestIdVisible(walletPopup, "password-input");
    await inputValue(locator, SPENDING_PASSWORD);
    const confirm = async function () {
        locator = await waitForRoleVisible(walletPopup, "button", "Confirm");
        await locator.click();
    };
    await confirm();
    // Sometimes we have to double confirm or we will stuck on the popup
    locator = await waitForRoleVisible(walletPopup, "button", "Close").catch(async () => {
        await confirm().catch(async () => { return; });
        return await waitForRoleVisible(walletPopup, "button", "Close");
    });
    await locator.click();
};
// The same as the signTx but we are rejecting the tx
export const rejectTx = async function (page, triggerSign) {
    var locator;
    const walletPopupPromise = new Promise(resolve => page.context().once('page', resolve));
    await triggerSign();
    const walletPopup = await walletPopupPromise;
    await walletPopup.reload();
    locator = await waitForRoleVisible(page, "button", "Cancel");
    await locator.click();
};
