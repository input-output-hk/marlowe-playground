import { waitFor, waitForRoleVisible } from "../../support/wait-for-behavior.js";
import { inputValue } from '../../support/html-behavior.js';
import { sleep } from '../../promise.js';
import { Bech32 } from '../../cardano.js';
var SPENDING_PASSWORD = "Runner test";
// Configure a nami wallet spcified by the mnemonic if it is not already configured.
export const configure = async function (page, mnemonic, walletURL) {
    await page.goto(`${walletURL}/mainPopup.html`);
    const readAddress = async () => await waitFor(async () => {
        const button = page.locator('button', { hasText: "Receive" });
        await button.click();
        const addressContainer = page.locator('.chakra-text', { hasText: /addr.*/ });
        const address = await addressContainer.innerText();
        try {
            return Bech32.fromString(address);
        }
        catch (error) {
            console.log(`Address ${address} is not a valid bech32 address`);
            throw error;
        }
    }, { label: "Wallet Address visible" });
    // Check if we are already logged in
    const buttonNewVisible = await waitFor(async () => {
        const locator = page.getByRole("button", { name: "New Wallet", exact: true });
        return await locator.isVisible();
    }, { timeout: 1000, onTimeout: () => { return false; } });
    if (!buttonNewVisible) {
        return await readAddress();
    }
    await page.goto(`${walletURL}/createWalletTab.html?type=import&length=24`);
    let locator;
    for (let i = 0; i < 24; i++) {
        const name = `Word ${i + 1}`;
        locator = await waitForRoleVisible(page, "textbox", name);
        await inputValue(locator, mnemonic[i]);
    }
    locator = await waitForRoleVisible(page, "button", "Next");
    await locator.click();
    locator = await waitForRoleVisible(page, "textbox", "Enter account name");
    await inputValue(locator, "Runner test");
    locator = await waitForRoleVisible(page, "textbox", "Enter password");
    await inputValue(locator, SPENDING_PASSWORD);
    locator = await waitForRoleVisible(page, "textbox", "Confirm password");
    await inputValue(locator, SPENDING_PASSWORD);
    locator = await waitForRoleVisible(page, "button", "Create");
    await locator.click();
    await waitForRoleVisible(page, "button", "Close");
    await page.goto(`${walletURL}/mainPopup.html`);
    const clickEmptyButton = async () => {
        const locator = await waitForRoleVisible(page, "button", "");
        await locator.click();
    };
    await clickEmptyButton();
    locator = await waitForRoleVisible(page, "menuitem", "Settings");
    await locator.click();
    locator = await waitForRoleVisible(page, "button", "Network");
    await locator.click();
    locator = await waitForRoleVisible(page, "combobox");
    await locator.selectOption("Preprod");
    locator = page.locator(".chakra-text").last();
    await locator.waitFor({ state: "visible" });
    await waitFor(async () => {
        const innerText = await locator.innerText();
        return (innerText === "Preprod");
    }, { label: "Preprod network text" });
    // Nami is BUGGY. Jumping directly or indirectly to subpages
    // is error prone. We need to do it step by step by slowly clicking.
    // await page.goto(`${walletURL}/wallet`); // this doesn't work
    // TODO: Replace `sleep` with proper sub-pages element visibility checks.
    await sleep(1);
    await clickEmptyButton();
    await sleep(1);
    await clickEmptyButton();
    return await readAddress();
};
// After action to connect wallet we can have redirect to a wallet page or we can jump directly
// to the app internals.
// This function helps to handle both cases by accepting a callback that checks if we are already
// authorized.
// It should be called after connect action.
export const authorizeApp = async function (page, triggerAuthorization, isAuthorizedCheck) {
    const walletPopupPromise = new Promise(resolve => page.context().once('page', resolve));
    await triggerAuthorization();
    const grantAccess = (async () => {
        const page = await walletPopupPromise;
        await page.reload();
        const locator = await waitForRoleVisible(page, "button", "Access");
        await locator.click();
        return true;
    })();
    const isAuthorized = async () => waitFor(async () => {
        const result = await isAuthorizedCheck(page);
        return result;
    }, { label: "App authorized" });
    await Promise.any([isAuthorized(), grantAccess]);
    await isAuthorized();
};
export const signTx = async (walletPopupWrapper) => {
    var locator;
    let possibleWalletPopup = walletPopupWrapper.getPage();
    if (possibleWalletPopup === undefined) {
        throw new Error("Wallet popup was probably already closed");
    }
    let walletPopup = possibleWalletPopup;
    locator = await waitForRoleVisible(walletPopup, "button", "Sign");
    await locator.click();
    locator = await waitForRoleVisible(walletPopup, "textbox", "Enter password");
    await inputValue(locator, SPENDING_PASSWORD);
    locator = await waitForRoleVisible(walletPopup, "button", "Confirm");
    await locator.click();
};
export const rejectTx = async function (page, triggerSign) {
    var locator;
    const walletPopupPromise = new Promise(resolve => page.context().once('page', resolve));
    await triggerSign();
    const walletPopup = await walletPopupPromise;
    await walletPopup.reload();
    locator = await waitForRoleVisible(page, "button", "Cancel");
    await locator.click();
};
