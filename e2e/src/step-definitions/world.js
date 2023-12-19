import playwright from 'playwright';
import { env } from '../env/parseEnv.js';
import * as fs from 'fs';
import * as nami from './wallets/nami.js';
import * as lace from './wallets/lace.js';
import { World, setWorldConstructor } from "@cucumber/cucumber";
import GlobalStateManager from "../support/globalStateManager.js";
import { Bech32 } from '../cardano.js';
const getWalletURL = async (context) => {
    let background = context.serviceWorkers();
    while (background.length !== 1) {
        await context.waitForEvent('serviceworker');
        background = context.serviceWorkers();
    }
    ;
    const walletId = background[0].url().split('/')[2];
    return 'chrome-extension://' + walletId;
};
export class ScenarioWorld extends World {
    constructor(options) {
        super(options);
        this.screen = null;
        this.contracts = {};
        this.globalStateManager = new GlobalStateManager();
        this.screensCache = {
            lace: {},
            nami: {}
        };
        this.mkLazyBrowsers = async () => {
            // This was probably copied - currenltly we use chromium only.
            const automationBrowsers = ['chromium', 'firefox', 'webkit'];
            const automationBrowser = env('UI_AUTOMATION_BROWSER');
            const pathToLaceExtension = env('LACE_WALLET_EXTENSION_PATH');
            const pathToNamiExtension = env('NAMI_WALLET_EXTENSION_PATH');
            const browserType = playwright[automationBrowser];
            const cache = this.screensCache;
            const screens = async function (walletType, walletName) {
                if (cache[walletType][walletName]) {
                    return cache[walletType][walletName];
                }
                const persistentContextName = '/tmp/' + walletName + '-' + walletType;
                const pathToExtension = walletType == "lace" ? pathToLaceExtension : pathToNamiExtension;
                const context = await browserType.launchPersistentContext(persistentContextName, {
                    devtools: process.env.DEVTOOLS !== 'false',
                    headless: process.env.HEADLESS !== 'false',
                    args: [
                        '--no-sandbox',
                        '--disable-gpu',
                        '--disable-notifications',
                        '--enable-automation',
                        '--no-first-run',
                        '--no-default-browser-check',
                        `--disable-extensions-except=${pathToExtension}`,
                        '--disable-web-security',
                        '--allow-insecure-localhost',
                        '--window-size=1920,1080',
                        '--allow-file-access-from-files',
                        '--disable-dev-shm-usage',
                        '--remote-allow-origins=*',
                        '--disable-features=IsolateOrigins, site-per-process'
                    ]
                });
                const getDefaultPage = async function () {
                    const pages = context.pages();
                    if (pages.length == 0) {
                        return await context.newPage();
                    }
                    return pages[0];
                };
                const walletURL = await getWalletURL(context);
                const page = await getDefaultPage();
                const mnemonic = fs.readFileSync('artifacts/wallets/' + walletName + '/mnemonic', 'utf-8').trim().split(' ');
                const expectedAddress = Bech32.fromString(fs.readFileSync('artifacts/wallets/' + walletName + '/testnet.bech32', 'utf-8').trim());
                let address = null;
                switch (walletType) {
                    case "lace":
                        address = await lace.configure(page, mnemonic, walletURL);
                        break;
                    case "nami":
                        address = await nami.configure(page, mnemonic, walletURL);
                        break;
                }
                if (address === null || address.toString() != expectedAddress.toString()) {
                    context.close();
                    throw new Error("Wallet configuration failed - wallet address is not the same as expected: given " + address + " != expected " + expectedAddress);
                }
                const wallet = { address, name: walletName, mnemonic, url: walletURL, type: walletType };
                const screen = { context, page, wallet, walletPopup: undefined };
                cache[walletType][walletName] = screen;
                return screen;
            };
            return screens;
        };
        this.globalConfig = options.parameters;
    }
    async init(contextOptions) {
        const screens = await this.mkLazyBrowsers();
        this.screens = screens;
    }
    getContractInfo(contractName) {
        const contractInfo = this.contracts[contractName];
        if (contractInfo === undefined) {
            throw new Error(`Contract ${contractName} is not defined`);
        }
        return contractInfo;
    }
    setContractInfo(contractName, contractInfo) {
        this.contracts[contractName] = contractInfo;
    }
    setWalletPopup(walletPopup) {
        this.getScreen().walletPopup = walletPopup;
    }
    async getWalletAddress(walletName) {
        // Just check if there is any wallet configured in the cache for a given user and grab the address
        if (walletName === undefined) {
            const { wallet } = this.getScreen();
            return wallet.address;
        }
        const fileContent = await fs.promises.readFile('artifacts/wallets/' + walletName + '/testnet.bech32', 'utf-8');
        return Bech32.fromString(fileContent.trim());
    }
    getScreen() {
        if (this.screen == null) {
            throw new Error("Screen is not set. Please use \"I use {walletName} {walletType} browser\" step to set it up.");
        }
        return this.screen;
    }
}
setWorldConstructor(ScenarioWorld);
