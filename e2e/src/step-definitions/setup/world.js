import playwright from 'playwright';
import { env } from '../../env/parseEnv.js';
import { World, setWorldConstructor } from "@cucumber/cucumber";
import GlobalStateManager from "../../support/globalStateManager.js";
export class ScenarioWorld extends World {
    constructor(options) {
        super(options);
        this.globalStateManager = new GlobalStateManager();
        this.newContext = async () => {
            const automationBrowsers = ['chromium', 'firefox', 'webkit'];
            const automationBrowser = env('UI_AUTOMATION_BROWSER');
            const pathToLaceExtension = env('LACE_WALLET_EXTENSION_PATH');
            const pathToNamiExtension = env('NAMI_WALLET_EXTENSION_PATH');
            const browserType = playwright[automationBrowser];
            const context = await browserType.launchPersistentContext('', {
                devtools: process.env.DEVTOOLS !== 'false',
                headless: process.env.HEADLESS !== 'false',
                args: [
                    '--no-sandbox',
                    '--disable-gpu',
                    '--disable-notifications',
                    '--enable-automation',
                    '--no-first-run',
                    '--no-default-browser-check',
                    `--disable-extensions-except=${pathToLaceExtension},${pathToNamiExtension}`,
                    '--disable-web-security',
                    '--allow-insecure-localhost',
                    '--window-size=1920,1080',
                    '--allow-file-access-from-files',
                    '--disable-dev-shm-usage',
                    '--remote-allow-origins=*',
                    '--disable-features=IsolateOrigins, site-per-process'
                ]
            });
            return context;
        };
        this.globalConfig = options.parameters;
    }
    async init(contextOptions) {
        await this.screen?.page?.close();
        await this.screen?.context?.close();
        const context = await this.newContext();
        const page = await context.newPage();
        this.screen = { context, page };
        return this.screen;
    }
}
setWorldConstructor(ScenarioWorld);
