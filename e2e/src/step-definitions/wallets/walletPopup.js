import { grabPopup } from '../popup.js';
// TODO: move the module level api which we have
// here and turn this into an interface.
export class WalletPopup {
    constructor(page) {
        this.page = page;
    }
    getPage() {
        if (!this.page.isClosed()) {
            return this.page;
        }
    }
    static async fromTrigger(page, triggerPopup) {
        const walletPopup = await grabPopup(page, triggerPopup);
        await walletPopup.reload();
        return new WalletPopup(walletPopup);
    }
}
