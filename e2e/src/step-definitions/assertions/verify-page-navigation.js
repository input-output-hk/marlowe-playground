import { Then } from '@cucumber/cucumber';
import { waitFor } from '../../support/wait-for-behavior.js';
import { currentPathMatchesPageId, } from '../../support/navigation-behavior.js';
Then(/^I should be on the "([^"]*)" page$/, async function (pageId) {
    const { page } = this.getScreen();
    const { globalConfig, } = this;
    await waitFor(() => currentPathMatchesPageId(page, pageId, globalConfig));
});
