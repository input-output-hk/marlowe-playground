export const navigateToPage = async (page, pageId, { pagesConfig, hostsConfig }) => {
    const { UI_AUTOMATION_HOST: environmentId = 'localhost', } = process.env;
    const hostPath = hostsConfig[`${environmentId}`];
    const url = new URL(hostPath);
    const pagesConfigItem = pagesConfig[pageId];
    if (!!pagesConfigItem.query) {
        url.search = pagesConfigItem.query;
    }
    url.pathname = pagesConfigItem.route;
    await page.goto(url.href);
};
const pathMatchesPageId = (pathname, hash, pageId, { pagesConfig }) => {
    const currentPath = `${pathname}${hash}`;
    const pagesConfigItem = pagesConfig[pageId];
    const pageRegexString = pagesConfigItem.regex;
    const pageRegex = new RegExp(pageRegexString);
    return pageRegex.test(currentPath);
};
export const currentPathMatchesPageId = (page, pageId, globalConfig) => {
    const { pathname, hash } = new URL(page.url());
    const url = new URL(page.url());
    return pathMatchesPageId(pathname, hash, pageId, globalConfig);
};
export const getCurrentPageId = (page, globalConfig) => {
    const { pagesConfig } = globalConfig;
    const pageConfigPageIds = Object.keys(pagesConfig);
    const { pathname, hash } = new URL(page.url());
    const currentPageId = pageConfigPageIds.find(pageId => pathMatchesPageId(pathname, hash, pageId, globalConfig));
    if (!currentPageId) {
        throw Error(`Failed to get page name from current route ${pathname}/${hash}, \
      possible pages: ${JSON.stringify((pagesConfig))}`);
    }
    return currentPageId;
};
