import { Page } from 'playwright';
import { GlobalConfig, PageId } from '../env/global.js';

export const navigateToPage = async (
  page: Page,
  pageId: PageId,
  { pagesConfig, hostsConfig }: GlobalConfig
): Promise<void> => {
  const {
    UI_AUTOMATION_HOST: environmentId = 'localhost',
  } = process.env

  const hostPath = hostsConfig[`${environmentId}`];
  const url = new URL(hostPath);
  const pagesConfigItem = pagesConfig[pageId];

  if (!!pagesConfigItem.query) {
    url.search = pagesConfigItem.query;
  }
  url.pathname = pagesConfigItem.route;
  await page.goto(url.href);
}

const pathMatchesPageId = (
  pathname: string,
  hash: string,
  pageId: PageId,
  { pagesConfig }: GlobalConfig
): boolean => {
  const currentPath = `${pathname}${hash}`;
  const pagesConfigItem = pagesConfig[pageId]
  const pageRegexString = pagesConfigItem.regex;
  const pageRegex = new RegExp(pageRegexString);
  return pageRegex.test(currentPath);
}

export const currentPathMatchesPageId = (
  page: Page,
  pageId: PageId,
  globalConfig: GlobalConfig,
): boolean => {
  const { pathname, hash } = new URL(page.url());
  const url = new URL(page.url());
  return pathMatchesPageId(pathname, hash, pageId, globalConfig);
}

export const getCurrentPageId = (
  page: Page,
  globalConfig: GlobalConfig,
): PageId => {
  const { pagesConfig } = globalConfig;
  const pageConfigPageIds = Object.keys(pagesConfig);
  const { pathname, hash } = new URL(page.url());
  const currentPageId = pageConfigPageIds.find(pageId =>
    pathMatchesPageId(pathname, hash, pageId, globalConfig)
  );

  if (!currentPageId) {
    throw Error(
      `Failed to get page name from current route ${pathname}/${hash}, \
      possible pages: ${JSON.stringify((pagesConfig))}`
    )
  }

  return currentPageId;
}
