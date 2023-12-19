import { getCurrentPageId } from './navigation-behavior.js';
export const getElementLocator = (page, elementKey, globalConfig) => {
    const currentPage = getCurrentPageId(page, globalConfig);
    const { pageElementMappings } = globalConfig;
    return pageElementMappings[currentPage]?.[elementKey] || pageElementMappings[currentPage]?.common?.[elementKey];
};
