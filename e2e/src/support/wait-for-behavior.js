export const waitFor = async (predicate, options) => {
    const { timeout = 60000, wait = 200 } = options || {};
    const sleep = (ms) => new Promise(resolve => setTimeout(resolve, ms));
    const startDate = new Date();
    const onTimeout = options?.onTimeout || (() => {
        throw new Error(`Wait time of ${timeout}ms exceeded`);
    });
    while (new Date().getTime() - startDate.getTime() < timeout) {
        const result = await predicate();
        if (result) {
            return result;
        }
        await sleep(wait);
        if (!options?.label) {
            console.log(`Waiting ${wait}ms`);
        }
        else {
            console.log(`Waiting ${wait}ms for ${options.label}`);
        }
    }
    return onTimeout();
};
export const waitForSelectorVisible = async (page, selector, timeout) => {
    console.log(`Waiting for ${selector} to be visible`);
    const locator = page.locator(selector);
    await locator.waitFor({ state: "visible", timeout: timeout !== undefined ? timeout : 10000 });
    return locator;
};
export const waitForRoleVisible = async (page, role, name, timeout) => {
    console.log(`Waiting for ${role} with ${name} text to be visible`);
    const locator = page.getByRole(role, { name, exact: true });
    await locator.waitFor({ state: "visible", timeout: timeout !== undefined ? timeout : 10000 });
    return locator;
};
export const waitForTestIdVisible = async (page, testId, timeout) => {
    console.log(`Waiting for ${testId} to be visible`);
    const locator = page.getByTestId(testId);
    await locator.waitFor({ state: "visible", timeout: timeout !== undefined ? timeout : 10000 });
    return locator;
};
