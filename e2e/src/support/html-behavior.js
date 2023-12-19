export const clickElement = async (locator) => {
    locator.click();
};
export const inputValue = async (locator, input) => {
    await locator.fill(input);
};
export const selectValue = async (locator, option) => {
    await locator.focus();
    locator.selectOption(option);
};
