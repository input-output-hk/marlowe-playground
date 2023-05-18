import { Locator } from 'playwright';

export const clickElement = async(
  locator: Locator,
): Promise<void> => {
  await locator.click()
}

export const inputValue = async (
  locator: Locator,
  input: string,
): Promise<void> => {
  await locator.focus();
  await locator.fill(input);
}

export const selectValue = async(
  locator: Locator,
  option: string,
): Promise<void> => {
  await locator.focus();
  await locator.selectOption(option);
}