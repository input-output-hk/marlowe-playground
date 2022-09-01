import { isBrowser, isNode, isJsDom } from "browser-or-node";

export const detectEnvironment = (browser) => (nodejs) => () => {
  if (isNode || isJsDom) {
    return nodejs;
  } else if (isBrowser) {
    return browser;
  } else {
    throw new Error("Unknown environment");
  }
};
