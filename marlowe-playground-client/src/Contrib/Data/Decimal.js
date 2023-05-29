import { Decimal, ROUND_UP, ROUND_DOWN } from "decimal.js";

export const _ROUND_UP = ROUND_UP;

export const _ROUND_DOWN = ROUND_DOWN;

export const setConfigurationImpl = (opts) => Decimal.set(opts);

export const getConfiguration = () => {
  const precision = Decimal.precision;
  const rounding = Decimal.rounding;
  return { precision, rounding };
};
