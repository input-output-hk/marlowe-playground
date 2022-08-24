export const unsafeSnocArray = function (suffix) {
  return function (arr) {
    arr.push(...suffix);
    return arr;
  };
};

export const unsafeCons = function (a) {
  return function (arr) {
    arr.unshift(a);
    return arr;
  };
};

export const unsafeConsArray = function (prefix) {
  return function (arr) {
    arr.unshift(...prefix);
    return arr;
  };
};
export const unsafeSnoc = function (a) {
  return function (arr) {
    arr.push(a);
    return arr;
  };
};
