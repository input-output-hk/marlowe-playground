export function _pretty(str) {
  return JSON.stringify(JSON.parse(str), null, 2);
}

export function unsafeStringify(a) {
  return JSON.stringify(a);
}
