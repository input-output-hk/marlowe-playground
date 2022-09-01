export function _setItem(key, value) {
  window.sessionStorage.setItem(key, value);
}

export function _getItem(key) {
  return window.sessionStorage.getItem(key);
}
