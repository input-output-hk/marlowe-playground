export function _close(window) {
  window.close();
}

export function _postMessage(message, targetOrigin, window) {
  window.postMessage(message, targetOrigin);
}

export function _matchMedia(query, window) {
  return window.matchMedia(query);
}

export function _matches(mediaQueryList) {
  return mediaQueryList.matches;
}
