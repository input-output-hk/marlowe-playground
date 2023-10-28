"use strict";

export function copyToClipboardImpl(text, onFulfilled, onRejected) {
  return navigator.clipboard.writeText(text);
};
