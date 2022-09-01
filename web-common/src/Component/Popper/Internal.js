import arrowModifier from "@popperjs/core/lib/modifiers/arrow.js";
import popperOffsets from "@popperjs/core/lib/modifiers/popperOffsets.js";
import computeStylesModifier from "@popperjs/core/lib/modifiers/computeStyles.js";
import applyStylesModifier from "@popperjs/core/lib/modifiers/applyStyles.js";
import eventListenerModifier from "@popperjs/core/lib/modifiers/eventListeners.js";
import offsetModifier from "@popperjs/core/lib/modifiers/offset.js";
import preventOverflowModifier from "@popperjs/core/lib/modifiers/preventOverflow.js";
import flipPlacementModifier from "@popperjs/core/lib/modifiers/flip.js";
import { createPopper } from "@popperjs/core/lib/createPopper.js";

export function _arrow(element, padding) {
  return Object.assign({}, arrowModifier, {
    options: { element, padding },
  });
}

export function _popperOffsets() {
  return popperOffsets;
}

export function _computeStyles(options) {
  return Object.assign({}, computeStylesModifier, {
    options,
  });
}

export function _applyStyles() {
  return applyStylesModifier;
}

export function _eventListeners(options) {
  return Object.assign({}, eventListenerModifier, {
    options,
  });
}

export function _offset(options) {
  return Object.assign({}, offsetModifier, {
    options,
  });
}

export function _preventOverflow(options) {
  return Object.assign({}, preventOverflowModifier, {
    options,
  });
}

export function _flipPlacement(options) {
  return Object.assign({}, flipPlacementModifier, {
    options,
  });
}

export function _createPopper(reference, popper, options) {
  return createPopper(reference, popper, options);
}

export function _destroyPopper(instance) {
  instance.destroy();
}

export function _forceUpdate(instance) {
  instance.forceUpdate();
}
