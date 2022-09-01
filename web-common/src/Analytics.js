/*eslint-env node*/
/*global exports gtag, analytics*/

export function trackEvent_(action, category, label, value) {
  // Google Analytics, the default.
  if (typeof gtag !== "undefined") {
    gtag("event", action, {
      event_category: category,
      event_label: label,
      value: value,
    });
  }
}

export function trackSegmentEvent_(action, payload) {
  // Segment.com.
  if (typeof analytics !== "undefined") {
    analytics.track(action, payload);
  }
}
