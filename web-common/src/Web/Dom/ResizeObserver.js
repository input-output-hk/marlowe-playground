export function resizeObserver(cb) {
  return function () {
    return new ResizeObserver(function (entries, observer) {
      return cb(entries)(observer)();
    });
  };
}

export function _observe(element) {
  return function (config) {
    return function (observer) {
      return function () {
        return observer.observe(element, config);
      };
    };
  };
}

export function unobserve(element) {
  return function (observer) {
    return function () {
      return observer.unobserve(element);
    };
  };
}

export function disconnect(observer) {
  return function () {
    return observer.disconnect();
  };
}
