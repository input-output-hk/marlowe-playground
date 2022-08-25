export function _setItem(key, value) {
  window.localStorage.setItem(key, value);
}

export function _removeItem(key) {
  window.localStorage.removeItem(key);
}

export function _getItem(key) {
  return window.localStorage.getItem(key);
}

export function _listen(toRawStorageEvent, callback) {
  var onStorageEvent = function (event) {
    if (event.storageArea === window.localStorage) {
      var rawStorageEvent = toRawStorageEvent(
        event.key,
        event.oldValue,
        event.newValue
      );
      return callback(rawStorageEvent)();
    } else {
      return null;
    }
  };

  var canceler = function (error) {
    return function () {
      window.removeEventListener("storage", onStorageEvent, false);
    };
  };

  window.addEventListener("storage", onStorageEvent, false);
  return canceler;
}

export function _getItems(toRawStorageEvent) {
  var events = [];
  var i;

  for (i = 0; i < window.localStorage.length; i++) {
    var key = window.localStorage.key(i);
    var value = window.localStorage.getItem(key);
    var rawStorageEvent = toRawStorageEvent(key, null, value);
    events.push(rawStorageEvent);
  }

  return events;
}
