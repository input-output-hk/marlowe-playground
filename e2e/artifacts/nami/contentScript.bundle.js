(()=>{var t={87757:(t,e,r)=>{t.exports=r(35666)},35666:t=>{var e=function(t){"use strict";var e,r=Object.prototype,n=r.hasOwnProperty,o="function"==typeof Symbol?Symbol:{},i=o.iterator||"@@iterator",a=o.asyncIterator||"@@asyncIterator",c=o.toStringTag||"@@toStringTag";function u(t,e,r){return Object.defineProperty(t,e,{value:r,enumerable:!0,configurable:!0,writable:!0}),t[e]}try{u({},"")}catch(t){u=function(t,e,r){return t[e]=r}}function s(t,e,r,n){var o=e&&e.prototype instanceof v?e:v,i=Object.create(o.prototype),a=new _(n||[]);return i._invoke=function(t,e,r){var n=h;return function(o,i){if(n===l)throw new Error("Generator is already running");if(n===p){if("throw"===o)throw i;return I()}for(r.method=o,r.arg=i;;){var a=r.delegate;if(a){var c=j(a,r);if(c){if(c===m)continue;return c}}if("next"===r.method)r.sent=r._sent=r.arg;else if("throw"===r.method){if(n===h)throw n=p,r.arg;r.dispatchException(r.arg)}else"return"===r.method&&r.abrupt("return",r.arg);n=l;var u=f(t,e,r);if("normal"===u.type){if(n=r.done?p:d,u.arg===m)continue;return{value:u.arg,done:r.done}}"throw"===u.type&&(n=p,r.method="throw",r.arg=u.arg)}}}(t,r,a),i}function f(t,e,r){try{return{type:"normal",arg:t.call(e,r)}}catch(t){return{type:"throw",arg:t}}}t.wrap=s;var h="suspendedStart",d="suspendedYield",l="executing",p="completed",m={};function v(){}function g(){}function w(){}var y={};y[i]=function(){return this};var b=Object.getPrototypeOf,x=b&&b(b(T([])));x&&x!==r&&n.call(x,i)&&(y=x);var L=w.prototype=v.prototype=Object.create(y);function E(t){["next","throw","return"].forEach((function(e){u(t,e,(function(t){return this._invoke(e,t)}))}))}function O(t,e){function r(o,i,a,c){var u=f(t[o],t,i);if("throw"!==u.type){var s=u.arg,h=s.value;return h&&"object"==typeof h&&n.call(h,"__await")?e.resolve(h.__await).then((function(t){r("next",t,a,c)}),(function(t){r("throw",t,a,c)})):e.resolve(h).then((function(t){s.value=t,a(s)}),(function(t){return r("throw",t,a,c)}))}c(u.arg)}var o;this._invoke=function(t,n){function i(){return new e((function(e,o){r(t,n,e,o)}))}return o=o?o.then(i,i):i()}}function j(t,r){var n=t.iterator[r.method];if(n===e){if(r.delegate=null,"throw"===r.method){if(t.iterator.return&&(r.method="return",r.arg=e,j(t,r),"throw"===r.method))return m;r.method="throw",r.arg=new TypeError("The iterator does not provide a 'throw' method")}return m}var o=f(n,t.iterator,r.arg);if("throw"===o.type)return r.method="throw",r.arg=o.arg,r.delegate=null,m;var i=o.arg;return i?i.done?(r[t.resultName]=i.value,r.next=t.nextLoc,"return"!==r.method&&(r.method="next",r.arg=e),r.delegate=null,m):i:(r.method="throw",r.arg=new TypeError("iterator result is not an object"),r.delegate=null,m)}function P(t){var e={tryLoc:t[0]};1 in t&&(e.catchLoc=t[1]),2 in t&&(e.finallyLoc=t[2],e.afterLoc=t[3]),this.tryEntries.push(e)}function k(t){var e=t.completion||{};e.type="normal",delete e.arg,t.completion=e}function _(t){this.tryEntries=[{tryLoc:"root"}],t.forEach(P,this),this.reset(!0)}function T(t){if(t){var r=t[i];if(r)return r.call(t);if("function"==typeof t.next)return t;if(!isNaN(t.length)){var o=-1,a=function r(){for(;++o<t.length;)if(n.call(t,o))return r.value=t[o],r.done=!1,r;return r.value=e,r.done=!0,r};return a.next=a}}return{next:I}}function I(){return{value:e,done:!0}}return g.prototype=L.constructor=w,w.constructor=g,g.displayName=u(w,c,"GeneratorFunction"),t.isGeneratorFunction=function(t){var e="function"==typeof t&&t.constructor;return!!e&&(e===g||"GeneratorFunction"===(e.displayName||e.name))},t.mark=function(t){return Object.setPrototypeOf?Object.setPrototypeOf(t,w):(t.__proto__=w,u(t,c,"GeneratorFunction")),t.prototype=Object.create(L),t},t.awrap=function(t){return{__await:t}},E(O.prototype),O.prototype[a]=function(){return this},t.AsyncIterator=O,t.async=function(e,r,n,o,i){void 0===i&&(i=Promise);var a=new O(s(e,r,n,o),i);return t.isGeneratorFunction(r)?a:a.next().then((function(t){return t.done?t.value:a.next()}))},E(L),u(L,c,"Generator"),L[i]=function(){return this},L.toString=function(){return"[object Generator]"},t.keys=function(t){var e=[];for(var r in t)e.push(r);return e.reverse(),function r(){for(;e.length;){var n=e.pop();if(n in t)return r.value=n,r.done=!1,r}return r.done=!0,r}},t.values=T,_.prototype={constructor:_,reset:function(t){if(this.prev=0,this.next=0,this.sent=this._sent=e,this.done=!1,this.delegate=null,this.method="next",this.arg=e,this.tryEntries.forEach(k),!t)for(var r in this)"t"===r.charAt(0)&&n.call(this,r)&&!isNaN(+r.slice(1))&&(this[r]=e)},stop:function(){this.done=!0;var t=this.tryEntries[0].completion;if("throw"===t.type)throw t.arg;return this.rval},dispatchException:function(t){if(this.done)throw t;var r=this;function o(n,o){return c.type="throw",c.arg=t,r.next=n,o&&(r.method="next",r.arg=e),!!o}for(var i=this.tryEntries.length-1;i>=0;--i){var a=this.tryEntries[i],c=a.completion;if("root"===a.tryLoc)return o("end");if(a.tryLoc<=this.prev){var u=n.call(a,"catchLoc"),s=n.call(a,"finallyLoc");if(u&&s){if(this.prev<a.catchLoc)return o(a.catchLoc,!0);if(this.prev<a.finallyLoc)return o(a.finallyLoc)}else if(u){if(this.prev<a.catchLoc)return o(a.catchLoc,!0)}else{if(!s)throw new Error("try statement without catch or finally");if(this.prev<a.finallyLoc)return o(a.finallyLoc)}}}},abrupt:function(t,e){for(var r=this.tryEntries.length-1;r>=0;--r){var o=this.tryEntries[r];if(o.tryLoc<=this.prev&&n.call(o,"finallyLoc")&&this.prev<o.finallyLoc){var i=o;break}}i&&("break"===t||"continue"===t)&&i.tryLoc<=e&&e<=i.finallyLoc&&(i=null);var a=i?i.completion:{};return a.type=t,a.arg=e,i?(this.method="next",this.next=i.finallyLoc,m):this.complete(a)},complete:function(t,e){if("throw"===t.type)throw t.arg;return"break"===t.type||"continue"===t.type?this.next=t.arg:"return"===t.type?(this.rval=this.arg=t.arg,this.method="return",this.next="end"):"normal"===t.type&&e&&(this.next=e),m},finish:function(t){for(var e=this.tryEntries.length-1;e>=0;--e){var r=this.tryEntries[e];if(r.finallyLoc===t)return this.complete(r.completion,r.afterLoc),k(r),m}},catch:function(t){for(var e=this.tryEntries.length-1;e>=0;--e){var r=this.tryEntries[e];if(r.tryLoc===t){var n=r.completion;if("throw"===n.type){var o=n.arg;k(r)}return o}}throw new Error("illegal catch attempt")},delegateYield:function(t,r,n){return this.delegate={iterator:T(t),resultName:r,nextLoc:n},"next"===this.method&&(this.arg=e),m}},t}(t.exports);try{regeneratorRuntime=e}catch(t){Function("r","regeneratorRuntime = r")(e)}}},e={};function r(n){var o=e[n];if(void 0!==o)return o.exports;var i=e[n]={exports:{}};return t[n](i,i.exports,r),i.exports}r.n=t=>{var e=t&&t.__esModule?()=>t.default:()=>t;return r.d(e,{a:e}),e},r.d=(t,e)=>{for(var n in e)r.o(e,n)&&!r.o(t,n)&&Object.defineProperty(t,n,{enumerable:!0,get:e[n]})},r.o=(t,e)=>Object.prototype.hasOwnProperty.call(t,e),(()=>{"use strict";function t(t,e,r){return e in t?Object.defineProperty(t,e,{value:r,enumerable:!0,configurable:!0,writable:!0}):t[e]=r,t}function e(t,e){var r=Object.keys(t);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(t);e&&(n=n.filter((function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable}))),r.push.apply(r,n)}return r}function n(r){for(var n=1;n<arguments.length;n++){var o=null!=arguments[n]?arguments[n]:{};n%2?e(Object(o),!0).forEach((function(e){t(r,e,o[e])})):Object.getOwnPropertyDescriptors?Object.defineProperties(r,Object.getOwnPropertyDescriptors(o)):e(Object(o)).forEach((function(t){Object.defineProperty(r,t,Object.getOwnPropertyDescriptor(o,t))}))}return r}function o(t,e,r,n,o,i,a){try{var c=t[i](a),u=c.value}catch(t){return void r(t)}c.done?e(u):Promise.resolve(u).then(n,o)}function i(t){return function(){var e=this,r=arguments;return new Promise((function(n,i){var a=t.apply(e,r);function c(t){o(a,n,i,c,u,"next",t)}function u(t){o(a,n,i,c,u,"throw",t)}c(void 0)}))}}function a(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}var c,u,s=r(87757),f=r.n(s),h="nami-wallet",d="extension",l="webpage",p="isWhitelisted",m="enable",v="isEnabled",g="requestData",w="returnData",y={InvalidRequest:{code:-1,info:"Inputs do not conform to this spec or are otherwise invalid."},InternalError:{code:-2,info:"An error occurred during execution of this API call."},Refused:{code:-3,info:"The request was refused due to lack of access - e.g. wallet disconnects."},AccountChange:{code:-4,info:"The account has changed. The dApp should call `wallet.enable()` to reestablish connection to the new account. The wallet should not ask for confirmation as the user was the one who initiated the account change in the first place."}},b=function t(){var e=this;a(this,t),this.requestData=function(){return new Promise(function(){var t=i(f().mark((function t(r,n){var o;return f().wrap((function(t){for(;;)switch(t.prev=t.next){case 0:return t.next=2,new Promise((function(t,e){return chrome.tabs.getCurrent((function(e){return t(e.id)}))}));case 2:return e.tabId=t.sent,o=e,e.port.onMessage.addListener((function t(e){o.port.onMessage.removeListener(t),r(e)})),t.t0=e.port,t.next=8,e.tabId;case 8:t.t1=t.sent,t.t2=g,t.t3={tabId:t.t1,method:t.t2},t.t0.postMessage.call(t.t0,t.t3);case 12:case"end":return t.stop()}}),t)})));return function(e,r){return t.apply(this,arguments)}}())},this.returnData=function(){var t=i(f().mark((function t(r){var n,o;return f().wrap((function(t){for(;;)switch(t.prev=t.next){case 0:return n=r.data,o=r.error,t.t0=e.port,t.t1=n,t.t2=o,t.t3=w,t.next=7,e.tabId;case 7:t.t4=t.sent,t.t5={data:t.t1,error:t.t2,method:t.t3,tabId:t.t4},t.t0.postMessage.call(t.t0,t.t5);case 10:case"end":return t.stop()}}),t)})));return function(e){return t.apply(this,arguments)}}(),this.port=chrome.runtime.connect({name:"internal-background-popup-communication"}),this.tabId=new Promise((function(t,e){return chrome.tabs.getCurrent((function(e){return t(e.id)}))}))},x=function t(){var e=this;a(this,t),this.add=function(t,r){e._methodList[t]=r},this.listen=function(){chrome.runtime.onMessage.addListener((function(t,r,n){return t.sender===l&&e._methodList[t.method](t,n),!0}))},this._methodList={}},L={sendToBackground:(c=i(f().mark((function t(e){return f().wrap((function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",new Promise((function(t,r){return chrome.runtime.sendMessage(n(n({},e),{},{target:h,sender:l}),(function(e){return t(e)}))})));case 1:case"end":return t.stop()}}),t)}))),function(t){return c.apply(this,arguments)}),sendToContent:function(t){var e=t.method,r=t.data;return new Promise((function(t,n){var o=Math.random().toString(36).substr(2,9);window.addEventListener("message",(function e(r){var i=r.data;"object"==typeof i&&null!==i&&i.target&&i.target===h&&i.id&&i.id===o&&i.sender&&i.sender===d&&(window.removeEventListener("message",e),i.error?n(i.error):t(i))})),window.postMessage({method:e,data:r,target:h,sender:l,id:o},window.origin)}))},sendToPopupInternal:function(t,e){return new Promise((function(r,n){chrome.runtime.onConnect.addListener((function n(o){o.onMessage.addListener((function i(a){a.tabId===t.id&&(a.method===g&&o.postMessage(e),a.method===w&&r(a),chrome.tabs.onRemoved.addListener((function e(a){t.id===a&&(r({target:h,sender:d,error:y.Refused}),chrome.runtime.onConnect.removeListener(n),o.onMessage.removeListener(i),chrome.tabs.onRemoved.removeListener(e))})))}))}))}))},createInternalController:function(){return new b},createProxyController:function(){chrome.runtime.onMessage.addListener(function(){var t=i(f().mark((function t(e){var r,n;return f().wrap((function(t){for(;;)switch(t.prev=t.next){case 0:if("object"==typeof e&&null!==e&&e.target&&e.target===h&&e.sender&&e.sender===d&&e.event){t.next=2;break}return t.abrupt("return");case 2:return t.next=4,L.sendToBackground({method:p,origin:window.origin});case 4:if((r=t.sent)&&!r.error){t.next=7;break}return t.abrupt("return");case 7:n=new CustomEvent("".concat(h).concat(e.event),{detail:e.data}),window.dispatchEvent(n);case 9:case"end":return t.stop()}}),t)})));return function(e){return t.apply(this,arguments)}}()),window.addEventListener("message",function(){var t=i(f().mark((function t(e){var r,o;return f().wrap((function(t){for(;;)switch(t.prev=t.next){case 0:if("object"==typeof(r=e.data)&&null!==r&&r.target&&r.target===h&&r.sender&&r.sender===l){t.next=3;break}return t.abrupt("return");case 3:if(r.origin=window.origin,r.method!==m&&r.method!==v){t.next=7;break}return L.sendToBackground(n({},r)).then((function(t){return window.postMessage(t)})),t.abrupt("return");case 7:return t.next=9,L.sendToBackground({method:p,origin:window.origin});case 9:if((o=t.sent)&&!o.error){t.next=13;break}return window.postMessage(n(n({},o),{},{id:r.id})),t.abrupt("return");case 13:return t.next=15,L.sendToBackground(r).then((function(t){window.postMessage(t)}));case 15:case"end":return t.stop()}}),t)})));return function(e){return t.apply(this,arguments)}}())},createBackgroundController:function(){return new x}};(function(){var t=document.documentElement.nodeName,e=!t||"html"===t.toLowerCase(),r=window.document.docType,n=!r||"html"===r.name;return e&&n})&&((u=document.createElement("script")).async=!1,u.src=chrome.runtime.getURL("injected.bundle.js"),u.onload=function(){this.remove()},(document.head||document.documentElement).appendChild(u),L.createProxyController())})()})();