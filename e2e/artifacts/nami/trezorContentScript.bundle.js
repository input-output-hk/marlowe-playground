(()=>{var n=chrome.runtime.connect({name:"trezor-connect"});n.onMessage.addListener((function(n){window.postMessage(n,window.location.origin)})),n.onDisconnect.addListener((function(){n=null})),window.addEventListener("message",(function(e){n&&e.source===window&&e.data&&n.postMessage({data:e.data})}))})();