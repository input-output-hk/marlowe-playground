/*eslint-env node*/
import jsonBigInt from "json-bigint";
import safeEval from "safe-eval";
import context from "src/Language/Javascript/MarloweJS.ts";

const JSONbig = jsonBigInt({ useNativeBigInt: true });

export function eval_(left, right, model) {
  // include any libraries etc we want by providing a context. be careful!
  // here we can pass in our library for constructing contracts
  var monaco = globalThis.monaco;
  return monaco.languages.typescript
    .getTypeScriptWorker()
    .then(function (worker) {
      return worker(model.uri).then(function (proxy) {
        return proxy.getEmitOutput(model.uri.toString()).then((r) => {
          var javascript = r.outputFiles[0].text;
          try {
            var slices = javascript.split(/^.*from 'marlowe-js';$/gm);
            var takeSlice = 0;
            if (slices.length > 1) {
              takeSlice = 1;
            }
            var justCode = slices.slice(takeSlice).join("");
            let res = safeEval(justCode, context)();
            return right(JSONbig.stringify(res));
          } catch (error) {
            return left(error.toString());
          }
        });
      });
    });
}
