import "../grammar.ne";
import jsonBigInt from "json-bigint";

// We need to patch the JSON.stringify in order for BigInt serialization to work.
const { stringify, parse } = jsonBigInt({ useNativeBigInt: true });

JSON.stringify = stringify;
JSON.parse = parse;

require("../output/Test.Main").main();
