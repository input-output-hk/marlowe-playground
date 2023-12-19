import { When } from '@cucumber/cucumber';
import { datetoTimeout, } from "@marlowe.io/language-core-v1";
const mkDoubleDepositAndNotify = (address1, address2) => {
    const twentyMinutesInMilliseconds = 20 * 60 * 1000;
    const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
    const notifyContinuation = {
        timeout: inTwentyMinutes,
        timeout_continuation: "close",
        "when": [
            {
                "then": "close",
                "case": {
                    "notify_if": true
                }
            }
        ],
    };
    return {
        timeout: inTwentyMinutes,
        timeout_continuation: "close",
        when: [
            { case: {
                    party: { address: address1.toString() },
                    deposits: 1000000n,
                    of_token: { currency_symbol: "", token_name: "" },
                    into_account: { address: address1.toString() }
                },
                then: notifyContinuation,
            },
            { case: {
                    party: { address: address2.toString() },
                    deposits: 2000000n,
                    of_token: { currency_symbol: "", token_name: "" },
                    into_account: { address: address2.toString() }
                },
                then: notifyContinuation,
            },
        ]
    };
};
When(/^I generate "DoubleDepositAndNotify" contract with "([^"]*)" as a first depositor and "([^"]*)" as a second depositor and call it "([^"]*)"$/, async function (first, second, contractNickname) {
    const firstAddress = await this.getWalletAddress(first);
    const secondAddress = await this.getWalletAddress(second);
    const contract = mkDoubleDepositAndNotify(firstAddress, secondAddress);
    this.setContractInfo(contractNickname, { contract: contract, contractId: undefined });
});
