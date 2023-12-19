import { When } from '@cucumber/cucumber';
import { datetoTimeout, } from "@marlowe.io/language-core-v1";
import { MarloweJSON } from "@marlowe.io/adapter/codec";
const mkSimpleDeposit = (address) => {
    const twentyMinutesInMilliseconds = 20 * 60 * 1000;
    const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
    return {
        timeout: inTwentyMinutes,
        timeout_continuation: "close",
        when: [
            { case: {
                    party: { address: address.toString() },
                    deposits: 1n,
                    of_token: { currency_symbol: "", token_name: "" },
                    into_account: { address: address.toString() }
                },
                then: "close",
            },
        ]
    };
};
const mkSimpleChoice = (address) => {
    const twentyMinutesInMilliseconds = 20 * 60 * 1000;
    const inTwentyMinutes = datetoTimeout(new Date(Date.now() + twentyMinutesInMilliseconds));
    return {
        timeout: inTwentyMinutes,
        timeout_continuation: "close",
        when: [
            { case: {
                    choose_between: [{
                            from: 1n,
                            to: 2n
                        }],
                    for_choice: {
                        choice_owner: { address: address.toString() },
                        choice_name: "simpleChoice",
                    }
                },
                then: "close",
            },
        ]
    };
};
const mkTimedOutSimpleChoice = (address) => {
    const twentyMinutesInMilliseconds = 20 * 60 * 1000;
    const inTwentyMinutes = datetoTimeout(new Date(Date.now() - twentyMinutesInMilliseconds));
    return {
        timeout: inTwentyMinutes,
        timeout_continuation: "close",
        when: [
            { case: {
                    choose_between: [{
                            from: 1n,
                            to: 2n
                        }],
                    for_choice: {
                        choice_owner: { address: address.toString() },
                        choice_name: "simpleChoice",
                    }
                },
                then: "close",
            },
        ]
    };
};
const mkSimpleNotify = () => {
    const twentyMinutesInMilliseconds = 20 * 60 * 1000;
    const inTwentyMinutes = datetoTimeout(new Date(Date.now() - twentyMinutesInMilliseconds));
    return {
        timeout: inTwentyMinutes,
        timeout_continuation: "close",
        when: [
            { case: {
                    notify_if: true,
                },
                then: "close",
            },
        ]
    };
};
// // And I generate the contract "SimpleDeposit" and write it to "/tmp/deposit.json"
When(/^I generate the contract "([^"]*)" and write it to "([^"]*)"/, async function (contractName, fileName) {
    const walletAddress = await this.getWalletAddress();
    const { globalStateManager } = this;
    switch (contractName) {
        case "SimpleDeposit":
            const contract1 = mkSimpleDeposit(walletAddress);
            globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract1, null, 4));
            break;
        case "SimpleChoice":
            const contract2 = mkSimpleChoice(walletAddress);
            globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract2, null, 4));
            break;
        case "TimedOutSimpleChoice":
            const contract3 = mkTimedOutSimpleChoice(walletAddress);
            globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract3, null, 4));
            break;
        case "SimpleNotify":
            const contract4 = mkSimpleNotify();
            globalStateManager.appendValue(fileName, MarloweJSON.stringify(contract4, null, 4));
            break;
        default:
            throw new Error("Unknown contract type: " + contractName);
    }
});
// And I generate "SimpleNotify" and call it "notify"
When(/^I generate "([^"]*)" and call it "([^"]*)"$/, async function (contractName, contractNickname) {
    const walletAddress = await this.getWalletAddress();
    let contract;
    switch (contractName) {
        case "SimpleDeposit":
            contract = mkSimpleDeposit(walletAddress);
            break;
        case "SimpleChoice":
            contract = mkSimpleChoice(walletAddress);
            break;
        case "TimedOutSimpleChoice":
            contract = mkTimedOutSimpleChoice(walletAddress);
            break;
        case "SimpleNotify":
            contract = mkSimpleNotify();
            break;
        default:
            throw new Error("Unknown contract type: " + contractName);
    }
    this.setContractInfo(contractNickname, { contract: contract, contractId: undefined });
});
