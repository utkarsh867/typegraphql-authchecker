import {Rule} from "../../src";

export const ruleIsTrue: Rule = (): boolean => {
    console.log("sync ruleIsTrue");
    return true;
};

export const promisedRuleIsTrue: Rule = (): Promise<boolean> => {
    console.log("async ruleIsTrue");
    return new Promise((resolve, reject) => {
        setTimeout(() => resolve(true), 100);
    });
};

export const ruleIsFalse: Rule = (): boolean => {
    console.log("ruleIsFalse");
    return false;
};

export const promisedRuleIsFalse: Rule = (): Promise<boolean> => {
    console.log("async ruleIsFalse");
    return new Promise((resolve, reject) => {
        setTimeout(() => resolve(false), 100);
    });
};
