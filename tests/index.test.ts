import "reflect-metadata";

import { authResolver, Rules } from "../src";

const ruleIsTrue = (): boolean => {
  console.log("sync ruleIsTrue");
  return true;
};

const promisedRuleIsTrue = (): Promise<boolean> => {
  console.log("async ruleIsTrue");
  return new Promise((resolve, reject) => {
    setTimeout(() => resolve(true), 100);
  });
};

const ruleIsFalse = (): boolean => {
  console.log("ruleIsFalse");
  return false;
};

const promisedRuleIsFalse = (): Promise<boolean> => {
  console.log("async ruleIsFalse");
  return new Promise((resolve, reject) => {
    setTimeout(() => resolve(false), 100);
  });
};

describe("can resolve AND functions", () => {
  it("returns false when one rule is false in async", async () => {
    const result = await authResolver({} as any, {
      AND: [promisedRuleIsFalse, promisedRuleIsTrue],
    });
    expect(result).toEqual(false);
  });
});

describe("can resolve AND functions", () => {
  it("returns false when one rule is false in sync", async () => {
    const result = await authResolver({} as any, {
      AND: [ruleIsFalse, ruleIsTrue],
    });
    expect(result).toEqual(false);
  });
});
