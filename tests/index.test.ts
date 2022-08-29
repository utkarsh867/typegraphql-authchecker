import { authResolver } from "../src";
import {promisedRuleIsFalse, promisedRuleIsTrue, ruleIsFalse, ruleIsTrue} from "./utils/rules";

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
