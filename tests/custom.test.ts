import {createCustomAuthChecker} from "../src";
import {ruleIsFalse, ruleIsTrue} from "./utils/rules";

describe("can create custom auth checker", () => {
    it("returns an auth checker", async () => {
        const customAuthChecker = createCustomAuthChecker((rules) => ({
            OR: [
                ruleIsTrue,
                { AND: [rules] }
            ]
        }));

        expect(customAuthChecker).toBeInstanceOf(Function);
    });
});

describe("can resolve custom auth checker", () => {
    it("returns false when special rule is false using AND",  async() => {
        const customAuthChecker = createCustomAuthChecker((rules) => ({
            AND: [
                ruleIsFalse,
                rules,
            ]
        }));

        const result = await customAuthChecker({} as any, ruleIsTrue);
        expect(result).toEqual(false);
    });
})

describe("can resolve custom auth checker", () => {
  it("returns true when special rule is true using OR", async() => {
      const customAuthChecker = createCustomAuthChecker((rules) => ({
      OR: [
          ruleIsTrue,
          { AND: [rules] }
      ]
    }));

    const result = await customAuthChecker({} as any, ruleIsFalse);
    expect(result).toEqual(true);
  })
})
