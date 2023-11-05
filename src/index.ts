import "reflect-metadata";
import { ArgsDictionary, ResolverData } from "type-graphql";

export interface ResolverDataWithArgs<
  TArgsType extends ArgsDictionary,
  TContextType extends object = {}
> extends ResolverData<TContextType> {
  args: TArgsType;
  context: TContextType;
}

export type Rule<TContextType extends object = {}, TArgsType extends ArgsDictionary = {}> = (
  D: ResolverDataWithArgs<TArgsType, TContextType>
) => boolean | Promise<boolean>;

export type Rules<TContextType extends object = {}> =
  | Rule<TContextType>
  | RuleObject<TContextType>
  | Rules<TContextType>[];

export interface RuleObject<TContextType extends object = {}> {
  OR?: Rules<TContextType>[];
  AND?: Rules<TContextType>[];
  NOT?: Rules<TContextType>[];
}

export type AuthCheckerFn<
  TContextType extends object = {},
  TRoleType = Rules<TContextType>
> = (
  resolverData: ResolverData<TContextType>,
  roles: TRoleType
) => boolean | Promise<boolean>;

export type FAuthChecker<
  TContextType extends object = {},
  TRoleType = Rules<TContextType>
> = AuthCheckerFn<TContextType, TRoleType>;

function isRule(rules: Rules): rules is Rule {
  if (typeof rules === "function") {
    return true;
  }
  return false;
}

function isRulesArray(rules: Rules): rules is Rules[] {
  if (Array.isArray(rules)) {
    const isArrayOfRules = rules.reduce((isRuleAcc, rule) => {
      if (isRule(rule) || isRulesObject(rule)) {
        return isRuleAcc && true;
      }
      return false;
    }, true);
    return isArrayOfRules;
  }
  return false;
}

function isRulesObject(rules: Rules): rules is RuleObject[] {
  if ("OR" in rules || "AND" in rules || "NOT" in rules) {
    return true;
  } else {
    return false;
  }
}

export const authResolver: FAuthChecker = async (
  { root, args, context, info },
  rules: Rules
) => {
  if (isRule(rules)) {
    return await rules({ root, args, context, info });
  } else if (isRulesArray(rules)) {
    return await rules.reduce<boolean | Promise<boolean>>(async (acc, rule) => {
      return (await authResolver({ root, args, context, info }, rule)) && acc;
    }, Promise.resolve(true));
  } else if (isRulesObject(rules)) {
    const andRules = rules.AND
      ? await rules.AND.reduce<boolean | Promise<boolean>>(
        async (andAcc, rule) => {
          return (
            (await authResolver({ root, args, context, info }, rule)) &&
            andAcc
          );
        },
        Promise.resolve(true)
      )
      : true;
    const notRules = rules.NOT
      ? await rules.NOT.reduce<boolean | Promise<boolean>>(
        async (notAcc, rule) => {
          return (
            !(await authResolver({ root, args, context, info }, rule)) &&
            notAcc
          );
        },
        Promise.resolve(true)
      )
      : true;
    const orRules = rules.OR
      ? await rules.OR.reduce<boolean | Promise<boolean>>(
        async (andAcc, rule) => {
          return (
            (await authResolver({ root, args, context, info }, rule)) ||
            andAcc
          );
        },
        Promise.resolve(false)
      )
      : true;
    return andRules && orRules && notRules;
  }
  return true;
};

const authChecker: FAuthChecker = async (
  { root, args, context, info },
  rules
) => {
  return await authResolver({ root, args, context, info }, rules);
};

export default authChecker;
