import "reflect-metadata";
import { ArgsDictionary, ResolverData } from "type-graphql";
export interface ResolverDataWithArgs<TArgsType extends ArgsDictionary, TContextType extends object = {}> extends ResolverData<TContextType> {
    args: TArgsType;
    context: TContextType;
}
export type Rule<TContextType extends object = {}, TArgsType extends ArgsDictionary = {}> = (D: ResolverDataWithArgs<TArgsType, TContextType>) => boolean | Promise<boolean>;
export type Rules<TContextType extends object = {}> = Rule<TContextType> | RuleObject<TContextType> | Rules<TContextType>[];
export interface RuleObject<TContextType extends object = {}> {
    OR?: Rules<TContextType>[];
    AND?: Rules<TContextType>[];
    NOT?: Rules<TContextType>[];
}
export type AuthCheckerFn<TContextType extends object = {}, TRoleType = Rules<TContextType>> = (resolverData: ResolverData<TContextType>, roles: TRoleType) => boolean | Promise<boolean>;
export type FAuthChecker<TContextType extends object = {}, TRoleType = Rules<TContextType>> = AuthCheckerFn<TContextType, TRoleType>;
export declare const authResolver: FAuthChecker;
declare const authChecker: FAuthChecker;
export default authChecker;
