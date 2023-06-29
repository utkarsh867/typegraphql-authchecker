import "reflect-metadata";
import { ArgsDictionary, ResolverData } from "type-graphql";
export interface ResolverDataWithArgs<TArgsType extends ArgsDictionary, TContextType = {}> extends ResolverData<TContextType> {
    args: TArgsType;
    context: TContextType;
}
export declare type Rule<TContextType = {}, TArgsType extends ArgsDictionary = {}> = (D: ResolverDataWithArgs<TArgsType, TContextType>) => boolean | Promise<boolean>;
export declare type Rules<TContextType = {}> = Rule<TContextType> | RuleObject<TContextType> | Rules<TContextType>[];
export interface RuleObject<TContextType = {}> {
    OR?: Rules<TContextType>[];
    AND?: Rules<TContextType>[];
    NOT?: Rules<TContextType>[];
}
export declare type AuthCheckerFn<TContextType = {}, TRoleType = Rules<TContextType>> = (resolverData: ResolverData<TContextType>, roles: TRoleType) => boolean | Promise<boolean>;
export declare type FAuthChecker<TContextType = {}, TRoleType = Rules<TContextType>> = AuthCheckerFn<TContextType, TRoleType>;
export declare const authResolver: FAuthChecker;
declare const authChecker: FAuthChecker;
export default authChecker;
