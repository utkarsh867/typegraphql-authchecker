"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.authResolver = void 0;
require("reflect-metadata");
function isRule(rules) {
    if (typeof rules === "function") {
        return true;
    }
    return false;
}
function isRulesArray(rules) {
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
function isRulesObject(rules) {
    if ("OR" in rules || "AND" in rules || "NOT" in rules) {
        return true;
    }
    else {
        return false;
    }
}
const authResolver = ({ root, args, context, info }, rules) => __awaiter(void 0, void 0, void 0, function* () {
    if (isRule(rules)) {
        return yield rules({ root, args, context, info });
    }
    else if (isRulesArray(rules)) {
        return yield rules.reduce((acc, rule) => __awaiter(void 0, void 0, void 0, function* () {
            return (yield (0, exports.authResolver)({ root, args, context, info }, rule)) && acc;
        }), Promise.resolve(true));
    }
    else if (isRulesObject(rules)) {
        const andRules = rules.AND
            ? yield rules.AND.reduce((andAcc, rule) => __awaiter(void 0, void 0, void 0, function* () {
                return ((yield (0, exports.authResolver)({ root, args, context, info }, rule)) &&
                    andAcc);
            }), Promise.resolve(true))
            : true;
        const notRules = rules.NOT
            ? yield rules.NOT.reduce((notAcc, rule) => __awaiter(void 0, void 0, void 0, function* () {
                return (!(yield (0, exports.authResolver)({ root, args, context, info }, rule)) &&
                    notAcc);
            }), Promise.resolve(true))
            : true;
        const orRules = rules.OR
            ? yield rules.OR.reduce((andAcc, rule) => __awaiter(void 0, void 0, void 0, function* () {
                return ((yield (0, exports.authResolver)({ root, args, context, info }, rule)) ||
                    andAcc);
            }), Promise.resolve(false))
            : true;
        return andRules && orRules && notRules;
    }
    return true;
});
exports.authResolver = authResolver;
const authChecker = ({ root, args, context, info }, rules) => __awaiter(void 0, void 0, void 0, function* () {
    return yield (0, exports.authResolver)({ root, args, context, info }, rules);
});
exports.default = authChecker;
