# TypeGraphQL AuthChecker

Write rules for `Authorized()` decorator of [TypeGraphQL](https://github.com/MichalLytek/type-graphql) resolvers.

## Introduction

I wanted to make rules for using Authorized decorator in TypeGraphQL resolvers just like I used to make them for [graphql-shield](https://github.com/maticzav/graphql-shield).

TypeGraphQL allows creating custom authCheckers to use authorization rules. However, it seems limited by the fact that it checks against `roles` and not rules that can be defined by the developer. Additionally, there seems to be no way to define conditionals such as `AND` or `OR` in the authChecker unless implemented by the user from scratch.

This library allows you to define rules for `Authorised()` decorator in TypeGraphQL resolvers.

## Usage

Import the `authChecker` from `typegraphql-authchecker`. Then use the `authChecker` as [TypeGraphQL's custom `authChecker`](https://typegraphql.com/docs/authorization.html#how-to-use).

```js
import authChecker from "typegraphql-authchecker";

// Use the authChecker as the custom authChecker for TypeGraphQL
buildSchema({
  resolvers,
  emitSchemaFile: true,
  authChecker: authChecker,
}).then((schema) => {});
```

Now, you can start creating rules and start using them!

For example, to check if user is authenticated:

```js
import { Rule } from "typegraphql-authchecker";
import { Context } from "../context";

export const isAuthenticated: Rule<Context> = ({ context }) => {
  if (context.user) {
    return true;
  }
  return false;
};
```

`Context` in the `Rule<Context>` is the type of your GraphQL server context.

Use this rule for a resolver:

```js
@Resolver()
export class UserResolver {
  @Authorized(isAuthenticated)
  @Query((returns) => User)
  async me(@Ctx() ctx: Context) {
    return ctx.user;
  }
}
```

### Using `AND`, `OR` and `NOT` conditionals

Rules can be combined using `AND`, `OR` and `NOT` conditionals to perform complex authorization checks.

For example, by default, this will check if user is authenticated `and` if user is an admin.

```js
@Authorized([isAuthenticated, isAdmin])
```

Another example, to check if user is admin `or` if user is a member, use `OR` conditional:

```js
@Authorized([{OR: [isAdmin, isMember]}])
```

The conditionals can be nested, for example:

```js
@Authorized([{ OR: [isAdmin, { AND: [isAuthenticated, isOwner]} ] }])
```

## Want to help?

Thank you! Please open issues and pull requests on GitHub!
