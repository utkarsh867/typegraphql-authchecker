import "reflect-metadata";
import { ApolloServer } from "apollo-server";
import { Authorized, buildSchema, Query, Resolver } from "type-graphql";
import authChecker, { Rule } from "typegraphql-authchecker";

interface Context {
  user: string;
}

const isAuthorized: Rule<Context> = ({ root, info, args, context }) => {
  console.log(context);
  const value = !!context.user;
  return value;
};

const isUserAdmin: Rule<Context> = ({ root, info, args, context }) => {
  if (context.user === "admin") return true;
  return false;
};

@Resolver()
class HelloWorldResolver {
  @Query((returns) => String)
  async hello() {
    return "world";
  }

  @Authorized({ AND: [isAuthorized, isUserAdmin] })
  @Query((returns) => String)
  async authorizedHello() {
    return "world";
  }
}

async function buildContext() {
  const ctx = {
    user: "admin",
  };
  return ctx;
}

buildSchema({
  resolvers: [HelloWorldResolver],
  authChecker,
})
  .then((schema) => {
    const server = new ApolloServer({
      schema,
      context: buildContext,
    });
    return server;
  })
  .then((server) => {
    server.listen().then(({ url }) => {
      console.log(`Running server at ${url}`);
    });
  });
