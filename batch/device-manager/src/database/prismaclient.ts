

import { PrismaClient, Prisma } from "@prisma/client";

const prismaClient = new PrismaClient({
  log: ["error", "info", "query", "warn"],
});

export { prismaClient, Prisma };