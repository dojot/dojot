import { PrismaClient } from '@prisma/client'

declare global {
  namespace Express {
    interface Request {
      tenant: string
      prisma: PrismaClient
    }
  }
}