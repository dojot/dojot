import { PrismaClient } from '@prisma/client'

declare global {
  namespace Express {
    interface Request {
      tenant: WebUtils.TenantInfo
      prisma: PrismaClient
    }
  }
}