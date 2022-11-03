import { PrismaClient } from '@prisma/client'
import { WebUtils } from '@dojot/microservice-sdk'

declare global {
  namespace Express {
    interface Request {
      lang: string
      tenant: WebUtils.TenantInfo
      prisma: PrismaClient
    }
  }
}
