import { PrismaClient } from '@prisma/client'
import { mockDeep } from 'jest-mock-extended'

export const PrismaClientMock = {
  new() {
    return mockDeep<PrismaClient>()
  },
}
