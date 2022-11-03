import { execSync } from 'child_process'

import { PrismaUtils } from 'src/utils'
import { ConfigMock, LoggerMock, PrismaClientMock } from 'tests/mocks'

jest.mock('child_process', () => {
  return { execSync: jest.fn((command: string) => command) }
})

describe('Prisma.utils', () => {
  const FakeLogger = LoggerMock.new()
  const prismaUtils = new PrismaUtils(FakeLogger, ConfigMock.new())
  const fakeDatabaseUrl = '--fake-database-url--'

  it('should the database url contain the schema and postgres prefix', () => {
    const databaseUrl = prismaUtils.getDatabaseUrl('admin')
    expect(databaseUrl).toContain('?schema=admin')
    expect(databaseUrl).toContain('postgresql://')
  })

  it('should execute the command to deploy migrations', () => {
    prismaUtils.deployMigrations(fakeDatabaseUrl)
    expect(execSync).toBeCalled()
  })

  it('should execute the command to seed database', () => {
    prismaUtils.seedDatabase(fakeDatabaseUrl)
    expect(execSync).toBeCalled()
  })

  it('should disconnect from database', async () => {
    const FakePrismaClient = PrismaClientMock.new()
    await prismaUtils.disconnectPrisma('admin', FakePrismaClient)
    expect(FakePrismaClient.$disconnect).toBeCalled()
    expect(FakeLogger.debug).toBeCalled()
  })

  it('should handle an error when disconnecting from database', async () => {
    const FakePrismaClient = PrismaClientMock.new()
    FakePrismaClient.$disconnect.mockRejectedValue(new Error('Error'))
    await prismaUtils.disconnectPrisma('admin', FakePrismaClient)
    expect(FakePrismaClient.$disconnect).toBeCalled()
    expect(FakeLogger.error).toBeCalled()
  })
})
