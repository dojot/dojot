import { PrismaUtils } from '../../../src/utils/Prisma.utils';
import { LoggerMock, ConfigMock, PrismaClientMock } from '../../mocks';

jest.mock('child_process', () => {
  return { execSync: jest.fn((command: string) => command) };
});

describe('Prisma.utils', () => {
  const FakeLogger = LoggerMock.new();
  const appconfig = ConfigMock.new();
  const prismaUtils = new PrismaUtils(FakeLogger, appconfig);
  const fakeDatabaseUrl = '--fake-database-url--';

  it('should the database url contain the schema and postgres prefix', () => {
    const databaseUrl = prismaUtils.getDatabaseUrl(
      'admin',
      FakeLogger,
      appconfig,
    );
    expect(databaseUrl).toContain('?schema=admin');
    expect(databaseUrl).toContain('postgresql://');
  });

  it('should disconnect from database', async () => {
    const FakePrismaClient = PrismaClientMock.new();
    await prismaUtils.disconnectPrisma('admin', FakePrismaClient);
    expect(FakePrismaClient.$disconnect).toBeCalled();
    expect(FakeLogger.debug).toBeCalled();
  });

  it('should handle an error when disconnecting from database', async () => {
    const FakePrismaClient = PrismaClientMock.new();
    FakePrismaClient.$disconnect.mockRejectedValue(new Error('Error'));
    await prismaUtils.disconnectPrisma('admin', FakePrismaClient);
    expect(FakePrismaClient.$disconnect).toBeCalled();
    expect(FakeLogger.error).toBeCalled();
  });

  it('should generate random of size 6', async () => {
    expect(prismaUtils.getRandomicHexIdDevices().length).toBe(6);
  });
});
