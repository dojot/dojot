import { TenantManager } from 'src/kafka'
import {
  AppMock,
  ConfigMock,
  LoggerMock,
  DojotHttpClientMock,
} from 'tests/mocks'

jest.mock('@prisma/client', () => ({
  PrismaClient: jest.fn(),
}))

describe('TenantManager.kafka', () => {
  describe('update', () => {
    it('should update the list of tenants', async () => {
      const { PrismaUtilsMock } = AppMock.new()
      const FakeDojotHttpClient = DojotHttpClientMock.new()

      const TENANTS = [{ id: 'admin' }, { id: 'master' }]
      const response = { data: { tenants: TENANTS } }
      FakeDojotHttpClient.request.mockResolvedValue(response as never)

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        PrismaUtilsMock,
        FakeDojotHttpClient,
      )

      await tenantManager.update()

      expect(FakeDojotHttpClient.request).toBeCalled()
      expect(tenantManager.tenants).toEqual(TENANTS)
      expect(PrismaUtilsMock.seedDatabase).toBeCalledTimes(TENANTS.length)
      expect(PrismaUtilsMock.getDatabaseUrl).toBeCalledTimes(TENANTS.length)
      expect(PrismaUtilsMock.deployMigrations).toBeCalledTimes(TENANTS.length)
    })

    it('should log error when fails to update the list of tenants', async () => {
      const { PrismaUtilsMock } = AppMock.new()
      const FakeDojotHttpClient = DojotHttpClientMock.new()
      const FakeLogger = LoggerMock.new()

      const error = new Error('Error')
      FakeDojotHttpClient.request.mockRejectedValue(error)

      const tenantManager = new TenantManager(
        FakeLogger,
        ConfigMock.new(),
        PrismaUtilsMock,
        FakeDojotHttpClient,
      )

      await tenantManager.update()

      expect(FakeDojotHttpClient.request).toBeCalled()
      expect(tenantManager.tenants).toEqual([])
      expect(PrismaUtilsMock.seedDatabase).not.toBeCalled()
      expect(PrismaUtilsMock.getDatabaseUrl).not.toBeCalled()
      expect(PrismaUtilsMock.deployMigrations).not.toBeCalled()
      expect(FakeLogger.error).toBeCalled()
    })
  })

  describe('create', () => {
    it('should create a new tenant', async () => {
      const PARAMS = {
        tenant: 'admin',
        signatureKey: {},
      }

      const { PrismaUtilsMock } = AppMock.new()

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      expect(tenantManager.tenants).toHaveLength(0)

      await tenantManager.create(PARAMS)

      expect(tenantManager.tenants).toHaveLength(1)
      expect(tenantManager.tenants[0]).toMatchObject({
        id: PARAMS.tenant,
        signatureKey: PARAMS.signatureKey,
      })

      expect(PrismaUtilsMock.getDatabaseUrl).toBeCalledWith(PARAMS.tenant)
      expect(PrismaUtilsMock.deployMigrations).toBeCalled()
      expect(PrismaUtilsMock.seedDatabase).toBeCalled()
    })

    it('should log error when fails to create tenant', async () => {
      const PARAMS = {
        tenant: 'admin',
        signatureKey: {},
      }

      const FakeLogger = LoggerMock.new()
      const { PrismaUtilsMock } = AppMock.new()

      const tenantManager = new TenantManager(
        FakeLogger,
        ConfigMock.new(),
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      jest.spyOn(tenantManager, 'updateTenantSchema').mockImplementation(() => {
        throw new Error('Error')
      })

      await tenantManager.create(PARAMS)

      expect(FakeLogger.error).toBeCalled()
      expect(PrismaUtilsMock.seedDatabase).not.toBeCalled()
      expect(PrismaUtilsMock.getDatabaseUrl).not.toBeCalled()
      expect(PrismaUtilsMock.deployMigrations).not.toBeCalled()
    })
  })

  describe('delete', () => {
    it('should delete a tenant', async () => {
      const PARAMS = { tenant: 'master' }

      const TENANTS = [
        { id: 'admin', sigKey: {} },
        { id: 'master', sigKey: {} },
        { id: 'test', sigKey: {} },
      ]

      const { PrismaUtilsMock } = AppMock.new()
      PrismaUtilsMock.dropSchema.mockResolvedValue(true)

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      tenantManager.tenants = TENANTS
      expect(tenantManager.tenants).toEqual(TENANTS)

      await tenantManager.delete(PARAMS)

      const NEW_TENANTS = TENANTS.filter(({ id }) => id !== PARAMS.tenant)
      expect(tenantManager.tenants).toEqual(NEW_TENANTS)
      expect(PrismaUtilsMock.getDatabaseUrl).toBeCalled()
      expect(PrismaUtilsMock.dropSchema).toBeCalledWith(
        PARAMS.tenant,
        expect.any(Object),
      )
      expect(PrismaUtilsMock.disconnectPrisma).toBeCalledWith(
        PARAMS.tenant,
        expect.any(Object),
      )
    })

    it('should handle errors when deleting a tenant', async () => {
      const PARAMS = { tenant: 'master' }

      const TENANTS = [
        { id: 'admin', sigKey: {} },
        { id: 'master', sigKey: {} },
        { id: 'test', sigKey: {} },
      ]

      const FakeLogger = LoggerMock.new()
      const { PrismaUtilsMock } = AppMock.new()

      PrismaUtilsMock.getDatabaseUrl.mockImplementation(() => {
        throw new Error('Error')
      })

      const tenantManager = new TenantManager(
        FakeLogger,
        ConfigMock.new(),
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      tenantManager.tenants = TENANTS
      expect(tenantManager.tenants).toEqual(TENANTS)

      await tenantManager.delete(PARAMS)

      expect(tenantManager.tenants).toEqual(TENANTS)
      expect(FakeLogger.error).toBeCalled()
    })
  })

  describe('updateTenantSchema', () => {
    it('should update a tenant schema', () => {
      const { PrismaUtilsMock } = AppMock.new()

      const VALUES = {
        TENANT: 'admin',
        DB_URL: 'database:url',
      }

      PrismaUtilsMock.getDatabaseUrl.mockReturnValue(VALUES.DB_URL)

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      tenantManager.updateTenantSchema(VALUES.TENANT)

      expect(PrismaUtilsMock.getDatabaseUrl).toBeCalledWith(VALUES.TENANT)
      expect(PrismaUtilsMock.deployMigrations).toBeCalledWith(VALUES.DB_URL)
      expect(PrismaUtilsMock.seedDatabase).toBeCalledWith(VALUES.DB_URL)
    })
  })
})
