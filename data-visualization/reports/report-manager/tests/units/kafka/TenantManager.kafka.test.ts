import { TenantManager } from 'src/kafka'
import { KafkaParsedPayloadValue } from 'src/types'
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
      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()
      const FakeDojotHttpClient = DojotHttpClientMock.new()

      const TENANTS = [{ id: 'admin' }, { id: 'master' }]
      const response = { data: { tenants: TENANTS } }
      FakeDojotHttpClient.request.mockResolvedValue(response as never)

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaUtilsMock,
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
      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()
      const FakeDojotHttpClient = DojotHttpClientMock.new()
      const FakeLogger = LoggerMock.new()

      const error = new Error('Error')
      FakeDojotHttpClient.request.mockRejectedValue(error)

      const tenantManager = new TenantManager(
        FakeLogger,
        ConfigMock.new(),
        KafkaUtilsMock,
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
    it('should create a new tenant', () => {
      const PAYLOAD_VALUE: KafkaParsedPayloadValue = {
        type: 'CREATE',
        tenant: 'admin',
        signatureKey: {},
      }

      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaUtilsMock,
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      expect(tenantManager.tenants).toHaveLength(0)

      tenantManager.create(PAYLOAD_VALUE)

      expect(tenantManager.tenants).toHaveLength(1)
      expect(tenantManager.tenants[0]).toMatchObject({
        id: PAYLOAD_VALUE.tenant,
        signatureKey: PAYLOAD_VALUE.signatureKey,
      })

      expect(PrismaUtilsMock.getDatabaseUrl).toBeCalledWith(
        PAYLOAD_VALUE.tenant,
      )

      expect(PrismaUtilsMock.deployMigrations).toBeCalled()
      expect(PrismaUtilsMock.seedDatabase).toBeCalled()
    })

    it('should log error when fails to create tenant', () => {
      const PAYLOAD_VALUE: KafkaParsedPayloadValue = {
        type: 'CREATE',
        tenant: 'admin',
        signatureKey: {},
      }

      const FakeLogger = LoggerMock.new()
      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()

      const tenantManager = new TenantManager(
        FakeLogger,
        ConfigMock.new(),
        KafkaUtilsMock,
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      jest.spyOn(tenantManager, 'updateTenantSchema').mockImplementation(() => {
        throw new Error('Error')
      })

      tenantManager.create(PAYLOAD_VALUE)

      expect(FakeLogger.error).toBeCalled()
      expect(PrismaUtilsMock.seedDatabase).not.toBeCalled()
      expect(PrismaUtilsMock.getDatabaseUrl).not.toBeCalled()
      expect(PrismaUtilsMock.deployMigrations).not.toBeCalled()
    })
  })

  describe('delete', () => {
    it('should delete a tenant', async () => {
      const PAYLOAD_VALUE: KafkaParsedPayloadValue = {
        type: 'DELETE',
        tenant: 'master',
        signatureKey: {},
      }

      const TENANTS = [
        { id: 'admin', sigKey: {} },
        { id: 'master', sigKey: {} },
        { id: 'test', sigKey: {} },
      ]

      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()
      PrismaUtilsMock.dropSchema.mockResolvedValue(true)

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaUtilsMock,
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      tenantManager.tenants = TENANTS
      expect(tenantManager.tenants).toEqual(TENANTS)

      await tenantManager.delete(PAYLOAD_VALUE)

      const NEW_TENANTS = TENANTS.filter(
        ({ id }) => id !== PAYLOAD_VALUE.tenant,
      )

      expect(tenantManager.tenants).toEqual(NEW_TENANTS)
      expect(PrismaUtilsMock.getDatabaseUrl).toBeCalled()
      expect(PrismaUtilsMock.dropSchema).toBeCalledWith(
        PAYLOAD_VALUE.tenant,
        expect.any(Object),
      )
      expect(PrismaUtilsMock.disconnectPrisma).toBeCalledWith(
        PAYLOAD_VALUE.tenant,
        expect.any(Object),
      )
    })

    it('should handle errors when deleting a tenant', async () => {
      const PAYLOAD_VALUE: KafkaParsedPayloadValue = {
        type: 'DELETE',
        tenant: 'master',
        signatureKey: {},
      }

      const TENANTS = [
        { id: 'admin', sigKey: {} },
        { id: 'master', sigKey: {} },
        { id: 'test', sigKey: {} },
      ]

      const FakeLogger = LoggerMock.new()
      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()

      PrismaUtilsMock.getDatabaseUrl.mockImplementation(() => {
        throw new Error('Error')
      })

      const tenantManager = new TenantManager(
        FakeLogger,
        ConfigMock.new(),
        KafkaUtilsMock,
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      tenantManager.tenants = TENANTS
      expect(tenantManager.tenants).toEqual(TENANTS)

      await tenantManager.delete(PAYLOAD_VALUE)

      expect(tenantManager.tenants).toEqual(TENANTS)
      expect(FakeLogger.error).toBeCalled()
    })
  })

  describe('handleTenantEvent', () => {
    it('should handle a tenant creation event', async () => {
      const PAYLOAD_VALUE: KafkaParsedPayloadValue = {
        type: 'CREATE',
        tenant: 'admin',
        signatureKey: {},
      }

      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()
      KafkaUtilsMock.getValue.mockReturnValue(PAYLOAD_VALUE)

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaUtilsMock,
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      const createFn = jest.spyOn(tenantManager, 'create')
      const deleteFn = jest.spyOn(tenantManager, 'delete')

      const payload = { value: JSON.stringify(PAYLOAD_VALUE) }
      await tenantManager.handleTenantEvent(payload)

      expect(KafkaUtilsMock.getValue).toBeCalledWith(payload)
      expect(deleteFn).not.toBeCalled()
      expect(createFn).toBeCalled()
    })

    it('should handle a tenant deletion event', async () => {
      const PAYLOAD_VALUE: KafkaParsedPayloadValue = {
        type: 'DELETE',
        tenant: 'admin',
        signatureKey: {},
      }

      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()
      KafkaUtilsMock.getValue.mockReturnValue(PAYLOAD_VALUE)

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaUtilsMock,
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      const createFn = jest.spyOn(tenantManager, 'create')
      const deleteFn = jest.spyOn(tenantManager, 'delete')

      const payload = { value: JSON.stringify(PAYLOAD_VALUE) }
      await tenantManager.handleTenantEvent(payload)

      expect(KafkaUtilsMock.getValue).toBeCalledWith(payload)
      expect(createFn).not.toBeCalled()
      expect(deleteFn).toBeCalled()
    })

    it('should handle errors when handling a tenant event', async () => {
      const PAYLOAD_VALUE: KafkaParsedPayloadValue = {
        type: 'CREATE',
        tenant: 'admin',
        signatureKey: {},
      }

      const FakeLogger = LoggerMock.new()
      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()

      KafkaUtilsMock.getValue.mockImplementation(() => {
        throw new Error('Error')
      })

      const tenantManager = new TenantManager(
        FakeLogger,
        ConfigMock.new(),
        KafkaUtilsMock,
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      const createFn = jest.spyOn(tenantManager, 'create')
      const deleteFn = jest.spyOn(tenantManager, 'delete')

      const payload = { value: JSON.stringify(PAYLOAD_VALUE) }
      await tenantManager.handleTenantEvent(payload)

      expect(KafkaUtilsMock.getValue).toBeCalledWith(payload)
      expect(createFn).not.toBeCalled()
      expect(deleteFn).not.toBeCalled()
      expect(FakeLogger.error).toBeCalled()
    })
  })

  describe('updateTenantSchema', () => {
    it('should update a tenant schema', () => {
      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()

      const VALUES = {
        TENANT: 'admin',
        DB_URL: 'database:url',
      }

      PrismaUtilsMock.getDatabaseUrl.mockReturnValue(VALUES.DB_URL)

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaUtilsMock,
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
