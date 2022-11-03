import { TenantManager } from 'src/kafka'
import { KafkaParsedPayloadValue } from 'src/types'
import {
  AppMock,
  ConfigMock,
  LoggerMock,
  DojotHttpClientMock,
} from 'tests/mocks'

describe('TenantManager.kafka', () => {
  const TENANTS = [{ id: 'admin' }, { id: 'master' }]

  describe('update', () => {
    it('should update the list of tenants', async () => {
      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()
      const FakeDojotHttpClient = DojotHttpClientMock.new()

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
    it('should create infra for a new tenant', () => {
      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()

      const value: KafkaParsedPayloadValue = {
        type: 'CREATE',
        tenant: 'admin',
        signatureKey: {},
      }

      const valueJson = JSON.stringify(value)
      const payload = { value: valueJson }

      KafkaUtilsMock.getValue.mockReturnValue(value)

      const tenantManager = new TenantManager(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaUtilsMock,
        PrismaUtilsMock,
        DojotHttpClientMock.new(),
      )

      expect(tenantManager.tenants).toHaveLength(0)
      tenantManager.create(payload)

      expect(tenantManager.tenants).toEqual([
        { id: value.tenant, signatureKey: value.signatureKey },
      ])

      expect(KafkaUtilsMock.getValue).toBeCalledWith(payload)
      expect(PrismaUtilsMock.seedDatabase).toBeCalled()
      expect(PrismaUtilsMock.deployMigrations).toBeCalled()
      expect(PrismaUtilsMock.getDatabaseUrl).toBeCalledWith(value.tenant)
    })

    it('should log error when fails to create infra for tenant', () => {
      const { KafkaUtilsMock, PrismaUtilsMock } = AppMock.new()
      const FakeLogger = LoggerMock.new()

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

      tenantManager.create({ value: '{}' })

      expect(FakeLogger.error).toBeCalled()
      expect(KafkaUtilsMock.getValue).toBeCalled()
      expect(PrismaUtilsMock.seedDatabase).not.toBeCalled()
      expect(PrismaUtilsMock.getDatabaseUrl).not.toBeCalled()
      expect(PrismaUtilsMock.deployMigrations).not.toBeCalled()
    })
  })
})
