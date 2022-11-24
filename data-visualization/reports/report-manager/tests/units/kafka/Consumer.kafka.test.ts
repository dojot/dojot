import { KafkaConsumer } from 'src/kafka'
import { ConfigMock, KafkaMock, LoggerMock } from 'tests/mocks'

jest.mock('@dojot/microservice-sdk')

describe('Consumer.kafka', () => {
  describe('getParsedValue', () => {
    it('should parse the payload value', () => {
      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaMock.new().TenantManagerMock,
      )

      const VALUE = {
        type: 'CREATE',
        tenant: 'admin',
        signatureKey: {},
      }

      const PAYLOAD = { value: JSON.stringify(VALUE) }
      expect(kafkaConsumer.getParsedValue(PAYLOAD)).toEqual(VALUE)
    })
  })

  describe('isConnected', () => {
    it('should return that it is connected', async () => {
      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaMock.new().TenantManagerMock,
      )

      const getStatusSpy = jest
        .spyOn(kafkaConsumer, 'getStatus')
        .mockResolvedValue({ connected: true })

      const isConnected = await kafkaConsumer.isConnected()

      expect(getStatusSpy).toBeCalled()
      expect(isConnected).toBe(true)
    })

    it('should return that it is not connected', async () => {
      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaMock.new().TenantManagerMock,
      )

      const getStatusSpy = jest
        .spyOn(kafkaConsumer, 'getStatus')
        .mockResolvedValue({ connected: false })

      const isConnected = await kafkaConsumer.isConnected()

      expect(getStatusSpy).toBeCalled()
      expect(isConnected).toBe(false)
    })

    it('should handle errors and return that it is not connected', async () => {
      const FakeLogger = LoggerMock.new()

      const kafkaConsumer = new KafkaConsumer(
        FakeLogger,
        ConfigMock.new(),
        KafkaMock.new().TenantManagerMock,
      )

      const getStatusSpy = jest
        .spyOn(kafkaConsumer, 'getStatus')
        .mockRejectedValue(new Error('Error'))

      const isConnected = await kafkaConsumer.isConnected()

      expect(getStatusSpy).toBeCalled()
      expect(isConnected).toBe(false)
      expect(FakeLogger.error).toBeCalled()
    })
  })

  describe('registerTenantEvent', () => {
    it('should register a listener for tenant event', () => {
      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaMock.new().TenantManagerMock,
      )

      const CALLBACK_ID = 101

      const registerCallbackSpy = jest
        .spyOn(kafkaConsumer, 'registerCallback')
        .mockReturnValue(CALLBACK_ID)

      kafkaConsumer.registerTenantEvent()

      expect(registerCallbackSpy).toBeCalled()
      expect(kafkaConsumer['callbackId']).toBe(CALLBACK_ID)
    })
  })

  describe('init', () => {
    it('should register the tenant event listener on init', async () => {
      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaMock.new().TenantManagerMock,
      )

      const registerTenantEventSpy = jest.spyOn(
        kafkaConsumer,
        'registerTenantEvent',
      )

      await kafkaConsumer.init()

      expect(registerTenantEventSpy).toBeCalled()
    })
  })

  describe('unregisterCallbacks', () => {
    it('should unregister all event listeners', () => {
      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaMock.new().TenantManagerMock,
      )

      const CALLBACK_ID = 759
      kafkaConsumer['callbackId'] = CALLBACK_ID

      const unregisterCallbackSpy = jest.spyOn(
        kafkaConsumer,
        'unregisterCallback',
      )

      kafkaConsumer.unregisterCallbacks()

      expect(unregisterCallbackSpy).toBeCalledWith(CALLBACK_ID)
      expect(kafkaConsumer['callbackId']).toBeFalsy()
    })

    it('should do nothing because there is no active listener', () => {
      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        KafkaMock.new().TenantManagerMock,
      )

      const unregisterCallbackSpy = jest.spyOn(
        kafkaConsumer,
        'unregisterCallback',
      )

      kafkaConsumer.unregisterCallbacks()

      expect(unregisterCallbackSpy).not.toBeCalled()
    })
  })

  describe('handleTenantEvent', () => {
    it('should handle a tenant creation event', async () => {
      const { TenantManagerMock } = KafkaMock.new()

      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        TenantManagerMock,
      )

      const VALUE = {
        type: 'CREATE',
        tenant: 'admin',
        signatureKey: {},
      }

      const PAYLOAD = { value: JSON.stringify(VALUE) }
      const CALLBACK = jest.fn()

      await kafkaConsumer.handleTenantEvent(PAYLOAD, CALLBACK)

      expect(TenantManagerMock.create).toBeCalledWith(VALUE)
      expect(TenantManagerMock.delete).not.toBeCalled()
      expect(CALLBACK).toBeCalled()
    })

    it('should handle a tenant deletion event', async () => {
      const { TenantManagerMock } = KafkaMock.new()

      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        TenantManagerMock,
      )

      const VALUE = {
        type: 'DELETE',
        tenant: 'admin',
        signatureKey: {},
      }

      const PAYLOAD = { value: JSON.stringify(VALUE) }
      const CALLBACK = jest.fn()

      await kafkaConsumer.handleTenantEvent(PAYLOAD, CALLBACK)

      expect(TenantManagerMock.delete).toBeCalledWith(VALUE)
      expect(TenantManagerMock.create).not.toBeCalled()
      expect(CALLBACK).toBeCalled()
    })

    it('should do nothing if handle a unknown event', async () => {
      const { TenantManagerMock } = KafkaMock.new()

      const kafkaConsumer = new KafkaConsumer(
        LoggerMock.new(),
        ConfigMock.new(),
        TenantManagerMock,
      )

      const VALUE = {
        type: 'UNKNOWN_EVENT',
        tenant: 'admin',
        signatureKey: {},
      }

      const PAYLOAD = { value: JSON.stringify(VALUE) }
      const CALLBACK = jest.fn()

      await kafkaConsumer.handleTenantEvent(PAYLOAD, CALLBACK)

      expect(TenantManagerMock.delete).not.toBeCalled()
      expect(TenantManagerMock.create).not.toBeCalled()
      expect(CALLBACK).toBeCalled()
    })

    it('should handle errors when handling a tenant event', async () => {
      const { TenantManagerMock } = KafkaMock.new()
      const FakeLogger = LoggerMock.new()

      const kafkaConsumer = new KafkaConsumer(
        FakeLogger,
        ConfigMock.new(),
        TenantManagerMock,
      )

      jest.spyOn(kafkaConsumer, 'getParsedValue').mockImplementation(() => {
        throw new Error('Error')
      })

      const VALUE = {
        type: 'CREATE',
        tenant: 'admin',
        signatureKey: {},
      }

      const PAYLOAD = { value: JSON.stringify(VALUE) }
      const CALLBACK = jest.fn()

      await kafkaConsumer.handleTenantEvent(PAYLOAD, CALLBACK)

      expect(TenantManagerMock.delete).not.toBeCalled()
      expect(TenantManagerMock.create).not.toBeCalled()
      expect(FakeLogger.error).toBeCalled()
      expect(CALLBACK).toBeCalled()
    })
  })
})
