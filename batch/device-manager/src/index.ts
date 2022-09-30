import { ConfigManager, Logger, WebUtils } from '@dojot/microservice-sdk'

import { AppConfig } from 'src/types'
import { KafkaConsumer, TenantManager } from './kafka'

import { App } from './app'
import KafkaProducer from './kafka/kafka-producer'

ConfigManager.loadSettings(
  'DEVICE_MANAGER_BATCH',
  'default.conf',
  './config',
  './build',
)

const config: AppConfig = ConfigManager.getConfig(
  'DEVICE_MANAGER_BATCH',
  './config',
  './build',
)


const logger = new Logger('device-manager-batch')
Logger.setLevel('console', 'debug')

const dojotHttpClient = new WebUtils.DojotHttpClient({
  logger,
  defaultClientOptions: {},
  defaultRetryDelay: 15000,
  defaultMaxNumberAttempts: 0,
})

const kafkaConsumer = new KafkaConsumer(logger, config)

const tenantManager = new TenantManager(logger,config,dojotHttpClient)

const secretFileHandler = new WebUtils.SecretFileHandler(config, logger)

secretFileHandler
  .handle('keycloak.client.secret', '/secrets')
  .then(async () => {
    try {
      logger.info('Starting application', {})

      const server = await new App(
        logger,
        config,
        kafkaConsumer,
        tenantManager,
      ).init()

      const handleCloseServerAndExitProcess = () => {
        server.close(() => process.exit(0))
      }

      process.on('SIGINT', handleCloseServerAndExitProcess)
      process.on('SIGTERM', handleCloseServerAndExitProcess)

      process.on('unhandledRejection', (e: Error) => {
        logger.error(`Unhandled rejection: ${e.stack || e}`, {})
        process.kill(process.pid, 'SIGTERM')
      })

      process.on('uncaughtException', (e: Error) => {
        logger.error(
          `uncaughtException: Unhandled exception: ${e.stack || e}`,
          {},
        )
        process.kill(process.pid, 'SIGTERM')
      })

      logger.info('Application is running', {})
    } catch (error: unknown) {
      logger.error('Application will be closed: ', { error })
      process.kill(process.pid, 'SIGTERM')
    }
  })