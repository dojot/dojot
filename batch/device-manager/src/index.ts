import { ConfigManager, Logger, ServiceStateManager, WebUtils } from '@dojot/microservice-sdk'

import { AppConfig } from 'src/types'
import { KafkaConsumer, TenantManager,KafkaProducer } from './kafka'
import { App } from './app'
import camelCase from 'lodash.camelcase'


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


const logger = new Logger('dojot-device-manager-batch')
Logger.setLevel('console', 'debug')

const dojotHttpClient = new WebUtils.DojotHttpClient({
  logger,
  defaultClientOptions: {},
  defaultRetryDelay: 15000,
  defaultMaxNumberAttempts: 0,
})

const kafkaConsumer = new KafkaConsumer(logger, config)
const kafkaProducer = new KafkaProducer(logger, config)
const tenantManager = new TenantManager(logger, config, dojotHttpClient)
const secretFileHandler = new WebUtils.SecretFileHandler(config, logger)
const serviceState = new ServiceStateManager({
  lightship: ConfigManager.transformObjectKeys(config.lightship['detect.kubernetes'], camelCase),
});

serviceState.registerService('dojot-device-manager-batch');

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
        kafkaProducer,
        serviceState
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

      logger.info('Application Device Manager Batch is running', {})
    } catch (error: unknown) {
      logger.error('Application will be closed: ', { error })
      process.kill(process.pid, 'SIGTERM')
    }
  })