import {
  Logger,
  WebUtils,
  ConfigManager,
  ServiceStateManager,
} from '@dojot/microservice-sdk'
import camelcase from 'lodash.camelcase'

import { Config } from 'src/types'
import { KafkaUtils, PrismaUtils } from 'src/utils'
import { KafkaConsumer, TenantManager } from 'src/kafka'

import { App } from './app'

ConfigManager.loadSettings(
  'REPORT_MANAGER',
  'default.conf',
  './configs',
  './dist',
)

const config: Config = ConfigManager.getConfig(
  'REPORT_MANAGER',
  './configs',
  './dist',
)

const logger = new Logger('report-manager')
Logger.setLevel('console', 'debug')

const serviceState = new ServiceStateManager({
  lightship: ConfigManager.transformObjectKeys(config.lightship, camelcase),
})

const dojotHttpClient = new WebUtils.DojotHttpClient({
  logger,
  defaultClientOptions: {},
  defaultRetryDelay: 15000,
  defaultMaxNumberAttempts: 0,
})

const kafkaUtils = new KafkaUtils()
const prismaUtils = new PrismaUtils(logger, config)

const kafkaConsumer = new KafkaConsumer(logger, config)
const tenantManager = new TenantManager(
  logger,
  config,
  kafkaUtils,
  prismaUtils,
  dojotHttpClient,
)

const retrieverHttpClient = new WebUtils.DojotHttpClient({
  logger,
  defaultRetryDelay: 10000,
  defaultMaxNumberAttempts: 5,
  defaultClientOptions: {
    baseURL: `${config.apis.retriever}/tss/v1`,
  },
})

const fileManagementHttpClient = new WebUtils.DojotHttpClient({
  logger,
  defaultRetryDelay: 10000,
  defaultMaxNumberAttempts: 5,
  defaultClientOptions: {
    baseURL: `${config.apis.filemgmt}/api/v1`,
  },
})

const secretFileHandler = new WebUtils.SecretFileHandler(config, logger)

secretFileHandler
  .handle('keycloak.client.secret', '/secrets/')
  .then(async () => {
    try {
      logger.info('Starting application', {})

      const server = await new App(
        logger,
        config,
        prismaUtils,
        kafkaConsumer,
        tenantManager,
        serviceState,
        retrieverHttpClient,
        fileManagementHttpClient,
      ).init()

      const handleCloseServerAndExitProcess = () => {
        server.close(() => process.exit(0))
      }

      process.on('SIGINT', handleCloseServerAndExitProcess)
      process.on('SIGTERM', handleCloseServerAndExitProcess)

      process.on('unhandledRejection', (e: Error) => {
        logger.error(`unhandledRejection: ${e.stack || e}`, {})
        process.kill(process.pid, 'SIGTERM')
      })

      process.on('uncaughtException', (e: Error) => {
        logger.error(`uncaughtException: ${e.stack || e}`, {})
        process.kill(process.pid, 'SIGTERM')
      })
    } catch (error) {
      logger.error('Error when starting app', error as never)
      process.kill(process.pid, 'SIGTERM')
    }
  })
