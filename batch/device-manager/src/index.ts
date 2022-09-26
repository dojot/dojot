import { ConfigManager, Logger } from '@dojot/microservice-sdk'

import { AppConfig } from 'src/types'
import { KafkaConsumer, TenantManager } from './kafka'

import { App } from './app'

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

const logger = new Logger('report-manager')
Logger.setLevel('console', 'debug')

const kafkaConsumer = new KafkaConsumer(logger, config)
const tenantManager = new TenantManager(logger)

new App(logger, config, kafkaConsumer, tenantManager).init().then((server) => {
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
    logger.error(`uncaughtException: Unhandled exception: ${e.stack || e}`, {})
    process.kill(process.pid, 'SIGTERM')
  })
})