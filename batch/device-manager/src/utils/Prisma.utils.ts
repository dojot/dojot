import { Logger } from '@dojot/microservice-sdk'
import { execSync } from 'child_process'
import { AppConfig } from 'src/types'

export const getDatabaseUrl = (schema: string,logger:Logger,config:AppConfig) => {
  if(schema == null)
  {
    schema = config.database.schema; 
  }
  const user = config.database.user
  const password = config.database.password
  const host = config.database.host
  const port = 5432
  const database = config.database.name
  const url = `postgresql://${user}:${password}@${host}:${port}/${database}?schema=${schema}` 
  logger.debug('Connection with Database',{url})
  return url
}


export const deployMigrations = (databaseUrl: string) => {
  const exportCommand = `export DATABASE_URL=${databaseUrl}`
  const migrateCommand = 'yarn prisma migrate deploy'
  execSync(`${exportCommand} && ${migrateCommand}`)
}