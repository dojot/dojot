import { execSync } from 'child_process'

export const getDatabaseUrl = (schema: string) => {
  const user = 'postgres'
  const password = 'postgres'
  const host = 'postgres'
  const port = 5432
  const database = 'report-manager'
  return `postgresql://${user}:${password}@${host}:${port}/${database}?schema=${schema}`
}

export const deployMigrations = (databaseUrl: string) => {
  const exportCommand = `export DATABASE_URL=${databaseUrl}`
  const migrateCommand = 'yarn prisma migrate deploy'
  execSync(`${exportCommand} && ${migrateCommand}`)
}