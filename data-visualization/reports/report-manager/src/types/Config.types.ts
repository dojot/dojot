export type Config = {
  sdk: object
  topic: object
  consumer: object
  subscribe: {
    'topics.regex.tenants': string
  }
  server: {
    port: number
  }
  express: {
    'parsing.limit': number
  }
  redis: {
    db: number
    host: string
    port: number
  }
  keycloak: {
    url: string
    'client.id': string
    'tenants.url': string
    'client.secret': string
    'client.secret.file': string
  }
  apis: {
    retriever: string
    filemgmt: string
  }
  app: {
    'report.expiration.ms': number
    'report.path': string
  }
  postgres: {
    user: string
    password: string
    host: string
    port: number
    database: string
  }
  lightship: {
    'detect.kubernetes': boolean
    port?: number
  }
  logger: {
    level: string
    verbose: boolean
  }
}
