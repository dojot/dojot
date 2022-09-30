export type AppConfig = {
    sdk: object
    topic: object
    consumer: object
    subscribe: {
      'topics.regex.tenants': string
    }
    api: {
      port: number
    }
    express: {
      'parsing.limit': number
    }
    database: {
      host: string
      name: string
      schema: string
      user: string
      password: string
    }
    keycloak: {
      'tenants.url': string
    }
    producer: object
    message: {
      'produce.topic.suffix': string 
    }
  }