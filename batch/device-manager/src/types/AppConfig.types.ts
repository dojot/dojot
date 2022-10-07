export type AppConfig = {
    sdk: object
    topic: object
    consumer: object
    producer: object
    subscribe: {
      'topics.regex.tenants': string
    }
    message: {
      'produce.topic.suffix': string 
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
    lightship: {
      'detect.kubernetes': boolean
    }
  }