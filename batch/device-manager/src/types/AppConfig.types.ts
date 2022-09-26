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
  }