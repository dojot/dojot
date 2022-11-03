import { mock } from 'jest-mock-extended'

import { Config } from 'src/types'

export const ConfigMock = {
  new() {
    return mock<Config>({
      sdk: {},
      topic: {},
      consumer: {},
      subscribe: {
        'topics.regex.tenants': 'regex',
      },
      server: {
        port: 1234,
      },
      express: {
        'parsing.limit': 200000,
      },
      redis: {
        db: 0,
        host: 'host',
        port: 1324,
      },
      keycloak: {
        url: 'http://keycloak:1432',
        'client.id': 'client',
        'tenants.url': 'tenant',
        'client.secret': 'secret',
        'client.secret.file': 'file',
      },
      apis: {
        retriever: 'http://retriever:4321',
        filemgmt: 'http://filemgmt:3412',
      },
      app: {
        'report.expiration.ms': 0,
        'report.path': '/reports',
      },
      postgres: {
        user: 'postgres',
        password: 'postgres',
        host: 'postgres',
        port: 5432,
        database: 'postgres',
      },
      lightship: {
        'detect.kubernetes': false,
      },
    })
  },
}
