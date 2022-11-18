import { mock } from 'jest-mock-extended';

import { AppConfig } from '../../src/types';

export const ConfigMock = {
  new() {
    return mock<AppConfig>({
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
      keycloak: {
        url: 'http://keycloak:1432',
        'client.id': 'client',
        'tenants.url': 'tenant',
        'client.secret': 'secret',
      },
      database: {
        user: 'postgres',
        password: 'postgres',
        host: 'postgres',
        port: 5432,
        name: 'dev',
      },
      lightship: {
        'detect.kubernetes': false,
      },
    });
  },
};
