export type AppConfig = {
  sdk: object;
  topic: object;
  consumer: object;
  producer: object;
  subscribe: {
    'topics.regex.tenants': string;
  };
  message: {
    'produce.topic.suffix': string;
    'produce.topic.subject': string;
  };
  server: {
    port: number;
  };
  api: {
    port: number;
  };
  express: {
    'parsing.limit': number;
  };
  database: {
    host: string;
    name: string;
    schema: string;
    user: string;
    password: string;
    port: number;
  };
  keycloak: {
    url: string;
    'client.id': string;
    'tenants.url': string;
    'client.secret': string;
  };
  lightship: {
    'detect.kubernetes': boolean;
    port?: number;
  };
};
