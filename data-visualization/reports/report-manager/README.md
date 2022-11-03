# :page_with_curl: Report Manager

API responsible for creating reports. He can create reports in many formats and even create a zip file with multiple reports.

## :notebook_with_decorative_cover: Environment Variables

Click in the items below to expand. :point_down:

<details>
  <summary>Server</summary>

| Key         | Type    | Default Value | Env Var                    |
| ----------- | ------- | ------------- | -------------------------- |
| server.port | integer | 3791          | REPORT_MANAGER_SERVER_PORT |

</details>

<details>
  <summary>App</summary>

| Key                      | Type    | Default Value | Env Var                                 | Description                                                          |
| ------------------------ | ------- | ------------- | --------------------------------------- | -------------------------------------------------------------------- |
| app.report.expiration.ms | integer | 0             | REPORT_MANAGER_APP_REPORT_EXPIRATION_MS | Report file expiration in milliseconds. Use 0 to disable expiration. |
| app.report.path          | string  | /reports      | REPORT_MANAGER_APP_REPORT_PATH          | Report file path. Don't put a slash at the end.                      |

</details>

<details>
  <summary>Kafka</summary>

| Key                                         | Type    | Default Value               | Env Var                                                    |
| ------------------------------------------- | ------- | --------------------------- | ---------------------------------------------------------- |
| consumer.client.id                          | string  | ${HOSTNAME:-report-manager} | REPORT_MANAGER_CONSUMER_CLIENT_ID                          |
| consumer.group.id                           | string  | ${HOSTNAME:-report-manager} | REPORT_MANAGER_CONSUMER_GROUP_ID                           |
| consumer.metadata.broker.list               | string  | kafka:9092                  | REPORT_MANAGER_CONSUMER_METADATA_BROKER_LIST               |
| consumer.topic.metadata.refresh.interval.ms | integer | 30000                       | REPORT_MANAGER_CONSUMER_TOPIC_METADATA_REFRESH_INTERVAL_MS |
| topic.auto.offset.reset                     | string  | earliest                    | REPORT_MANAGER_TOPIC_AUTO_OFFSET_RESET                     |
| subscribe.topics.regex.tenants              | string  | ^.+dojot\.tenancy           | REPORT_MANAGER_SUBSCRIBE_TOPICS_REGEX_TENANTS              |

</details>

<details>
  <summary>Express</summary>

| Key                   | Type    | Default Value | Env Var                              |
| --------------------- | ------- | ------------- | ------------------------------------ |
| express.parsing.limit | integer | 256000        | REPORT_MANAGER_EXPRESS_PARSING_LIMIT |

</details>

<details>
  <summary>Redis</summary>

| Key        | Type    | Default Value        | Env Var                   |
| ---------- | ------- | -------------------- | ------------------------- |
| redis.host | string  | report-manager-redis | REPORT_MANAGER_REDIS_HOST |
| redis.port | integer | 6379                 | REPORT_MANAGER_REDIS_PORT |
| redis.db   | integer | 0                    | REPORT_MANAGER_REDIS_DB   |

</details>

<details>
  <summary>Keycloak</summary>

| Key                         | Type   | Default Value                            | Env Var                                    |
| --------------------------- | ------ | ---------------------------------------- | ------------------------------------------ |
| keycloak.url                | string | http://keycloak:8080                     | REPORT_MANAGER_KEYCLOAK_URL                |
| keycloak.tenants.url        | string | http://keycloak-proxy:8081/api/v1/tenant | REPORT_MANAGER_KEYCLOAK_TENANTS_URL        |
| keycloak.client.id          | string | dojot-report-manager                     | REPORT_MANAGER_KEYCLOAK_CLIENT_ID          |
| keycloak.client.secret.file | string | dojot-report-manager                     | REPORT_MANAGER_KEYCLOAK_CLIENT_SECRET_FILE |

</details>

<details>
  <summary>APIs</summary>

| Key            | Type   | Default Value                  | Env Var                       |
| -------------- | ------ | ------------------------------ | ----------------------------- |
| apis.retriever | string | http://influxdb-retriever:4000 | REPORT_MANAGER_APIS_RETRIEVER |
| apis.filemgmt  | string | http://file-mgmt:7000          | REPORT_MANAGER_APIS_FILEMGMT  |

</details>

<details>
  <summary>Postgres</summary>

| Key               | Type    | Default Value  | Env Var                          |
| ----------------- | ------- | -------------- | -------------------------------- |
| postgres.user     | string  | postgres       | REPORT_MANAGER_POSTGRES_USER     |
| postgres.password | string  | postgres       | REPORT_MANAGER_POSTGRES_PASSWORD |
| postgres.host     | string  | postgres       | REPORT_MANAGER_POSTGRES_HOST     |
| postgres.port     | integer | 5432           | REPORT_MANAGER_POSTGRES_PORT     |
| postgres.database | string  | report-manager | REPORT_MANAGER_POSTGRES_DATABASE |

</details>

<details>
  <summary>Service State Manager (Lightship)</summary>

These parameters are passed directly to the SDK ServiceStateManager. Check the [official repository](https://github.com/dojot/dojot-microservice-sdk-js) for more info on the values.

| Key                                 | Default Value | Valid Values | Environment variable                               |
| ----------------------------------- | ------------- | ------------ | -------------------------------------------------- |
| lightship.detect.kubernetes         | false         | boolean      | REPORT_MANAGER_LIGHTSHIP_DETECT_KUBERNETES         |
| lightship.graceful.shutdown.timeout | 60000         | number       | REPORT_MANAGER_LIGHTSHIP_GRACEFUL_SHUTDOWN_TIMEOUT |
| lightship.port                      | 9000          | number       | REPORT_MANAGER_LIGHTSHIP_PORT                      |
| lightship.shutdown.delay            | 5000          | number       | REPORT_MANAGER_LIGHTSHIP_SHUTDOWN_DELAY            |
| lightship.shutdown.handler.timeout  | 5000          | number       | REPORT_MANAGER_LIGHTSHIP_SHUTDOWN_HANDLER_TIMEOUT  |

</details>

## :computer: Development

You will need to have [NodeJS](https://nodejs.org/en/) and [Yarn](https://yarnpkg.com/) installed in your computer.

To start developing follow these steps:

```shell
# 1 - Install dependencies
yarn install
# 2 - Start the development server
yarn dev
```

## :black_large_square: Scripts

There are many scripts you can use. See the `scripts` object of the [package.json](package.json) file. You can run all scripts with yarn: `yarn SCRIPT_NAME`

## :card_index_dividers: Create Migrations

Prisma needs a database connection to create a new migration, so we need to run a postgres instance locally with docker and then run prisma commands to create the migration.

To make easier the process of creating a migration, some files were created:

- [migration.yml](docker/migration.yml) - It's a config file for docker compose to run a postgres instance locally.
- [create_migration.sh](scripts/create_migration.sh) - It's a bash script that runs the postgres instance and the prisma command to create a migration automatically.

So, in fact, you just need to open a terminal window at the project's root folder and run this:

```bash
sh scripts/create_migration.sh
```

_OBS: You need to modify the [schema.prisma](prisma/schema.prisma) first!_

## :eyes: Things To Pay Attention

**Prisma Seeds**

- The seeds run for each tenant, which means that the seeds will run for each schema, in practice.
- The seeds run for all existing tenants when the docker container is being started.
- Be careful with seeds, because they cannot be duplicated in the database. So, be sure to use the `skipDuplicates` param of the `createMany` function or to check for duplicates before creating a record.

**Controllers**

- Make sure to always call the express `next` function at the end of the controller. It is important for prisma to disconnect from the database at the end. Without calling `next` the subsequent middleware will not execute and the connection will stay alive indefinitely.
- Make sure to call the express `next(e)` function passing the error inside the `catch` block. It is necessary for express to run all error handlers.

**Routes**

- Every express route must use the `DisconnectPrismaInterceptor` to disconnect from database after executing the controller method.

**Locales**

- All JSON files support just one level of nesting, so don't use objects in these files, it will not work.
- Make sure to add the same keys on all JSON files. The values can be different, but they should have the same keys.

## :man: Author

Luan Eduardo da Costa
