# InfluxDB Retriever

The **InfluxDB Retriever** is responsible for retrieving data from device data time series in InfluxDB.

## **Table of Contents**

1. [Overview](#overview)
   1. [Mapping between InfluxDB and dojot devices](#reading-data-from-influxdb)
2. [Dependencies](#dependencies)
   1. [Dojot Services](#dojot-services)
   2. [Others Services](#others-services)
3. [Running the service](#running-the-service)
   1. [Configurations](#configurations)
      1. [General Configurations](#general-configurations)
      2. [Server Configurations](#server-configurations)
      3. [InfluxDB Configurations](#influxdb-configurations)
      4. [graphQL Configurations](#graphql-configurations)
      5. [Service State Manager](#service-state-manager)
   2. [How to run](#how-to-run)
4. [Documentation](#documentation)
5. [Issues and help](#issues-and-help)

## Overview

The **InfluxDB Retriever** service is used when it is necessary to obtain device data time series. Through its REST interface it is possible to apply time period filters, use pagination and use ordination. The link to the API documentation for the available endpoints:

- [Latest retriever API documentation](https://dojot.github.io/dojot/storage/time-series/influxdb/retriever/doc.html)
- [Development retriever API documentation](https://dojot.github.io/dojot/storage/time-series/influxdb/retriever/doc.html?version=development)

In addition, there is an endpoint for using version 1 documentation (the only one so far) interactively. The url to use it follows this pattern:

`http{s}://{host}:{port}/tss/v1/api-docs/`

For example, the address could be:

`http://localhost:3000/tss/v1/api-docs/`

### Mapping between InfluxDB and dojot devices

The mapping between the elements of influxdb and dojot devices are:

- _Organization_: It will be the **tenant**
- _Measurement_: It will be the **deviceId**
- _Bucket_ : By default it is called _devices_, but this value can be changed in Storer.
- _Fields_: Each `key` from `attrs` will be a _field_ beginning with 'dojot.' with their respective values and with its respective values being parsed in a JSON.

## Dependencies

The services dependencies are listed in the next topics.

- Dojot Services
- Others Services: They are external services;

### Dojot Services

none

### Others Services

- InfluxDB (tested using InfluxDB version 2.0.2)

## Running the service

### Configurations

Before running the **InfluxDB Storer** service within your environment, make sure you configure the
environment variables to match your needs.

You can select the configuration file via the `RETRIEVER_APP_USER_CONFIG_FILE` variable. Its default value
is `production.conf`. Check the [config directory](./config) for the user configurations that are
available by default.

For more information about the usage of the configuration files and environment variables, check the
**ConfigManager** module in our [Microservice SDK](https://github.com/dojot/dojot-microservice-sdk-js).
You can also check the [ConfigManager environment variables documentation](https://github.com/dojot/dojot-microservice-sdk-js/blob/master/lib/configManager/README.md#environment-variables) for more details.

In short, all the parameters in the next sections are mapped to environment variables that begin
with `RETRIEVER_`. You can either use environment variables or configuration files to change their values.
You can also create new parameters via environment variables by following the fore mentioned
convention.

#### General Configurations

| Key                        | Purpose                                                                         | Default Value | Valid Values             | Environment variable                 |
| -------------------------- | ------------------------------------------------------------------------------- | ------------- | ------------------------ | ------------------------------------ |
| log.console.level          | Console logger level                                                            | info          | info, debug, error, warn | RETRIEVER_LOG_CONSOLE_LEVEL          |
| log.file                   | Enables logging on file (location: /var/log/influxdb-retriever-logs-%DATE%.log) | false         | boolean                  | RETRIEVER_LOG_FILE                   |
| log.file.level             | Log level to log on files                                                       | info          | string                   | RETRIEVER_LOG_FILE_LEVEL             |
| log.verbose                | Whether to enable logger verbosity or not                                       | false         | boolean                  | RETRIEVER_LOG_VERBOSE                |
| express.trustproxy         | Enables reverse proxy support                                                   | true          | boolean                  | RETRIEVER_EXPRESS_TRUSTPROXY         |
| paginate.default.max.limit | Sets the default/maximum number of records/points in a page.                    | 256           | integer                  | RETRIEVER_PAGINATE_DEFAULT_MAX_LIMIT |

#### Server Configurations

| Key                        | Purpose                                                                                                                                                           | Default Value | Valid Values | Environment variable                 |
| -------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------- | ------------ | ------------------------------------ |
| server.host                | Server address                                                                                                                                                    | 0.0.0.0       | string       | RETRIEVER_SERVER_HOST                |
| server.port                | Sever Port                                                                                                                                                        | 3000          | integer      | RETRIEVER_SERVER_PORT                |
| server.ca                  | File path to list of supplied CAs. If passed enable TLS                                                                                                           | none          | path         | RETRIEVER_SERVER_CA                  |
| server.cert                | File path to certificate.                                                                                                                                         | none          | path         | RETRIEVER_SERVER_CERT                |
| server.key                 | File path to key certificate.                                                                                                                                     | none          | path         | RETRIEVER_SERVER_KEY                 |
| server.reject.unauthorized | If true, the server certificate is verified against the list of supplied CAs. It is considered only if 'server.ca' is passed.                                     | none          | boolean      | RETRIEVER_SERVER_REJECT_UNAUTHORIZED |
| server.request.cert        | Whether to authenticate the remote peer by requesting a certificate. Clients always request a server certificate. It is considered only if 'server.ca' is passed. | none          | boolean      | RETRIEVER_SERVER_REQUEST_CERT        |

#### InfluxDB Configurations

| Key                   | Purpose                                                                           | Default Value        | Valid Values | Environment variable            |
| --------------------- | --------------------------------------------------------------------------------- | -------------------- | ------------ | ------------------------------- |
| influx.default.bucket | Bucket name for all created buckets                                               | devices              | string       | RETRIEVER_INFLUX_DEFAULT_BUCKET |
| influx.default.token  | Configure a token (this token will be allowed to write/read in all organizations) | dojot@token_default  | string       | RETRIEVER_INFLUX_DEFAULT_TOKEN  |
| influx.heathcheck.ms  | Defines how often the communication with InfluxDB is verified in milliseconds.    | 30000                | integer      | RETRIEVER_INFLUX_HEATHCHECK_MS  |
| influx.max.timeout.ms | Request timeout in the communication with the influxdb in milliseconds.           | 30000                | integer      | RETRIEVER_INFLUX_MAX_TIMEOUT_MS |
| influx.url            | Address of the _InfluxDB_ service                                                 | http://influxdb:8086 | url          | RETRIEVER_INFLUX_URL            |

#### GraphQL Configurations

| Key              | Purpose                       | Default Value | Valid Values | Environment variable |
| ---------------- | ----------------------------- | ------------- | ------------ | -------------------- |
| graphql.graphiql | Used to enable the GraphQL UI | false         | boolean      | GRAPHQL_EDITOR       |

#### Service State Manager

These parameters are passed directly to the SDK ServiceStateManager. Check the
[official repository](https://github.com/dojot/dojot-microservice-sdk-js) for more info on the
values.

| Key                                 | Default Value | Valid Values | Environment variable                          |
| ----------------------------------- | ------------- | ------------ | --------------------------------------------- |
| lightship.detect.kubernetes         | false         | boolean      | RETRIEVER_LIGHTSHIP_DETECT_KUBERNETES         |
| lightship.graceful.shutdown.timeout | 60000         | number       | RETRIEVER_LIGHTSHIP_GRACEFUL_SHUTDOWN_TIMEOUT |
| lightship.port                      | 9000          | number       | RETRIEVER_LIGHTSHIP_PORT                      |
| lightship.shutdown.delay            | 5000          | number       | RETRIEVER_SHUTDOWN_DELAY                      |
| lightship.shutdown.handler.timeout  | 5000          | number       | RETRIEVER_SHUTDOWN_HANDLER_TIMEOUT            |

### How to run

Beforehand, you need an already running dojot instance in your machine. Check out the
[dojot documentation](https://dojotdocs.readthedocs.io)
for more information on installation methods.

Generate the Docker image:

```shell
docker build -t <username>/influxdb-retriever:<tag> -f  .
```

Then an image tagged as `<username>/influxdb-retriever:<tag>` will be made available. You can send it to
your DockerHub registry to made it available for non-local dojot installations:

```shell
docker push <username>/influxdb-retriever:<tag>
```

**NOTE THAT** you can use the official image provided by dojot in its [DockerHub page](https://hub.docker.com/r/dojot/influxdb-retriever).

## Documentation

Check the documentation for more information:

- [Latest InfluxDB Retriever API documentation](https://dojot.github.io/dojot/storage/time-series/influxdb/retriever/doc.html)
- [Development InfluxDB Retriever API documentation](https://dojot.github.io/dojot/storage/time-series/influxdb/retriever/doc.html?version=development)
- [Latest dojot platform documentation](https://dojotdocs.readthedocs.io/en/latest)

## Issues and help

If you found a problem or need help, leave an issue in the main
[dojot repository](https://github.com/dojot/dojot) and we will help you!
