# InfluxDB Storer

The **InfluxDB Storer** is responsible for consuming dojot's messages from Apache Kafka and creating device data series at InfluxDB.


## **Table of Contents**

1. [Overview](#overview)
    1. [Dojot's messages from Kafka topics](#dojots-messages-from-kafka-topics)
    2. [Writing data from kafka in InfluxDB](#writing-data-from-kafka-in-influxdb)
2. [Dependencies](#dependencies)
   1. [Dojot Services](#dojot-services)
   2. [Others Services](#others-services)
3. [Running the service](#running-the-service)
   1. [Configurations](#configurations)
        1. [General Configurations](#general-configurations)
        2. [InfluxDB Configurations](#influxdb-configurations)
            1. [InfluxDB Write Options](#influxdb-write-options)
        3. [SDK Consumer](#sdk-consumer)
            1. [Main object](#main-object)
            2. [kafka.consumer object](#kafkaconsumer-object)
            3. [kafka.topic object](#kafkatopic-object)
        4. [Service State Manager](#service-state-manager)
   2. [How to run](#how-to-run)
4. [Documentation](#documentation)
5. [Issues and help](#issues-and-help)

## Overview

### Dojot's messages from Kafka topics

The **InfluxDB-Storer** consumes the following dojot's messages:

- `*.dojot.tenancy` (Messages related to tenants' life cycle)
  - `CREATE` (Tenant was created):

    ```
        {
            "type": "CREATE",
            "tenant": <string: tenant>
        }
    ```

  - `DELETE` (Tenant was deleted):

    ```
        {
            "type": "DELETE",
            "tenant": <string: tenant>
        }
    ```

- `*.device-data` (Messages sent by devices to Dojot, publications)

    ```
    {
        "metadata": {
            "deviceid": <string:device ID>,
            "tenant": <string: tenant>,
            "timestamp": <integer: unix timestamp ms> | <string: Date-time RFC3339>,
            "shouldPersist": <boolean, is optional>
        },
        "attrs":{
            <string: attribute name>: <any JSON type>,
            …
            <string: attribute name>: <any JSON type>
        }
    }
    ```

- `*.dojot.device-manager.device` (Topic that receives messages sent by Dojot to devices and messages from device lifecycle events)

  - `configure`(Messages sent by dojot to devices, actuation):

    ```
        {
            "event": "configure",
            "meta": {
                "timestamp": <integer: unix timestamp ms> | <string: Date-time RFC3339>
                "service": <string: tenant>,
                "shouldPersist": <boolean, is optional>
            },
            "data" : {
                "id" : <string: device ID>,
                "attrs": {
                    <string: attribute name>: <any JSON type>,
                    …
                    <string: attribute name>: <any JSON type>
                }
            }
        }
    ```

  - `remove` (Device deletion event):

    ```
        {
            "event":"remove",
            "meta":{
                "service": <string: tenant>,
            },
            "data":{
                "id": <string: device ID>,
            }
        }
    ```

Whereas that:

- `Any JSON type` means:
  - a string
  - a number
  - an object (JSON object)
  - an array
  - a boolean
  - null
- [`Date-time RFC3339`](https://tools.ietf.org/html/rfc3339#section-5.6) means:
  - A string described in RFC3339. Example: YYYY-MM-DDThh:mm:ss.fffffffffZ.
  - It can handle up to nanosecond precision  in the *time-secfrac* part.
  - It can handle accurately up to nanoseconds, the rest will be discarded.
- The key`shouldPersist` in attrs (is optional) means:
  - if this key does not exist or its value is `true`:  the message attributes will be persisted.
  - its value is `false` :  the message attributes will not be persisted.

### Writing data from kafka in InfluxDB

In this section the idea is to explain what this service does with each message it consumes.

- `*.dojot.tenancy`
  - `CREATE`: This message will trigger the creation of a new *Organization* (**tenant**).
  - `DELETE`: This message will trigger the deletion of an existing *Organization* (**tenant**).

- `*.device-data`: This message will trigger a data insertion. Its `attrs` will be saved in a *measurement* (**deviceid**), in the default *bucket* and in an *Organization* (**tenant**). Each `key` from `attrs` will be a *field* beginning with 'dojot.' with their respective values being serialized to a string.

- `*.dojot.device-manager.device`
  - `configure`: The same behavior as  `*.device-data`.
  - `remove`: This message will trigger the deletion of a *measurement* (**deviceid**) in an *Organization* (**tenant**).


__NOTE THAT__ When service starts a default Organization with a default bucket, a default user with a default password and a default token must have **already been created**, optionally with a retention. You need to configure all of these values, see more at [general configurations](#general-configurations).

## Dependencies

The services dependencies are listed in the next topics.

- Dojot Services
- Others Services: They are external services;

### Dojot Services

none

### Others Services

- Kafka (tested using Kafka version 2.12)
- InfluxDB (tested using InfluxDB version 2.0.2)

## Running the service

### Configurations

Before running the **InfluxDB Storer** service within your environment, make sure you configure the
environment variables to match your needs.

You can select the configuration file via the `STORER_APP_USER_CONFIG_FILE` variable. Its default value
is `production.conf`. Check the [config directory](./config) for the user configurations that are
available by default.

For more information about the usage of the configuration files and environment variables, check the
__ConfigManager__ module in our [Microservice SDK](https://github.com/dojot/dojot-microservice-sdk-js).
You can also check the [ConfigManager environment variables documentation](https://github.com/dojot/dojot-microservice-sdk-js/blob/master/lib/configManager/README.md#environment-variables) for more details.

In short, all the parameters in the next sections are mapped to environment variables that begin
with `STORER_`. You can either use environment variables or configuration files to change their values.
You can also create new parameters via environment variables by following the fore mentioned
convention.

#### General Configurations

| Key | Purpose | Default Value | Valid Values | Environment variable
| --- | ------- | ------------- | ------------ | --------------------
| delete.device.data.enable | Deletes all data related to a device (measurement) if it was deleted. (Influxdb has some limitations in this version and it still doesn't work, so the default is false) | false | boolean  | STORER_DELETE_DEVICE_DATA_ENABLE
| delete.tenant.data.enable | Deleted all data related to a tenant (organization) if it was deleted.  | true | boolean  | STORER_DELETE_TENANT_DATA_ENABLE
| log.console.level | Console logger level | info | info, debug, error, warn | STORER_LOG_CONSOLE_LEVEL
| log.file | Enables logging on file (location: /var/log/influxdb-storer-logs-%DATE%.log) | false | boolean  | STORER_LOG_FILE
| log.file.level  | Log level to log on files | info | string  | STORER_LOG_FILE_LEVEL
| log.verbose | Whether to enable logger verbosity or not | false | boolean | STORER_LOG_VERBOSE
| kafka.heathcheck.ms | Specific how often it is to check if it is possible to communicate with the *kafka* service in milliseconds.  | 30000 | integer  | STORER_KAFKA_HEATHCHECK_MS
| subscribe.topics.suffix.device.data  | Suffix for the dojot topic that receives data from devices. | device-data | string  | STORER_SUBSCRIBE_TOPICS_SUFFIX_DEVICE_DATA
| subscribe.topics.suffix.device.manager | Suffix for the dojot topic that sends data to devices and receives device lifecycle events. | dojot.device-manager.device | string  | STORER_SUBSCRIBE_TOPICS_SUFFIX_DEVICE_MANAGER
| subscribe.topics.suffix.tenants  | Suffix for dojot topic that receives exclusion and tenant creation events. | dojot.tenancy | string  | STORER_SUBSCRIBE_TOPICS_SUFFIX_TENANTS

#### InfluxDB Configurations

| Key | Purpose | Default Value | Valid Values | Environment variable
| --- | ------- | ------------- | ------------ | --------------------
| influx.default.bucket | Bucket name for all created buckets and must exist in the organization configured in `influx.default.organization`. | devices | string  | STORER_INFLUX_DEFAULT_BUCKET
| influx.default.organization | Set up the name of the initial organization, must be configured before starting this service and must have the bucket configured at `influx.default.bucket`. | admin | string  | STORER_INFLUX_DEFAULT_ORGANIZATION
| influx.default.token | Configure a token (this token should be allowed to write/read in all organizations), must be configured before starting this service. | dojot@token_default | string  | STORER_INFLUX_DEFAULT_TOKEN
| influx.heathcheck.ms | Defines how often the communication with InfluxDB is verified in milliseconds.   | 30000 | integer  | STORER_INFLUX_HEATHCHECK_MS
| influx.retention.hrs | Data retention time (expiration) in hours (0 is infinite retention). Settings used only for organizations created by this service and does not change retention in existing buckets. | 168 | integer | STORER_INFLUX_RETENTION_HRS
| influx.url | Address of the *InfluxDB* service  | http://influxdb:8086 | url | STORER_INFLUX_URL

##### InfluxDB Write Options

You can pass any configuration available at https://influxdata.github.io/influxdb-client-js/influxdb-client.writeoptions.html, just be careful with valid values other than strings.(only note the pattern camelCase to lowercase with `.`).

| Key | Purpose | Default Value | Valid Values | Environment variable
| --- | ------- | ------------- | ------------ | --------------------
| influx.write.options.batch.size | Maximum number of records to send in a batch | 10000 | integer  | STORER_INFLUX_WRITE_OPTIONS_BATCH_SIZE
| influx.write.options.flush.interval | Maximum time in milliseconds to keep points in an unflushed batch (0 means don't periodically flush, only when service is down)  | 1 | integer  | STORER_INFLUX_WRITE_OPTIONS_FLUSH_INTERVAL
| influx.write.options.max.retries | Maximum number of retries follow an exponential backoff strategy | 3  | integer  | STORER_INFLUX_WRITE_OPTIONS_MAX_RETRIES
| influx.write.options.max.retry.delay | Maximum delay between retries in milliseconds | 15000  | integer | STORER_INFLUX_WRITE_OPTIONS_MAX_RETRY_DELAY
| influx.write.options.min.retry.delay | Minimum delay between retries in milliseconds | 1000  | integer  | STORER_INFLUX_WRITE_OPTIONS_MIN_RETRY_DELAY
| influx.write.options.max.buffer.lines | Maximum size of the retry buffer - it contains items that could not be sent for the first time  | 60000  | integer  | STORER_INFLUX_WRITE_OPTIONS_MAX_BUFFER_LINES

#### SDK Consumer

These parameters are passed directly to the SDK consumer. Check the
[official repository](https://github.com/dojot/dojot-microservice-sdk-js) for more info on the
values.

##### **Main object**

Key | Default Value | Valid Values | Environment variable
--- | ------------- | ------------ | --------------------
sdk.in.processing.max.messages | 1 | integer | STORER_SDK_IN_PROCESSING_MAX_MESSAGES
sdk.queued.max.messages.bytes | 10485760 | integer | STORER_SDK_QUEUED_MAX_MESSAGES_BYTES
sdk.subscription.backoff.min.ms | 1000 | integer | STORER_SDK_SUBSCRIPTION_BACKOFF_MIN_MS
sdk.subscription.backoff.max.ms | 60000 | integer | STORER_SDK_SUBSCRIPTION_BACKOFF_MAX_MS
sdk.subscription.backoff.delta.ms | 1000 | integer | STORER_SDK_SUBSCRIPTION_BACKOFF_DELTA_MS
sdk.commit.interval.ms | 5000 | integer | STORER_SDK_COMMIT_INTERVAL_MS

##### **kafka.consumer object**

Key | Default Value | Valid Values | Environment variable
--- | ------------- | ------------ | --------------------
consumer.client.id | ${HOSTNAME:-influxdb-storer} | string | STORER_CONSUMER_CLIENT_ID
consumer.group.id | influxdb-storer | string | STORER_CONSUMER_GROUP_ID
consumer.metadata.broker.list | kafka-server:9092 | Initial list of brokers as a CSV list of broker host or host:port.  | STORER_CONSUMER_METADATA_BROKER_LIST
consumer.topic.metadata.refresh.interval.ms | 30000 | milliseconds (integer) | STORER_CONSUMER_TOPIC_METADATA_REFRESH_INTERVAL_MS

##### **kafka.topic object**

| Key | Default Value | Valid Values | Environment variable
| --- | ------------- | ------------ | --------------------
| topic.auto.offset.reset | earliest | smallest, earliest, beginning, largest, latest, end, error | STORER_TOPIC_AUTO_OFFSET_RESET

#### Service State Manager

These parameters are passed directly to the SDK ServiceStateManager. Check the
[official repository](https://github.com/dojot/dojot-microservice-sdk-js) for more info on the
values.

| Key | Default Value | Valid Values | Environment variable
| --- | ------------- | ------------ | --------------------
| lightship.detect.kubernetes | false | boolean | STORER_LIGHTSHIP_DETECT_KUBERNETES
| lightship.graceful.shutdown.timeout | 120000 | number | STORER_LIGHTSHIP_GRACEFUL_SHUTDOWN_TIMEOUT
| lightship.port | 9000 | number | STORER_LIGHTSHIP_PORT
| lightship.shutdown.delay | 5000 | number | STORER_SHUTDOWN_DELAY
| lightship.shutdown.handler.timeout | 15000 | number | STORER_SHUTDOWN_HANDLER_TIMEOUT

### How to run

Beforehand, you need an already running dojot instance in your machine. Check out the
[dojot documentation](https://dojotdocs.readthedocs.io)
for more information on installation methods.

Generate the Docker image:

```shell
docker build -t <username>/influxdb-storer:<tag> -f  .
```

Then an image tagged as`<username>/influxdb-storer:<tag>` will be made available. You can send it to your DockerHub registry to made it available for non-local dojot installations:

```shell
docker push <username>/influxdb-storer:<tag>
```

__NOTE THAT__  you can use the official image provided by dojot in its  [DockerHub page](https://hub.docker.com/r/dojot/influxdb-storer).

## Documentation

Check the documentation for more information:

- [Latest dojot platform documentation](https://dojotdocs.readthedocs.io/en/latest)

## Issues and help

If you found a problem or need help, leave an issue in the main
[dojot repository](https://github.com/dojot/dojot) and we will help you!
