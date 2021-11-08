# Device Authentication

Authentication and authorization mechanism based on user and password.

## How to build

As this is a npm-based project, open the terminal and run

```
$ npm install
```

You can also build this component using docker, open the terminal on the project path and run

```
# you may need sudo on your machine: https://docs.docker.com/engine/installation/linux/linux-postinstall/
$ docker build -t <tag> .
```

## Examples

Request:

```HTTP
  POST /basic-auth/v1/:deviceId/basic-credentials \
    -H 'content-type: application/json' \
    -H 'Authorization: Bearer ${TOKEN}'
```

response:

```HTTP
  HTTP/1.1 200 OK
  Content-type: application/json

  {
    credentials: {
      username: 'tenant1@123abc',
      password: 'AOxRg!v1heGuQ0Y',
    },
    basicAuth: 'Basic dGVuYW50MUAxMjNhYmM6QU94UmchdjFoZUd1UTBZ',
  }


Request:

```HTTP
  POST /basic-auth/v1/internal/authentication \
    -H 'content-type: application/json' \
    -H 'Basic base64encode(username:password)'
```

HTTPS response:

```HTTP
  HTTP/1.1 200 OK
  Content-type: application/json

  {
    message: 'The credential is valid.',
  }
```

## General Configurations

These are the environment variables used by iotagent-http

### Lightship

| Key                         | Default Value | Valid Values | Environment variable                   |
|-----------------------------|---------------|--------------|----------------------------------------|
| lightship.detect.kubernetes | false         | boolean      | BASIC_AUTH_LIGHTSHIP_DETECT_KUBERNETES |

### Logger config

| Key               | Default Value                            | Valid Values             | Environment variable         |
|-------------------|------------------------------------------|--------------------------|------------------------------|
| log.verbose       | false                                    | boolean                  | BASIC_AUTH_LOG_VERBOSE       |
| log.console.level | info                                     | info, debug, error, warn | BASIC_AUTH_LOG_CONSOLE_LEVEL |
| log.file          | false                                    | boolean                  | BASIC_AUTH_LOG_FILE          |
| log.file.level    | info                                     | info, debug, error, warn | BASIC_AUTH_LOG_FILE_LEVEL    |
| log.file.filename | basic-auth-${HOSTNAME:-}-logs-%DATE%.log | string                   | BASIC_AUTH_LOG_FILE_FILENAME |

### Kafka Producer

| Key                                            | Default Value | Valid Values                                                       | Environment variable                                      |
|------------------------------------------------|---------------|--------------------------------------------------------------------|-----------------------------------------------------------|
| producer.metadata.broker.list                  | kafka:9092    | Initial list of brokers as a CSV list of broker host or host:port. | BASIC_AUTH_PRODUCER_METADATA_BROKER_LIST                  |
| producer.compression.codec                     | gzip          | string                                                             | BASIC_AUTH_PRODUCER_COMPRESSION_CODE                      |
| producer.retry.backoff.ms                      | 200           | integer                                                            | BASIC_AUTH_PRODUCER_RETRY_BACKOFF_MS                      |
| producer.message.send.max.retries              | 10            | integer                                                            | BASIC_AUTH_PRODUCER_MESSAGE_SEND_MAX_RETRIES              |
| producer.socket.keepalive.enable               | true          | boolean                                                            | BASIC_AUTH_PRODUCER_SOCKET_KEEPALIVE_ENABLE               |
| producer.queue.buffering.max.messages          | 100000        | integer                                                            | BASIC_AUTH_PRODUCER_QUEUE_BUFFERING_MAX_MESSAGES          |
| producer.queue.buffering.max.ms                | 1000          | integer                                                            | BASIC_AUTH_PRODUCER_QUEUE_BUFFERING_MAX_MS                |
| producer.batch.num.messages                    | 1000000       | integer                                                            | BASIC_AUTH_PRODUCER_BATCH_NUM_MESSAGES                    |
| producer.dr_cb                                 | true          | boolean                                                            | BASIC_AUTH_PRODUCER_DR_CB                                 |
| producer.enable.idempotence                    | false         | boolean                                                            | BASIC_AUTH_PRODUCER_ENABLE_IDEMPOTENCE                    |
| producer.max.in.flight.requests.per.connection | 1000000       | integer                                                            | BASIC_AUTH_PRODUCER_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION |

### SDK Producer

| Key                               | Default Value | Valid Values | Environment variable                         |
|-----------------------------------|---------------|--------------|----------------------------------------------|
| sdkproducer.connect.timeout.ms    | 5000          | integer      | BASIC_AUTH_SDKPRODUCER_CONNECT_TIMEOUT_MS    |
| sdkproducer.disconnect.timeout.ms | 10000         | integer      | BASIC_AUTH_SDKPRODUCER_DISCONNECT_TIMEOUT_MS |
| sdkproducer.flush.timeout.ms      | 2000          | integer      | BASIC_AUTH_SDKPRODUCER_FLUSH_TIMEOUT_MS      |

### Kafka Consumer

| Key                                            | Default Value           | Valid Values                                                       | Environment variable                                      |
|------------------------------------------------|-------------------------|--------------------------------------------------------------------|-----------------------------------------------------------|
| consumer.group.id                              | basic-auth              | string                                                             | BASIC_AUTH_CONSUMER_GROUP_ID                              |
| consumer.client.id                             | ${HOSTNAME:-basic-auth} | string                                                             | BASIC_AUTH_CONSUMER_CLIENT_ID                             |
| consumer.metadata.broker.list                  | kafka:9092              | Initial list of brokers as a CSV list of broker host or host:port. | BASIC_AUTH_CONSUMER_METADATA_BROKER_LIST                  |
| consumer.max.in.flight.requests.per.connection | 1000000                 | integer                                                            | BASIC_AUTH_CONSUMER_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION |
| consumer.socket.keepalive.enable               | false                   | boolean                                                            | BASIC_AUTH_CONSUMER_SOCKET_KEEPALIVE_ENABLE               |

### SDK Consumer

| Key                                       | Default Value | Valid Values | Environment variable                         |
|-------------------------------------------|---------------|--------------|----------------------------------------------|
| sdkconsumer.in.processing.max.messages    | 1             | integer      | BASIC_AUTH_SDKPRODUCER_CONNECT_TIMEOUT_MS    |
| sdkconsumer.queued.max.messages.bytes     | 10485760      | integer      | BASIC_AUTH_SDKPRODUCER_DISCONNECT_TIMEOUT_MS |
| sdkconsumer.subscription.backoff.min.ms   | 1000          | integer      | BASIC_AUTH_SDKPRODUCER_FLUSH_TIMEOUT_MS      |
| sdkconsumer.subscription.backoff.max.ms   | 60000         | integer      | BASIC_AUTH_SDKPRODUCER_CONNECT_TIMEOUT_MS    |
| sdkconsumer.subscription.backoff.delta.ms | 1000          | integer      | BASIC_AUTH_SDKPRODUCER_DISCONNECT_TIMEOUT_MS |
| sdkconsumer.commit.interval.ms            | 5000          | integer      | BASIC_AUTH_SDKPRODUCER_FLUSH_TIMEOUT_MS      |

### Kafka Topic

| Key                     | Default Value | Valid Values                                               | Environment variable               |
|-------------------------|---------------|------------------------------------------------------------|------------------------------------|
| topic.auto.offset.reset | earliest      | smallest, earliest, beginning, largest, latest, end, error | BASIC_AUTH_TOPIC_AUTO_OFFSET_RESET |
| topic.acks              | -1            | integer                                                    | BASIC_AUTH_TOPIC_ACKS              |

### Kafka Messenger

| Key                            | Default Value | Valid Values | Environment variable                      |
|--------------------------------|---------------|--------------|-------------------------------------------|
| messenger.produce.topic.suffix | device-data   | string       | BASIC_AUTH_MESSENGER_PRODUCE_TOPIC_SUFFIX |

### HTTP

| Key       | Default Value | Valid Values | Environment variable |
|-----------|---------------|--------------|----------------------|
| http.host | 0.0.0.0       | string       | BASIC_AUTH_HTTP_HOST |
| http.port | 3001          | integer      | BASIC_AUTH_HTTP_PORT |

### Express

| Key                   | Default Value | Valid Values | Environment variable             |
|-----------------------|---------------|--------------|----------------------------------|
| express.trustproxy    | true          | boolean      | BASIC_AUTH_EXPRESS_TRUSTPROXY    |
| express.parsing.limit | 256000        | integer      | BASIC_AUTH_EXPRESS_PARSING.LIMIT |

### Mongo

| Key                                         | Default Value                      | Valid Values | Environment variable                                   |
|---------------------------------------------|------------------------------------|--------------|--------------------------------------------------------|
| mongo.conn.uri                              | mongodb://mongodb:27017/basic-auth | string       | BASIC_AUTH_MONGO_CONN_URI                              |
| mongo.conn.options.autoindex                | true                               | boolean      | BASIC_AUTH_MONGO_CONN_OPTIONS_AUTOINDEX                |
| mongo.conn.options.poolsize                 | 100                                | integer      | BASIC_AUTH_MONGO_CONN_OPTIONS_POOLSIZE                 |
| mongo.conn.options.serverselectiontimeoutms | 30000                              | integer      | BASIC_AUTH_MONGO_CONN_OPTIONS_SERVERSELECTIONTIMEOUTMS |
| mongo.conn.options.heartbeatfrequencyms     | 10000                              | integer      | BASIC_AUTH_MONGO_CONN_OPTIONS_HEARTBEATFREQUENCYMS     |
| mongo.conn.options.sockettimeoutms          | 360000                             | integer      | BASIC_AUTH_MONGO_CONN_OPTIONS_SOCKETTIMEOUTMS          |
| mongo.conn.options.family                   | 0                                  | integer      | BASIC_AUTH_MONGO_CONN_OPTIONS_FAMILY                   |
| mongo.query.maxtimems                       | 30000                              | integer      | BASIC_AUTH_MONGO_QUERY_MAXTIMEMS                       |

### URL

| Key         | Default Value                              | Valid Values | Environment variable   |
|-------------|--------------------------------------------|--------------|------------------------|
| url.tenants | http://auth:5000/admin/tenants             | string       | BASIC_AUTH_URL_TENANTS |
| url.devices | http://device-manager:5000/device          | string       | BASIC_AUTH_URL_DEVICES |
| url.device  | http://device-manager:5000/internal/device | string       | BASIC_AUTH_URL_DEVICE  |

### Sync

| Key                  | Default Value | Valid Values | Environment variable            |
|----------------------|---------------|--------------|---------------------------------|
| sync.cron.expression | * */12 * * *  | string       | BASIC_AUTH_SYNC_CRON_EXPRESSION |


## Documentation

Check the documentation for more information:

- [Latest dojot platform documentation](https://dojotdocs.readthedocs.io/en/latest)

## generates and help

If you found a problem or need help, leave an issue in the main
[dojot repository](https://github.com/dojot/dojot) and we will help you!

# **License**

The Cron source code is released under Apache License 2.0.

Check NOTICE and LICENSE files for more information.
