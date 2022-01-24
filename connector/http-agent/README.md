# IoTAgent HTTP

IoT agents ought to receive messages from physical devices (directly or through a gateway) and send them commands in order to configure them. This IoT agent, receive messages via HTTP with JSON payloads.

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

HTTPS request:

```HTTP
  POST /http-agent/v1/incoming-messages/ \
    -H 'content-type: application/json' \
    -d '{
      "ts": "2021-06-16T09:32:01.683000Z",
      "data": {
        "temperature": 25.79,
        "heartRate": 70,
        "respiratoryRate": 40,
        "battery": 3
      }
  }' \
  --cacert ca.crt \
  --cert exemple.crt \
  --key exemple.key
```

or

```HTTP
  POST /http-agent/v1/incoming-messages/create-many \
    -H 'content-type: application/json' \
    -d '[
      {
        "ts": "2021-06-16T09:32:01.683000Z",
        "data": {
          "temperature": 25.79,
          "heartRate": 70,
          "respiratoryRate": 40,
          "battery": 3
        }
      },
      {
        "data": {
          "temperature": 25.79,
          "heartRate": 70,
          "respiratoryRate": 40,
          "battery": 3
        }
    }
  ]' \
  --cacert ca.crt \
  --cert exemple.crt \
  --key exemple.key
```

HTTPS response:

```HTTP
  HTTP/1.1 204 No Content
```

## General Configurations

These are the environment variables used by iotagent-http

### Lightship

| Key                         | Default Value | Valid Values | Environment variable                   |
|-----------------------------|---------------|--------------|----------------------------------------|
| lightship.detect.kubernetes | false         | boolean      | HTTP_AGENT_LIGHTSHIP_DETECT_KUBERNETES |

### Logger config

| Key               | Default Value                            | Valid Values             | Environment variable         |
|-------------------|------------------------------------------|--------------------------|------------------------------|
| log.verbose       | false                                    | boolean                  | HTTP_AGENT_LOG_VERBOSE       |
| log.console.level | info                                     | info, debug, error, warn | HTTP_AGENT_LOG_CONSOLE_LEVEL |
| log.file          | false                                    | boolean                  | HTTP_AGENT_LOG_FILE          |
| log.file.level    | info                                     | info, debug, error, warn | HTTP_AGENT_LOG_FILE_LEVEL    |
| log.file.filename | http-agent-${HOSTNAME:-}-logs-%DATE%.log | string                   | HTTP_AGENT_LOG_FILE_FILENAME |

### Kafka Producer

| Key                                            | Default Value | Valid Values                                                       | Environment variable                             |
|------------------------------------------------|---------------|--------------------------------------------------------------------|--------------------------------------------------|
| producer.metadata.broker.list                  | kafka:9092    | Initial list of brokers as a CSV list of broker host or host:port. | HTTP_AGENT_PRODUCER_METADATA_BROKER_LIST         |
| producer.compression.codec                     | gzip          | string                                                             | HTTP_AGENT_COMPRESSION_CODE                      |
| producer.retry.backoff.ms                      | 200           | integer                                                            | HTTP_AGENT_RETRY_BACKOFF_MS                      |
| producer.message.send.max.retries              | 10            | integer                                                            | HTTP_AGENT_MESSAGE_SEND_MAX_RETRIES              |
| producer.socket.keepalive.enable               | true          | boolean                                                            | HTTP_AGENT_SOCKET_KEEPALIVE_ENABLE               |
| producer.queue.buffering.max.messages          | 100000        | integer                                                            | HTTP_AGENT_QUEUE_BUFFERING_MAX_MESSAGES          |
| producer.queue.buffering.max.ms                | 1000          | integer                                                            | HTTP_AGENT_QUEUE_BUFFERING_MAX_MS                |
| producer.batch.num.messages                    | 1000000       | integer                                                            | HTTP_AGENT_BATCH_NUM_MESSAGES                    |
| producer.dr_cb                                 | true          | boolean                                                            | HTTP_AGENT_DR_CB                                 |
| producer.enable.idempotence                    | false         | boolean                                                            | HTTP_AGENT_ENABLE_IDEMPOTENCE                    |
| producer.max.in.flight.requests.per.connection | 1000000       | integer                                                            | HTTP_AGENT_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION |

### Kafka SDK

| Key                       | Default Value | Valid Values | Environment variable             |
|---------------------------|---------------|--------------|----------------------------------|
| sdk.connect.timeout.ms    | 5000          | integer      | HTTP_AGENT_CONNECT_TIMEOUT_MS    |
| sdk.disconnect.timeout.ms | 10000         | integer      | HTTP_AGENT_DISCONNECT_TIMEOUT_MS |
| sdk.flush.timeout.ms      | 2000          | integer      | HTTP_AGENT_FLUSH_TIMEOUT_MS      |

### Kafka Topic

| Key                     | Default Value | Valid Values                                               | Environment variable               |
|-------------------------|---------------|------------------------------------------------------------|------------------------------------|
| topic.auto.offset.reset | earliest      | smallest, earliest, beginning, largest, latest, end, error | HTTP_AGENT_TOPIC_AUTO_OFFSET_RESET |
| topic.acks              | -1            | integer                                                    | HTTP_AGENT_TOPIC_ACKS              |

### Kafka Messenger

| Key                            | Default Value | Valid Values | Environment variable                      |
|--------------------------------|---------------|--------------|-------------------------------------------|
| messenger.produce.topic.suffix | device-data   | string       | HTTP_AGENT_MESSENGER_PRODUCE_TOPIC_SUFFIX |

### Server

#### HTTPS

| Key                       | Default Value         | Valid Values | Environment variable                 |
|---------------------------|-----------------------|--------------|--------------------------------------|
| https.host                | 0.0.0.0               | string       | HTTP_AGENT_HTTPS_HOST                |
| https.port                | 3000                  | integer      | HTTP_AGENT_HTTPS_PORT                |
| https.ca                  | /certs/ca.crt         | string       | HTTP_AGENT_HTTPS_CA                  |
| https.key                 | /certs/http-agent.key | string       | HTTP_AGENT_HTTPS_KEY                 |
| https.cert                | /certs/http-agent.crt | string       | HTTP_AGENT_HTTPS_CERT                |
| https.request.cert        | true                  | boolean      | HTTP_AGENT_HTTPS_REQUEST_CERT        |
| https.reject.unauthorized | true                  | boolean      | HTTP_AGENT_HTTPS_REJECT_UNAUTHORIZED |

#### HTTP

| Key       | Default Value | Valid Values | Environment variable |
|-----------|---------------|--------------|----------------------|
| http.host | 0.0.0.0       | string       | HTTP_AGENT_HTTP_HOST |
| http.port | 3001          | integer      | HTTP_AGENT_HTTP_PORT |

### Secure Context

| Key                         | Default Value | Valid Values                | Environment variable                   |
|-----------------------------|---------------|-----------------------------|----------------------------------------|
| security.crl                | /certs/ca.crl | string                      | HTTP_AGENT_SECURITY_CRL                |
| security.cert.directory     | /certs        | string                      | HTTP_AGENT_SECURITY_CERT_DIRECTORY     |
| security.unsecure.mode      | false         | boolean                     | HTTP_AGENT_SECURITY_UNSECURE_MODE      |
| security.unsecure.mode.only | false         | boolean                     | HTTP_AGENT_SECURITY_UNSECURE_MODE_ONLY |
| security.authorization.mode | fingerprint   | fingerprint, cn, basic-auth | HTTP_AGENT_SECURITY_AUTHORIZATION_MODE |

### Reload

| Key                | Default Value | Valid Values | Environment variable          |
|--------------------|---------------|--------------|-------------------------------|
| reload.attempts    | 10            | integer      | HTTP_AGENT_RELOAD_ATTEMPTS    |
| reload.interval.ms | 1000          | integer      | HTTP_AGENT_RELOAD_INTERVAL_MS |

### Express

| Key                   | Default Value | Valid Values | Environment variable             |
|-----------------------|---------------|--------------|----------------------------------|
| express.trustproxy    | true          | boolean      | HTTP_AGENT_EXPRESS_TRUSTPROXY    |
| express.parsing.limit | 256000        | integer      | HTTP_AGENT_EXPRESS_PARSING.LIMIT |

### Redis

| Key                        | Default Value    | Valid Values | Environment variable                  |
|----------------------------|------------------|--------------|---------------------------------------|
| redis.host                 | http-agent-redis | string       | HTTP_AGENT_REDIS_HOST                 |
| redis.port                 | 6379             | integer      | HTTP_AGENT_REDIS_PORT                 |
| redis.db                   | 0                | integer      | HTTP_AGENT_REDIS_DB                   |
| redis.reconnect.after.ms   | 5000             | integer      | HTTP_AGENT_REDIS_RECONNECT_AFTER_MS   |
| redis.operation.timeout.ms | 1000             | integer      | HTTP_AGENT_REDIS_OPERATION_TIMEOUT_MS |

### URL

| Key                 | Default Value                                                | Valid Values | Environment variable           |
|---------------------|--------------------------------------------------------------|--------------|--------------------------------|
| url.device.auth     | http://basic-auth:3000/basic-auth/v1/internal/authentication | string       | HTTP_AGENT_URL_BASIC_AUTH      |
| url.certificate.acl | http://certificate-acl:3000/internal/api/v1/acl-entries      | string       | HTTP_AGENT_URL_CERTIFICATE_ACL |

## Notes

- If a timestamp is not part of the message sent by a device, it will be added by the http-agent.
- In the x509-identity-mgmt service be sure to set the X509IDMGMT_CERTIFICATE_CHECK_SUBJECTDN variable to "true".

## Documentation

Check the documentation for more information:

- [Latest dojot platform documentation](https://dojotdocs.readthedocs.io/en/latest)

## Issues and help

If you found a problem or need help, leave an issue in the main
[dojot repository](https://github.com/dojot/dojot) and we will help you!

# **License**

The http-agent source code is released under Apache License 2.0.

Check NOTICE and LICENSE files for more information.
