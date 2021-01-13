# Kafka2Ftp

The **kafka2ftp** service provides a connector solution for forwarding messages from Apache Kafka to FTP servers.

## **Table of Contents**

1. [Overview](#overview)
   1. [Message forwarding service to FTP servers](#message-forwarding-service-to-ftp-servers)
   2. [Using with Flowbroker](#using-with-flowbroker)
2. [Dependencies](#dependencies)
   1. [Dojot Services](#dojot-services)
   2. [Others Services](#others-services)
3. [Running the service](#running-the-service)
   1. [Configuration](#configuration)
      1. [Environment variables](#environment-variables)
   2. [How to run](#how-to-run)
4. [Documentation](#documentation)
5. [Issues and help](#issues-and-help)

## Overview

### Message forwarding service to FTP servers

The **kafka2ftp** subscribes to the topic *tenant*.dojot.ftp (*tenant* is defined in the environment variable), in which messages are produced with information about the filename, encoding format and file content. These messages are processed by the service and sent to the corresponding FTP server. Example of a message received by this service:

```json
{
  "metadata": {
     "msgId": "33846252-659f-42cc-8831-e2ccb923a702",
     "ts": 1571858674,
     "service": "ServiceA",
     "contentType": "application/vnd.dojot.ftp+json"
  },
  "data": {
      "filename": "filename.jpg",
      "encoding": "base64",
      "content": "..."
   }
}

```

Where the keys above are:

Key             | Purpose
--------------- | --------------------------------------------------------
msgId           | Value of type *uuidv4* used to uniquely identify the message in the context of dojot.
ts              | Timestamp in Unix Timestamp (ms) format from the moment the message was produced.
service         | Name of the service that generated the message.
contentType     | Type identifier of the content of the data attribute.
filename        | Name of the file to be sent to the FTP server.
encoding        | Encoding the contents of the file. Valid values are: ascii, base64, hex, utf16le, utf8 and binary.
content         | File contents.

### Using with Flowbroker

You can use this service together with the [flowbroker](https://github.com/dojot/flowbroker) using the **Publish in FTP topic** node, see more in the [dojot platform documentation](https://dojotdocs.readthedocs.io)  on the topic *Using flow builder*.

## Dependencies

The services dependencies are listed in the next topics.

- Dojot Services: They are dojot services
- Others Services: They are external services

### Dojot Services

- Flowbroker >= v0.5.0 (Only if you are using the **Publish in FTP topic** node)

### Others Services

- Kafka (tested using Kafka version 2.12)

## Running the service

### Configuration

Before proceeding, **make sure you configure your environment**.

#### Environment variables

Key                        | Purpose                                                  | Default Value      | Valid Values |
-------------------------- | -------------------------------------------------------- | ---------------    | -----------  |
FTP_FTPS                   | Explicit FTPS over TLS                                   | false              | String (true or false)                |
FTP_HOST                   | Server FTP host                                          | localhost          | hostname/IP                       |
FTP_PASSWORD               | Server FTP Password                                      | guest              | String                       |
FTP_PORT                   | Server FTP port                                          | 21                 | Natural number                      |
FTP_REMOTE_DIR             | Path where files will be sent (This path must exist previously)   | /                  | Path                         |
FTP_TENANT                 | Tenant is a context identifier into dojot. This defines the topic on which the service will subscribe *tenant*.dojot.ftp                  | admin               | String  |
FTP_USER                   | Server FTP Username                                      | anonymous          | String                       |
KAFKA_GROUP_ID             | The Kafka consumer group ID to be used                   | kafka2ftp          | String       |
KAFKA_HOSTS                | Addresses of the kafka brokers separated by a comma      | kafka:9092         | hostname/IP  |
LOG_LEVEL                  | logger level                                             | info               | debug, error, warning, info  |
MAX_CONCURRENT_CONNECTIONS | The maximum number of FTP connections at the same time.                                            | 10                 | Natural number               |
RETRIES      | The maximum number of times to attempt the upload       | 13                 | Natural number               |

Note: Number of RETRIES sent uses the "exponential backoff" strategy. When RETRIES=13 using an exponential backoff strategy, this means the last attempt is made after 2h 16m 31 sec. The total time in ms can be calculated with _Sum[1000*2^k, {k, 0, n-1}]_ where *n=retries*.

## How to run

Beforehand, you need an already running dojot instance in your machine. Check out the
[dojot documentation](https://dojotdocs.readthedocs.io)
for more information on installation methods.

Generate the Docker image:

```shell
docker build -t <username>/kafka2ftp:<tag> -f  .
```

Then the image tagged a `<username>/kafka2ftp:<tag>` will be made available. You can send it to
your DockerHub registry to made it available for non-local dojot installations:

```shell
docker push <username>/kafka2ftp:<tag>
```

__NOTE THAT__  you can use the official image provided by dojot in its  [DockerHub page](https://hub.docker.com/r/dojot/kafka2ftp).

## Documentation

Check the documentation for more information:

- [Latest dojot platform documentation](https://dojotdocs.readthedocs.io/en/latest)

## Issues and help

If you found a problem or need help, leave an issue in the main
[dojot repository](https://github.com/dojot/dojot) and we will help you!