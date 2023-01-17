# FILE-MGMT

The "File-mgmt" service is responsible for storing files in dojot. Through the REST interface provided by the service, it's possible to list, upload, download and delete files.

## Running the service

### Configurations

Before running the **File-mgmt** service within your environment, make sure you configure the
environment variables to match your needs.

You can select the configuration file via the `FILEMGMT_APP_USER_CONFIG_FILE` variable. Its default value
is `production.conf`. Check the [config directory](./src/config) for the user configurations that are
available by default.


#### General Configurations

| Key                        | Purpose                                                                         | Default Value | Valid Values             | Environment variable                 |
| -------------------------- | ------------------------------------------------------------------------------- | ------------- | ------------------------ | ------------------------------------ |
| log.console.level          | Console logger level                                                            | info          | info, debug, error, warn | FILEMGMT_LOG_CONSOLE_LEVEL          |
| log.file                   | Enables logging on file (location: /var/log/influxdb-retriever-logs-%DATE%.log) | false         | boolean                  | FILEMGMT_LOG_FILE                   |
| log.file.level             | Log level to log on files                                                       | info          | string                   | FILEMGMT_LOG_FILE_LEVEL             |
| log.verbose                | Whether to enable logger verbosity or not                                       | false         | boolean                  | FILEMGMT_LOG_VERBOSE                |
| express.trustproxy         | Enables reverse proxy support                                                   | true          | boolean                  | FILEMGMT_EXPRESS_TRUSTPROXY         |

#### Server Configurations

| Key                        | Purpose                                                                         | Default Value | Valid Values             | Environment variable                 |
| -------------------------- | ------------------------------------------------------------------------------- | ------------- | ------------------------ | ------------------------------------ |
| server.host                | Server address                                                                                                                                                    | 0.0.0.0       | string       | FILEMGMT_SERVER_HOST                |
| server.port                | Sever Port                                                                                                                                                        | 7000          | integer      | FILEMGMT_SERVER_PORT                |


#### Kafka Consumer Configurations

Key                                         | Default Value             | Valid Values  | Environment variable
--------------------------------------------|---------------------------| ------------  | --------------------
consumer.client.id                          | ${HOSTNAME:-file-mgmt}    | string                | FILEMGMT_CONSUMER_CLIENT_ID
consumer.group.id                           | file-mgmt                 | string                | FILEMGMT_CONSUMER_GROUP_ID
consumer.metadata.broker.list               | kafka-server:9092         | Initial list of brokers as a CSV list of broker host or host:port.| FILEMGMT_CONSUMER_METADATA_BROKER_LIST
consumer.topic.metadata.refresh.interval.ms | 30000                     | milliseconds (integer)| FILEMGMT_CONSUMER_TOPIC_METADATA_REFRESH_INTERVAL_MS
topic.auto.offset.reset                     | earliest                  | string                | FILEMGMT_TOPIC_AUTO_OFFSET_RESET
subscribe.topics.regex.tenants              | ^.+dojot\.tenancy         | string                | SUBSCRIBE_TOPICS_REGEX_TENANTS
sdk.in.processing.max.messages | 1 | integer | FILEMGMT_SDK_IN_PROCESSING_MAX_MESSAGES
sdk.queued.max.messages.bytes | 10485760 | integer | FILEMGMT_SDK_QUEUED_MAX_MESSAGES_BYTES
sdk.subscription.backoff.min.ms | 1000 | integer | FILEMGMT_SDK_SUBSCRIPTION_BACKOFF_MIN_MS
sdk.subscription.backoff.max.ms | 60000 | integer | FILEMGMT_SDK_SUBSCRIPTION_BACKOFF_MAX_MS
sdk.subscription.backoff.delta.ms | 1000 | integer | FILEMGMT_SDK_SUBSCRIPTION_BACKOFF_DELTA_MS
sdk.commit.interval.ms | 5000 | integer | FILEMGMT_SDK_COMMIT_INTERVAL_MS



#### MinIO Configurations

| Key                        | Purpose                                                                         | Default Value | Valid Values             | Environment variable                 |
| -------------------------- | ------------------------------------------------------------------------------- | ------------- | ------------------------ | ------------------------------------ |
| minio.host                | MinIO address.                                            | minio-files   | string    | FILEMGMT_MINIO_HOST 
| minio.port                | MinIO port.                                               | 9000          | integer   | FILEMGMT_MINIO_PORT
| minio.ssl                 | Sets the use of SSL.                                      | false         | boolean   | FILEMGMT_MINIO_SSL
| minio.accessKey           | MinIO access key.                                         |    -          | string    | FILEMGMT_MINIO_ACCESSKEY 
| minio.secretKey           | MinIO secret key.                                         |    -          | string    | FILEMGMT_MINIO_SECRETKEY
| minio.upload.size.limit   | Sets the file size limit.                                 | 26214400      | string    | FILEMGMT_MINIO_UPLOAD_SIZE_LIMIT
| minio.presigned.expiry    | Sets the expiration time of urls generated by the service.| 900           | string    | FILEMGMT_PRESIGNED_EXPIRY
| minio.bucket.suffix       | Sets the suffix for the bucket names key.                 | cpqd.dojot.   | string    | FILEMGMT_BUCKET_SUFFIX

#### Service State Manager

These parameters are passed directly to the SDK ServiceStateManager. Check the
[official repository](https://github.com/dojot/dojot-microservice-sdk-js) for more info on the
values.

| Key                                 | Default Value | Valid Values | Environment variable                          |
| ----------------------------------- | ------------- | ------------ | --------------------------------------------- |
| lightship.detect.kubernetes         | false         | boolean      | FILEMGMT_LIGHTSHIP_DETECT_KUBERNETES         |
| lightship.graceful.shutdown.timeout | 60000         | number       | FILEMGMT_LIGHTSHIP_GRACEFUL_SHUTDOWN_TIMEOUT |
| lightship.port                      | 9000          | number       | FILEMGMT_LIGHTSHIP_PORT                      |
| lightship.shutdown.delay            | 5000          | number       | FILEMGMT_SHUTDOWN_DELAY                      |
| lightship.shutdown.handler.timeout  | 5000          | number       | FILEMGMT_SHUTDOWN_HANDLER_TIMEOUT            |



