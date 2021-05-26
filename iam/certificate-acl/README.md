# certificate-acl

The **certificate-acl** is responsible for keeping in memory an association between certificates' fingerprint and their owners so that dojot services that needs this information can query it instead of x509-identity-mgmt service, which keeps this information only on disk.

It's worth to say that the association between a device and a fingerprint is kept by the x509-identity-mgmt, but the association between a service and a fingerprint is not. This latter information in only available at the **certificate-acl** service.

The **certificate-acl** service listen on to certificate events at Apache Kafka and builds the relationships between fingerprints and their owners (devices or services) at a Redis database. For each query, the **certificate-acl** service tries to get the information from the Redis firstly, which is faster; and only on case of failure it goes to the x509-identity-mgmt service.

The **certificate-acl** service provides an internal endpoint that must be used only by dojot services to query the owner of a given certificate. To do this, a service makes a **GET** request to the endpoint **internal/api/v1/acl-entries/:fingerprint** passing the fingerprint of the certificate. In case of success, it will be returned an **HTTP code 200** and a json with the corresponding owner.

Example of an HTTP request:
~~~HTTP
GET internal/api/v1/acl-entries/9A:83:29:BD:FA:95:B2:E2:25:5F:AA:27:44:27:B8:11:17:B8:C2:1E:EE:06:85:4C:C8:C8:FD:F9:C2:9A:9A:34
~~~

Example of an HTTP response:
~~~HTTP
HTTP/1.1 200 OK
Content-type: application/json

"v2k-bridge"
~~~

## Running the service

### Configurations

Before running the **certificate-acl** service within your environment, make
sure you configure the environment variables to meet your needs.

You can choose the configuration file via the `CERTIFICATE_ACL_USER_CONFIG_FILE`
variable. Its default value is `production.conf`. Check the
[config directory](./config) for the user configurations that are
available by default.

For more information about the usage of the configuration files and environment
variables, check the __ConfigManager__ module in our
[Microservice SDK](https://github.com/dojot/dojot-microservice-sdk-js).
You can also check the
[ConfigManager environment variables documentation](https://github.com/dojot/dojot-microservice-sdk-js/blob/master/lib/configManager/README.md#environment-variables)
for more details.

In short, all the parameters in the next sections are mapped to environment
variables that begin with `CERTIFICATE_ACL_`. You can either use environment
variables or configuration files to change their values.
You can also create new parameters via environment variables by following the
fore mentioned convention.

#### Application settings

 Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| app.kafka.consumer.topic.regex | string | `^.*dojot\.x509-identity-mgmt\.certificates` | | CERTIFICATE_ACL_APP_KAFKA_CONSUMER_TOPIC_REGEX | Regular express to the kafka' topics to be consumed by the service. |

#### Health-check settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| healthcheck.kafka.interval.ms | integer | `5000` | `[0, ...]` | CERTIFICATE_ACL_HEALTHCHECK_KAFKA_INTERVAL_MS | Interval in milliseconds to check the health of the communication between the service and the Kafka broker. |

#### Server settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| server.host | string | `0.0.0.0` | | CERTIFICATE_ACL_SERVER_HOST | IP of the network interface on which the http server listens on. The value `0.0.0.0` means listening on every available interface. |
| server.port | integer | `3000` | | CERTIFICATE_ACL_SERVER_PORT | Port on which the application listens for http requests. |

#### Logger settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| logger.verbose | boolean | `false` | `true` or `false` | CERTIFICATE_ACL_LOGGER_VERBOSE | In verbose mode, file and line of where the logging method has been called are added as metadata to the logging message. |
| logger.console.level | string | `info` | `error`, `warn`, `info` or `debug` | CERTIFICATE_ACL_LOGGER_CONSOLE_LEVEL | logging level. |
| logger.file.enable | boolean | `false` | `true` or `false` | CERTIFICATE_ACL_LOGGER_FILE_ENABLE | Flag that enables logging to file. |
| logger.file.level | string | `info` | `error`, `warn`, `info` or `debug` | CERTIFICATE_ACL_LOGGER_FILE_LEVEL | logging level. |
| logger.file.dir | string | `./temp/log/` | | CERTIFICATE_ACL_LOGGER_FILE_DIR | Directory where the log files will be saved. |
| logger.file.name | string | `dojot.certificate-acl-%DATE%.log` | | CERTIFICATE_ACL_LOGGER_FILE_NAME | Log file name pattern. |
| logger.file.max | string | `7d` | | CERTIFICATE_ACL_LOGGER_FILE_MAX | Maximum number of logs to keep. If not set, no logs will be removed. This can be a number of files or number of days. If using days, add 'd' as the suffix. |
| logger.file.size | string | `10m` | | CERTIFICATE_ACL_LOGGER_FILE_SIZE | Maximum size of the file after which it will rotate. This can be a number of bytes, or units of kb, mb, and gb. If using the units, add 'k', 'm', or 'g' as the suffix. The units need to directly follow the number. |

#### Kafka integration settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| enable.async.commit| boolean | `true` | `true` or `false` | CERTIFICATE_ACL_ENABLE_ASYNC_COMMIT | Enable asynchronous processing mode. |
| commit.on.failure| boolean | `false` | `true` or `false` | CERTIFICATE_ACL_ENABLE_ASYNC_COMMIT | Enable to commit even if the processing callback has failed. |
| kafka.topic.auto.offset.reset | string | `earliest` | `latest`, `earliest`, `none` | CERTIFICATE_ACL_KAFKA_TOPIC_AUTO_OFFSET_RESET | What to do when there is no initial offset in Kafka or if the current offset does not exist any more on the server. See more [here](https://kafka.apache.org/documentation/#consumerconfigs_auto.offset.reset). |
| kafka.consumer.group.id | string | `certificate-acl` | | CERTIFICATE_ACL_KAFKA_CONSUMER_GROUP_ID | A unique string that identifies the consumer group this consumer belongs to. See more [here](https://kafka.apache.org/documentation/#consumerconfigs_group.id). |
| kafka.consumer.metadata.broker.list | string | `kafka:9092` | - | CERTIFICATE_ACL_KAFKA_CONSUMER_METADATA_BROKER_LIST | A comma-separated list containing the kafka's `host:port`. |

#### Redis integration settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
|redis.host | string | `acl-redis` | | CERTIFICATE_ACL_REDIS_HOST | Host for the Redis database. |
|redis.port | integer | 6379 | | CERTIFICATE_ACL_REDIS_PORT | Port for the Redis database. |
|redis.db | integer | 0 | [0-15] | CERTIFICATE_ACL_REDIS_DB | Redis database to be used. |
|redis.reconnect.after.ms | integer | `5000` | `[0,...]` | CERTIFICATE_ACL_REDIS_RECONNECT_AFTER_MS | Time in milliseconds to wait before reconnecting to the redis database.  |
| redis.operation.timeout.ms | integer | `1000` | `[0,...]` | CERTIFICATE_ACL_REDIS_OPERATION_TIMEOUT_MS | Timeout in milliseconds for an operation in the redis database. |

#### X509-identity-mgmt integration settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
|service.hostname | string | `x509-identity-mgmt` | | CERTIFICATE_ACL_SERVICE_HOSTNAME | Hostname for the x509-identity-mgmt service. |
|service.port | integer | `3000` |  | CERTIFICATE_ACL_SERVICE_PORT | Port for the x509-identity-mgmt service. |
|service.path | integer | `/internal/api/v1/certificates/` | `` | CERTIFICATE_ACL_SERVICE_PATH | Path to the certificate collection. |
|service.timeout | integer | `3000` | `[0,...]` | CERTIFICATE_ACL_SERVICE_TIMEOUT |Timeout of the request to the x509-identity-mgmt service in milliseconds. |
