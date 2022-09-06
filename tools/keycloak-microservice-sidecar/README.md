# Keycloak Microservice Sidecar

The purpose of this component is to provide an integration with keycloak to any service who use REST as interface.

## Table of Contents

- [Keycloak Microservice Sidecar](#keycloak-microservice-sidecar)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Keycloak configuration](#keycloak-configuration)
  - [Running the service](#running-the-service)
    - [Configurations](#configurations)
      - [Keycloak settings](#keycloak-settings)
      - [Primary app settings](#primary-app-settings)
      - [Server settings](#server-settings)
      - [Express Framework settings](#express-framework-settings)
      - [Logger Settings](#logger-settings)
    - [How to run](#how-to-run)
  - [Debugging the service](#debugging-the-service)
  - [Testing the service](#testing-the-service)
  - [Issues and help](#issues-and-help)

## Overview

[Keycloak](https://www.keycloak.org/) is an Open Source Identity and Access Management. Some microservices in Dojot can't use the [microsercice-sdk](https://github.com/dojot/dojot-microservice-sdk-js), who have the integration with keycloak because the microservice-sdk is only supported by Node.js.

So, this component make the bridge with the microservice-sdk integrating keycloak and redirect all the api calls to the primary app. 

It is important that you see the [microsercice-sdk] for custom settings for the dojot platform.

## Keycloak configuration

For correct use of this sidecar its necessary to configure Keycloak.

This service need a client in Keycloak configued. You can pass the configuration of this client as enviroments variables in this sidecar.

## Running the service

### Configurations

Before running the **keycloak-microservice-sidecar** service within your environment, make
sure you configure the environment variables to match your needs.

You can select the configuration file via the `MICROSERVICEKEYCLOAKSIDECAR_USER_CONFIG_FILE`

variable. Its default value is `production.conf` . Check the
[config directory](./src/config) for the user configurations that are
available by default.

For more information about the usage of the configuration files and environment
variables, check the __ConfigManager__ module in our
[Microservice SDK](https://github.com/dojot/dojot-microservice-sdk-js).
You can also check the
[ConfigManager environment variables documentation](https://github.com/dojot/dojot-microservice-sdk-js/blob/master/lib/configManager/README.md#environment-variables)
for more details.

In short, all the parameters in the next sections are mapped to environment
variables that begin with `MICROSERVICEKEYCLOAKSIDECAR_` . You can either use environment
variables or configuration files to change their values.
You can also create new parameters via environment variables by following the
fore mentioned convention.

#### Keycloak settings

| Key                               | type    | Default Value                               | Valid Values | Environment variable                      | Purpose                                                                                                                                         |
| --------------------------------- | ------- | --------------------------------------------| ------------ | ----------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| keycloak.url                      | string  | `http://keycloak:8080`                      |              | MICROSERVICEKEYCLOAKSIDECAR_KEYCLOAK_URL           | Keycloak URL    |
| keycloak.client.id                | string  | `microservice-keycloadk-sidecar`            |              | MICROSERVICEKEYCLOAKSIDECAR_KEYCLOAK_CLIENT_ID     | Name of keycloak client id in keycloak for microservice-keycloadk-sidecar  |
| keycloak.client.secret            | string  |                                             |              | MICROSERVICEKEYCLOAKSIDECAR_KEYCLOAK_CLIENT_SECRET | Secret of the client microservice-keycloak-sidecar on keycloak |
| keycloak.tentants.url             | string  | `http://keycloak-proxy:8081/api/v1/tenant`  |              | MICROSERVICEKEYCLOAKSIDECAR_KEYCLOAK_TENANTS_URL   | URL of the microservice keycloack-proxy |
| keycloak.access.key               | string  |                                             |              | MICROSERVICEKEYCLOAKSIDECAR_KEYCLOAK_ACCESS_KEY    | Access key to access keycloak |
| keycloak.secret.key               | string  |                                             |              | MICROSERVICEKEYCLOAKSIDECAR_KEYCLOAK_SECRET_KEY    | Secret key to access keycloak |


#### Primary app settings
| Key                               | type    | Default Value          | Valid Values    | Environment variable                         | Purpose                                                                                                                                         |
| --------------------------------- | ------- | ---------------------- | --------------- | -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| primaryapp.url                    | string   |                       |                 | MICROSERVICEKEYCLOAKSIDECAR_PRIMARYAPP_URL               | URl for the primary app             |
| primaryapp.path                   | string[] |                       |                 | MICROSERVICEKEYCLOAKSIDECAR_PRIMARYAPP_PATH              | List of route paths in primary app             |
| primaryapp.healthcheck.port       | integer | `9000`                 | from 0 to 65535 | MICROSERVICEKEYCLOAKSIDECAR_PRIMARYAPP_HEALTHCHECK_PORT  | port for Health Check or primary app.                                                                                                                  |


#### Server settings

| server.shutdown.delay             | integer | `10000`                | positive values | MICROSERVICEKEYCLOAKSIDECAR_SERVER_SHUTDOWN_DELAY             | Delay (in milliseconds) until the shutdown routine starts.                                                                                      |
| server.shutdown.gracefultimeoutms | integer | `60000`                | positive values | MICROSERVICEKEYCLOAKSIDECAR_SERVER_SHUTDOWN_GRACEFULTIMEOUTMS | Number of milliseconds to wait until the process finishes normally after receiving a shutdown signal, after which the process is forced to end. |
| server.shutdown.handlertimeoutms  | integer | `5000`                 | positive values | MICROSERVICEKEYCLOAKSIDECAR_SERVER_SHUTDOWN_HANDLERTIMEOUTMS  | Number of milliseconds waiting for execution of the shutdown handlers, if this value is exceeded, the process is forced to end.                 |

#### Express Framework settings

| Key                         | type    | Default Value                                                                                                                     | Valid Values                                                  | Environment variable                   | Purpose                                                                                                                                                                                                                                                                                                               |
| --------------------------- | ------- | --------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- | -------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| framework.trustproxy        | boolean | `true`                                                                                                                            | `true` or `false`                                             | MICROSERVICEKEYCLOAKSIDECAR_FRAMEWORK_TRUSTPROXY        | By enabling the trust proxy feature, the client IP address will be updated in the correct places with the forwarded IP. If the service is running behind a reverse proxy, it is necessary to configure the reverse proxy to forward the client's real IP address (http://expressjs.com/en/guide/behind-proxies.html). |                             |


#### Logger Settings

| Key                  | type    | Default Value                         | Valid Values                         | Environment variable            | Purpose                                                                                                                                                                                                               |
| -------------------- | ------- | ------------------------------------- | ------------------------------------ | ------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| logger.verbose       | boolean | `false`                               | `true` or `false`                    | MICROSERVICEKEYCLOAKSIDECAR_LOGGER_VERBOSE       | In verbose mode, file and line of where the logging method has been called are added as metadata to the logging message.                                                                                              |
| logger.console.level | string  | `info`                                | `error` , `warn` , `info` or `debug` | MICROSERVICEKEYCLOAKSIDECAR_LOGGER_CONSOLE_LEVEL | logging level.                                                                                                                                                                                                        |
| logger.file.enable   | boolean | `false`                               | `true` or `false`                    | MICROSERVICEKEYCLOAKSIDECAR_LOGGER_FILE_ENABLE   | Flag that enables logging to file.                                                                                                                                                                                    |
| logger.file.level    | string  | `info`                                | `error` , `warn` , `info` or `debug` | MICROSERVICEKEYCLOAKSIDECAR_LOGGER_FILE_LEVEL    | logging level.                                                                                                                                                                                                        |
| logger.file.dir      | string  | `./temp/log/`                         |                                      | MICROSERVICEKEYCLOAKSIDECAR_LOGGER_FILE_DIR      | Directory where the log files will be saved.                                                                                                                                                                          |
| logger.file.name     | string  | `dojot.keycloak-sidecar-%DATE%.log` |                                      | MICROSERVICEKEYCLOAKSIDECAR_LOGGER_FILE_NAME     | Log file name pattern.                                                                                                                                                                                                |
| logger.file.max      | string  | `7d`                                  |                                      | MICROSERVICEKEYCLOAKSIDECAR_LOGGER_FILE_MAX      | Maximum number of logs to keep. If not set, no logs will be removed. This can be a number of files or number of days. If using days, add 'd' as the suffix.                                                           |
| logger.file.size     | string  | `10m`                                 |                                      | MICROSERVICEKEYCLOAKSIDECAR_LOGGER_FILE_SIZE     | Maximum size of the file after which it will rotate. This can be a number of bytes, or units of kb, mb, and gb. If using the units, add 'k', 'm', or 'g' as the suffix. The units need to directly follow the number. |


### How to run

Beforehand, you need an already running dojot instance in your machine. Check
out the [dojot documentation](https://dojotdocs.readthedocs.io) for more
information on installation methods.

Generate the Docker image:

~~~shell
docker build -t <username>/keycloak-sidecar:<tag> -f  .
~~~

Then an image tagged as `<username>/keycloak-sidecar:<tag>` will be made
available. You can send it to your DockerHub registry to made it available for
non-local dojot installations:

~~~shell
docker push <username>/keycloak-sidecar:<tag>
~~~

## Debugging the service

To debug the service in a development environment, we prefer to use the VS Code, 
and for that there is the [.vscode/launch.json](./js/.vscode/launch.json) file
that can be studied and there you will have tips on what you need to be present
in your local environment to run the service outside the container.

See also the [default.conf](./js/config/default.conf) file used by the service, 
look for URLs there and you will have a good tip of what needs to be
externalized from the _docker-compose_ environment for the service to have
access. The [development.conf](./js/config/development.conf) file takes
precedence over the [default.conf](./js/config/default.conf) file when we run
the _service_ in _debug mode_ using VS Code but it is completely discarded in a
_production_ environment.

Basically, you must attend to the _version_ of Node.js and have installed the
_build-essential_ library (in the case of Linux distributions based on Debian).
It is likely that in order to compile the `node-rdkafka` _node_module_ it will
be necessary to install the _gzip compression_ library. In this case, the most
advisable is to study the dependencies declared in the [Dockerfile](./Dockerfile)
file to have an idea of what is needed.

You may need the following dependencies installed on your Linux:
~~~shell
$ # in the case of Linux distributions based on Debian:
$ sudo apt-get install -y \

               build-essential \
               node-gyp \
               make \
               ca-certificates \
               gzip

~~~

For the _keycloak-sidecar_ service to be able to access the other services on
the dojot platform, it is necessary to map the service container _ports_ to
ports on your _localhost_ (based on a _docker-compose_ deployment).
See the [development.conf](./js/config/development.conf) file for dependent
services.

In order for the service (running locally) to be able to connect to Kafka
(inside the docker-compose), in addition to externalizing the Kafka container
port to your localhost, it is also necessary to edit the `/etc/hosts` file on
your Linux:
~~~shell
$ sudo vi /etc/hosts
~~~

And include the following entry:

~~~
127.0.0.1 kafka
~~~

This way, your local DNS will know how to correctly resolve the domain for the
_kafka_ service (remember that it is necessary to externalize the Kafka
container port to your localhost).

## Testing the service

To perform unit tests, just open a terminal in the project's root directory
(the same directory as the `package.json` file) and execute the following
command:

~~~shell
$ npm test
~~~

Unit tests will be performed and a test coverage report will be generated in
the `./coverage` directory.
To view the test coverage report, simply open the
`./coverage/lcov-report/index.html` file in a browser.

To debug the unit tests, run the command:

~~~shell
$ npm run debugtest
~~~

From there, you can use the VS Code to attach to the running process and debug
the test cases. Learn more at [Jest Troubleshooting](https://jestjs.io/docs/troubleshooting#debugging-in-vs-code).

## Issues and help

If you found a problem or need help, leave an issue in the main
[dojot repository](https://github.com/dojot/dojot) and we will help you!
