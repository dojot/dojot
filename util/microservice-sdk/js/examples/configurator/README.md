# Config Manager

A sample usage of Config Manager. To simplify the test, you can use the Docker Compose file provided
with the example.

You can add some environment variables in the Docker Compose file to add them to the configurations.
See the [Docker Compose file](./docker-compose.yml) for examples of environment variables.
There is also a [production.conf](./config/production.conf) file that you can add new configurations
or change existing ones.

To run the example:
```shell
docker-compose up
```

## Overview

This example utilizes VerneMQ to demonstrate the capabilities of the ConfigManager. The
[index.js](./index.js) file implements a simple MQTT client that connects to VerneMQ and publish
messages in it. The same messages are retrieved via subscription.
