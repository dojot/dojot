# Basic Producer

This example implements a [basic producer](sample.js) that publishes one message.

It is also provided a [Docker Compose file](docker-compose.yml) with the whole environment to see the producer in action, including a service which consumes the messages as the [basic producer](sample.js) is restarted by docker-compose.

To run this example, type:

```sh
docker-compose up
```