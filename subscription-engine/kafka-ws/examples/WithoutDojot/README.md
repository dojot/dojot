# Kafka WS example application - without Dojot

Kafka WS can be used in any application, not necessarily in a Dojot environment. This example shows
the component being used in this kind of scenario.

## Usage

It is provided a Docker Compose file with the whole environment to see the Kafka-ws in action,
including a service that publishes messages to kafka every 20 seconds for two diferents topics
(`tenant1.ws.example.test` and `tenant2.ws.example.test`) and two websocket clients.

To run this example, simply type:

```shell
docker-compose up
```
