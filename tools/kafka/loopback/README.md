# Loopback service for Kafka

This service's goal is to simulate actuations in dojot by forwarding messages in the `device-data`
topic (publishing topic) to the `device-manager.device` topic (actuation topic), adapting its format
to be dojot-compliant.

__Note__ You need to enable the `dev-test-cli` client in the keycloak. For security reasons it is disabled by default, after use it is recommended to disable it again.

# Configurations

## Environment Variables

Key                     | Purpose                               | Default Value               | Valid Values     |
----------------------- | ------------------------------------- | --------------------------- | ---------------- |
KEYCLOAK_ADDRESS            | Address of the Auth service           | http://keycloak:8080            | hostname/IP:port |
DATA_BROKER_ADDRESS     | Address of the Data Broker service    | http://data-broker:80       | hostname/IP:port |
DEVICE_DATA_TOPIC       | Topic to be consumed                  | device-data                 | string           |
DEVICE_MANAGER_TOPIC    | Topic to produce to                   | dojot.device-manager.device | string           |
DOJOT_TENANT          | dojot's  tenant                 | admin                       | string           |
DOJOT_PASSWORD          | dojot's user password                 | admin                       | string           |
DOJOT_USERNAME          | dojot's user name                     | admin                       | string           |
KAFKA_BROKER_LIST       | Comma-separated list of Kafka brokers | kafka-server:9092           | hostname/IP:port |
LOOPBACK_CONSUMER_GROUP | Kafka consumer group to be used       | loopback-group              | string           |

# Example

After configuring the Loopback service to work in your environment, we can send messages to dojot.

__NOTE THAT__ in this example we are using MQTT, but the protocol is not relevant; you are good to
go as long as you can succesfully send messages to dojot with your preferred protocol.

Before proceeding, we need to subscribe to the actuation topic to be able to receive the messages.
In the terminal, run:

```shell
mosquitto_sub -h <dojot_host> -p 1883 -t admin:123abc/config -u admin:123abc
```

Now open a new terminal and send the message:

```shell
mosquitto_pub -h <dojot_host> -p 1883 -t admin:123abc/attrs -m '{"timestamp": 1583939224072}' -u admin:123abc
```

In the first terminal you should see the same message you sent arriving:

```json
{ "timestamp": 1583939224072 }
```

## Internal Behaviour

When you sent the message with `mosquitto_pub`, the Loopback service received the following message
from `admin.device-data` topic:

```json
{
  "metadata": {
    "deviceid": "123abc",
    "tenant": "admin",
    "timestamp": 1583939224
  },
  "attrs": {
    "timestamp": 1583939224072
  }
}
```

After receiving the above message, the Loopback service transforms it to create the following
actuation message, that is sent to the `admin.device-manager.device` topic:

```json
{
  "event": "configure",
  "meta": {
    "service": "admin",
    "timestamp": 1583939224
  },
  "data": {
    "id": "123abc",
    "attrs": {
      "timestamp": 1583939224072
    }
  }
}
```

More information on the message's format can be found
[here](https://dojotdocs.readthedocs.io/projects/DeviceManager/en/latest/kafka-messages.html).
