# Loopback service for 100K epic

This service is a helper for testing the 100k-epic. It's goal is to transform messages sent from devices to dojot into messages sent from dojot to devices. To implement this "loopback", it transfers incoming messages from **device-data** topic to **device-manager** topic adapting them according to dojot's message schema.

# Configurations
## Environment Variables

Key                      | Purpose                                                             | Default Value   			| Valid Values      |
------------------------ | ------------------------------------------------------------------- | -------------------------- | ----------------- |
DOJOT_USERNAME           | username to access dojot                 						   | admin           			| string   		    |
DOJOT_PASSWORD           | password to access dojot                 						   | admin           			| string   		    |
AUTH_ADDRESS             | Address of the auth service                                         | http://auth:5000           | hostname/IP:port  |
DATA_BROKER_ADDRESS      | Address of the data broker service                                  | http://data-broker:80    	| hostname/IP:port  |
KAFKA_BROKER_LIST        | Addresses of the kafka brokers separated by a comma                 | kafka-server:9092			| hostname/IP:port  |
LOOPBACK_CONSUMER_GROUP  | Kafka consumer group                                                | loopback-group             | string            |
DEVICE_DATA_TOPIC    	 | Topic to consume from                                               | device-data        		| string            |
DEVICE_MANAGER_TOPIC     | Topic to produce to                                                 | dojot.device-manager.device| string            |

# Example
Bellow there is an example of the message received from the **device-data** topic and the modified message sent to **device-manager** topic.

Received message from device-data:
```
{
    "metadata": { 
        "deviceid":"e33dada7ce5a4905819d8fb0606c613f",
        "tenant":"admin",
        "timestamp":1583939224
    },
    "attrs": {
        "timestamp": 1583939224072
    }
}
```

Sent message to device-manager:
```
{
  "event": "configure",
  "meta": {
    "service": "admin",
    "timestamp": 1583939224
  },
  "data": {
    "id": "e33dada7ce5a4905819d8fb0606c613f",
    "attrs": {
        "timestamp": 1583939224072
    }
  }
}
```

More information can be find [here](https://dojotdocs.readthedocs.io/projects/DeviceManager/en/latest/kafka-messages.html).