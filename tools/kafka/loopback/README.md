# 100K Loopback

This service is a helper for 100k-epic, transfers incoming payload from a **device-data** topic to **device-manager** topic in the dojot style.

# Configurations
## Environment Variables

Key                      | Purpose                                                             | Default Value   			| Valid Values      |
------------------------ | ------------------------------------------------------------------- | -------------------------- | ----------------- |
DOJOT_USERNAME           | username to login on auth and retrieve token						   | admin           			| string   		    |
DOJOT_PASSWORD           | password to login on auth and retrieve token						   | admin           			| string   		    |
AUTH_ADDRESS             | Address of the auth service                                         | http://auth:5000           | hostname/IP:port  |
DATA_BROKER_ADDRESS      | Address of the data broker service                                  | http://data-broker:80    	| hostname/IP:port  |
KAFKA_BROKER_LIST        | Addresses of the kafka brokers separated by a comma                 | kafka-server:9092			| hostname/IP:port  |
LOOPBACK_CONSUMER_GROUP  | Kafka consumer group                                                | loopback-group             | string            |
DEVICE_DATA_TOPIC    	 | Topic to consume from                                               | device-data        		| string            |
DEVICE_MANAGER_TOPIC     | Topic to produce the modified messages                              | dojot.device-manager.device| string            |

# Example
As a specific component for dojot device there is an example

Device data received message
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

Device manager modified payload
```
{
    "metadata": {
        "deviceid": "5908d1b9787f4dc9a5bdf94f5122b37e",
        "tenant":"admin",
        "timestamp":1583955993 
    },
    "attrs": {
        "timestamp": 1583955993805
    }
}
```

More information can be find [here](https://dojotdocs.readthedocs.io/projects/DeviceManager/en/latest/kafka-messages.html).