# Latency pub test prometheus

### Latency between publication MQTT and Kafka with Prometheus and Dojot

A [prometheus](https://github.com/prometheus/prometheus) client to measure the latency between publication MQTT and when payload arrives in Apache Kafka for dojot.

### **How works**

Latency is exposed at *http(s)://ipcontainer:PROMETHEUS_PORT/metrics* to Prometheus.

The latency is measured with the diffence between final timestamp and start timestamp.

The start timestamp in seconds must be within the  payload (publication MQTT) and data as follow:

`{"attrs": {"timestamp": 1575978282.524759 }}`

And the final timestamp in miliseconds is exposed by kafka in extra information.


## **Environment variables**

Key                      | Purpose                                                             | Default Value   | Valid Values   |
------------------------ | ------------------------------------------------------------------- | --------------- | -------------- |
AUTH_URL                 | Address of the auth service                                         | http://auth:5000| url    |
DATA_BROKER_URL              | Address of the data broker                                          | http://data-broker  | url    |
KAFKA_HOSTS              | Address of the kafka broker                                         |kafka-server:9092| hostname/IP    |
LOG_LEVEL                 | logger level                                      | info | debug, error, warning, info   |
PROMETHEUS_PORT         | Port of prometheus client                                             | 3000              | integer        |

### Grafana

There is a dashboard configuration for use with this service in [dashboard](./examples/dashboard-grafana/Dojot100kV2.json).

