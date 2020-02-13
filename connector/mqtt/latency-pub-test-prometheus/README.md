# Latency pub test prometheus

### Latency between publication MQTT and Kafka with Prometheus and Dojot

A [prometheus](https://github.com/prometheus/prometheus) client to measure the latency between publication MQTT and when payload arrives in Apache Kafka for dojot.


### **How works**

Prometheus collects metrics from monitored targets by scraping metrics HTTP endpoints on these targets. When Prometheus scrapes your instance's HTTP endpoint, the client exposes metrics between the last collector to the current.

Every time that this client receives a message from Kafka It store a latency, like describe below:

 - The latency is measured with the diffence between final timestamp and start timestamp.

 - The start timestamp in miliseconds must be within the  payload (publication MQTT) and data as follow: `{"attrs": {"timestamp": 1575978282524759 }}`

 - And the final timestamp in miliseconds is exposed by kafka in extra information.

When the Prometheus collects the metrics, It collects the metrics such as average, maximum, minimum and standard deviation between the last collector and the current one.

Metrics are following the Prometheus conventions. They are available via HTTP endpoint (http://127.0.0.1:3000/metrics), with default port 3000 and default path /metrics, [see more](https://prometheus.io/docs/instrumenting/clientlibs/).

## **Environment variables**

Key                      | Purpose                                                  | Default Value      | Valid Values |
------------------------ | -------------------------------------------------------- | ---------------    | -----------  |
AUTH_URL                 | Address of the auth service                              | http://auth:5000   | url          |
DATA_BROKER_URL          | Address of the data broker                               | http://data-broker | url          |
KAFKA_HOSTS              | Addresses of the kafka brokers separeted by a comma      | kafka-server:9092  | hostname/IP  |
LOG_LEVEL                | logger level                                             | info               | debug, error, warning, info  |
PROMETHEUS_PORT          | Port of prometheus client                                | 3000               | integer       |

## Exposed metrics

Metric                                                                |   Description
------                                                                | -----------
**dojot_latency_pub_statistics**                                          | Exposes metrics between the last collector to the current collector for Prometheus.
dojot_latency_pub_statistics {statistic_kind="maximum",unit="ms"}                   | Maximum value between these collections
dojot_latency_pub_statistics {statistic_kind="minimum",unit="ms"}                   | Minimum value between these collections
dojot_latency_pub_statistics {statistic_kind="average",unit="ms"}                   | Average value between these collections
dojot_latency_pub_statistics {statistic_kind="median",unit="ms"}                    | Median value between these collections
dojot_latency_pub_statistics {statistic_kind="standard_deviation",unit="ms"}        | Standard Deviation value between these collections

Example:

```
# HELP dojot_latency_pub_statistics Latency between publication MQTT and Kafka with Prometheus and Dojot
# TYPE dojot_latency_pub_statistics gauge
dojot_latency_pub_statistics{statistic_kind="maximum",unit="ms"} 1
dojot_latency_pub_statistics{statistic_kind="minimum",unit="ms"} 2
dojot_latency_pub_statistics{statistic_kind="average",unit="ms"} 3
dojot_latency_pub_statistics{statistic_kind="median",unit="ms"} 4
dojot_latency_pub_statistics{statistic_kind="standard_deviation",unit="ms"} 5
```

### Standard and runtime collectors

All Standard and runtime collectors have prefix *dojot_latency_pub*, [read more](https://prometheus.io/docs/instrumenting/writing_clientlibs/#standard-and-runtime-collectors) about standard and runtime collectors.

### Grafana

There is a dashboard configuration for use with this service in [dashboard](./examples/dashboard-grafana/Dojot _100k_V3.json).
