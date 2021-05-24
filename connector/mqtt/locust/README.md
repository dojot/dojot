# **Dojot Load Test**
The Dojot Load Test tool is an implementation using the open-source load testing tool Locust to generate traffic from/to Dojot. Tested in Dojot's development images.


# **How to use**

__Note__ You need to enable the `dev-test-cli` client in the keycloak. For security reasons it is disabled by default, after use it is recommended to disable it again.

## **Prerequisites**

If you don't have any certificates, you should first generate them. See the
Generate Certificates script [documentation](./src/scripts/README.md) for more info.

## **Docker-Compose**

**Remember to change the environment variables to your needs before continuing.**

In Locust root directory, bring up the master node:

```shell
docker-compose -f Docker/docker-compose-master.yml up
```

After the complete initialization of the master node, run the slave:

```shell
docker-compose -f Docker/docker-compose-slave.yml up
```

You can also run more than one slave at once:

```shell
docker-compose -f Docker/docker-compose-slave.yml up --scale locust-slave=10
```

We recommend running no more slaves than the number of cores in your hardware.

## **Dockerfile**

If you need to execute the containers individually, you can build the image by
yourself and then run it. This approach is not recommended.

Building the image:
```shell
sudo docker build -t locust-mqtt .
```

Starting the container:
```shell
sudo docker run -it -d -p 8089:8089 locust-mqtt
```

Running the Locust code:
```shell
sudo docker exec -it <CONTAINER_ID> /bin/bash -c "locust -f main.py Client"
```

## **Accessing the GUI**

After the initialization of the master, you can access the graphical interface by
typing the address to the server you are running the master followed by the Locust
port, e.g: `localhost:8089`.

## **Using the Grafana dashboard**

If you are using Ansible to install dojot, you can configure it to scrape for Locust metrics. To
accomplish this, you must provide the IP/hostname of the Locust master machine and the port 9646,
which is the default exporter port (you can change it in the
[Docker Compose file](./Docker/docker-compose-master.yml)). Check Ansible variables documentation
for the server-side configuration of Prometheus.

After the configuration is done, you can access the Grafana dashboard built for Locust integration.
It already comes embedded in the Ansible installation.

# **How it works**

Locust works with a master/slave architecture, as you can see in the diagram.

- The Master node is responsible for gathering and showing data from slaves using the
graphical interface
- The Slave node is responsible for making the communication with the server, sending
and receiving messages from/to dojot

<p align="center">
  <img src="./docs/diagrams/Locust.png">
</p>

The main idea behind Locust is to call **Locust tasks** from time to time to execute tests.
These tasks are run in the slaves only. In our case, we have only one task, responsible for
publishing MQTT messages with QoS 1 to the IoTAgent every 30s.

Aside from publishing, we receive messages too. Every time one client is created by Locust and
successfully connects to the server, it will subscribe to the config topic. The config topic
receives messages from Dojot to actuate in the device, so it is emulating an actuation in the
device - in our case, the device is a Locust client.

The used topics are:
- **Publish**: tenant:deviceid/attrs
- **Subscribe**: tenant:deviceid/config

## **Certificate revocation and renovation**

It is possible to simulate the revocation and renovation of certificates using this tool.
Be aware that in the beginning the system may not achieve the maximum connection RPS,
due to the certificates' files being written in the system.
To accomplish this, take a look at the following environment variables:

- RENEW_DEVICES and REVOKE_DEVICES to enable the simulations
- TIME_TO_RENEW and TIME_TO_REVOKE to control the time of renovation and revocation

## **Locust messages**

There are several messages that are displayed in Locust interface. These messages
are:
- connect: the client sent a connect message to the broker
- disconnect: the client were disconnected from the broker for some reason
- publish: the client published to the broker
- subscribe: the client subscribed to a topic in the broker
- recv_message: the client received a message from the subscribed topic
- renew: a certificate has been renovated
- revoke: a certificate has been revoked


# **Configuration**

All the commands in this guide are meant to be executed in the `locust` directory, a.k.a the
directory this README is on, unless otherwise told.

## **Environment Variables**

Before running any tests using this tool, make sure you configure the environment variables to match your needs.

When using Docker, you should pass the variables in the Dockerfile for the component you are running.
The Dockerfile for the Locust master and slave are in the `Docker` directory.

### **Locust**

Locust behaviour and Redis configurations.

Key                          | Purpose                                                        | Default Value | Valid Values                                  |
---------------------------- | -------------------------------------------------------------- | ------------- | --------------------------------------------- |
CA_CERT_FILE                 | CA certificate file                                            | ca.crt        | file name                                     |
CERT_DIR                     | certificates and private keys directory                        | cert/         | directory name                                |
DEBUG_MODE                   | activate debug mode in shell scripts                           | 0             | 0, 1                                          |
DEVICES_TO_RENEW             | number of devices to renew randomly                            | 1000          | integer                                       |
DEVICES_TO_REVOKE            | number of devices to revoke randomly                           | 1000          | integer                                       |
LOCUST_MASTER_HOST           | Locust master IP/hostname                                      | locust-master | hostname/IP                                   |
LOG_LEVEL                    | log level (case insensitive)                                   | info          | notset, debug, info, warning, error, critical |
MAX_TIME_RECONN              | max time (in seconds) to try to reconnect to the MQTT broker   | 600           | integer                                       |
MIN_TIME_RECONN              | min time (in seconds) to try to reconnect to the MQTT broker   | 1             | integer                                       |
PROBABILITY_TO_RENEW         | probability to renew a device                                  | 10            | integer in [0, 100]                           |
PROBABILITY_TO_REVOKE        | probability to revoke a device                                 | 10            | integer in [0, 100]                           |
REDIS_BACKUP                 | use a Redis dump with IDs instead of generating new ones       | y             | y, n                                          |
REDIS_CERTIFICATES_DB        | database with the certificates                                 | 0             | integer in [0, 15]                            |
REDIS_CONN_TIMEOUT           | redis timeout                                                  | 180           | integer                                       |
REDIS_HOST                   | redis host                                                     | redis         | hostname/IP                                   |
REDIS_MAPPED_DB              | database with the mapped device IDs from certificates database | 1             | integer in [0, 15]                            |
REDIS_PASSWD                 | redis password                                                 | none          | passwords                                     |
REDIS_PORT                   | redis port                                                     | 6379          | port value                                    |
REDIS_STORED_JWT_EXPIRE_TIME | time (in seconds) to expire the cached JWT                     | 1800          | positive integer                              |
RENEW_CERT_DIR               | directory where the certs to be renewed will be stored         | renew/        | directory name                                |
RENEW_DEVICES                | enable random renovation of devices (case insensitive)         | False         | True, False                                   |
REVOKE_CERT_DIR              | directory where the certs to be revoked will be stored         | revoke/       | directory name                                |
REVOKE_DEVICES               | enable random revocation of devices (case insensitive)         | False         | True, False                                   |
TASK_MAX_TIME                | max time of each Locust's tasks (ms)                           | 30000         | integer                                       |
TASK_MIN_TIME                | min time of each Locust's tasks (ms)                           | 29500         | integer                                       |
TENANT                       | tenant that is publishing                                      | admin         | string                                        |
TIME_TO_RENEW                | time to renew the cert after the client initialization         | 1000          | integer                                       |
TIME_TO_REVOKE               | time to revoke the cert after the client initialization        | 1000          | integer                                       |

### **MQTT**

Configurations related to MQTT communication.

Key                     | Purpose                    | Default Value | Valid Values      |
----------------------- | -------------------------- | ------------- | ----------------- |
DOJOT_MQTT_HOST         | MQTT broker host           | 127.0.0.1     | hostname/IP       |
DOJOT_MQTT_PORT         | MQTT broker port           | 1883          | port value        |
DOJOT_MQTT_QOS          | MQTT broker QoS level      | 1             | 0, 1, 2           |
DOJOT_MQTT_TIMEOUT      | MQTT broker timeout        | 60            | integer           |

### **Dojot**

Dojot integration configuration.

Key                     | Purpose                              | Default Value         | Valid Values       |
----------------------- | ------------------------------------ | --------------------- | ------------------ |
DOJOT_API_RETRIES       | number of retries for API calls      | 3                     | integer            |
DOJOT_API_RETRY_TIME    | time to wait between retries         | 5000                  | float time in ms   |
DOJOT_DEVICES_PAGE_SIZE | /device endpoint page size           | 20                    | positive integers  |
DOJOT_ENV               | use a dojot instance                 | n                     | y, n               |
DOJOT_GATEWAY_TIMEOUT   | dojot auth API timeout               | 180                   | integer            |
DOJOT_PASSWD            | dojot user's password                | admin                 | passwords          |
DOJOT_URL               | dojot instance address               | http://127.0.0.1:8000 | hostname/IP        |
DOJOT_USER              | dojot user                           | admin                 | usernames          |

## **Operating System**

While small tests can be run without problems, bigger ones create some obstacles.
To create a lot of clients in only one machine, the default number of ports in the
OS will not accommodate the required number of connections. To increase it, run:

```shell
sudo sysctl -w net/ipv4/ip_local_port_range="1024 65535"
```

# Development

You can use the Docker Compose provided with the `generate_certs` script to develop. In case you
missed it, you can see it [here](#generate-certificates).

## Lint

There is a lint in the repository that you should follow. To check the linting state in it:
```shell
run_lint
```

If you are not running inside the generate_certs Docker Compose, run:
```shell
pylint src --rcfile=.pylintrc tests --rcfile=.pylintrc
```

## Unit Tests

You can get the complete tests report by running:
```shell
run_cov
```

If you are not running inside the generate_certs Docker Compose, run:
```shell
coverage run -m pytest tests && coverage html
```

It will generate an HTML coverage report in the `htmlcov` directory.

# **Issues and help**

If you found a problem or need help, leave an issue in the main [Dojot repository](https://github.com/dojot/dojot) and we will help you!

# **References**
[Material to understand paho-mqtt threads](http://www.steves-internet-guide.com/loop-python-mqtt-client/)

[Locust]( https://locust.io/)

[Paho-MQTT Python Library](https://pypi.org/project/paho-mqtt/)
