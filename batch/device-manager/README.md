# Device Manager Batch

[![License badge](https://img.shields.io/badge/License-Apache%202.0-blue.svg)]
[![Docker badge](https://img.shields.io/docker/pulls/dojot/iotagent-json.svg)](https://hub.docker.com/r/dojot/device-manager/)

The Device Manager Batch handles all CRUD operations related to devices in dojot.

## How does it work

The Device Manager Batch stores and retrieves information models for devices and templates and a few
static information about them as well. Whenever a device is created, removed or just edited, it will
publish a message through Kafka.

## Dependencies

### Dojot services

The minimal set of dojot services needed to run Device Manager is:

- Kafka
- PostgreSQL
- Keycloak proxy

### Development

- NodeJs (https://nodejs.org/en/)
- Prisma (https://www.prisma.io/)
- Dojot sdk (https://github.com/dojot/dojot-microservice-sdk-js)
- Keycloak (https://www.keycloak.org/)

The minimal set of dojot services needed to run Device Manager Batcj is:

- Kafka
- PostgreSQL
- Keycloak proxy

## Configuration

| Key            | Purpose                       | Default Value       | Accepted values             |
| -------------- | ----------------------------- | ------------------- | --------------------------- |
| BROKER         | Kafka topic subject manager   | http://data-broker  | Hostname                    |
| CREATE_DB      | Option to create the database | True                | Boolean                     |
| DBDRIVER       | PostgreSQL database driver    | postgresql+psycopg2 | String                      |
| DBHOST         | PostgreSQL database host      | postgres            | String                      |
| DBNAME         | PostgreSQL database name      | dojot_devm          | String                      |
| DBPASS         | PostgreSQL database password  | none                | String                      |
| DBUSER         | PostgreSQL database user      | postgres            | String                      |
| KAFKA_HOST     | Kafka host                    | kafka               | Hostname                    |
| KAFKA_PORT     | Kafka port                    | 9092                | Number                      |
| LOG_LEVEL      | Logger level                  | INFO                | DEBUG, ERROR, WARNING, INFO |
| STATUS_TIMEOUT | Kafka timeout                 | 5                   | Number                      |

## How to run

For a simple and fast setup, an official Docker image for this service is available on
[DockerHub](https://hub.docker.com/r/dojot/device-manager-batch).

### **Standalone - with Docker**

If you really need to run Device Manager as a standalone process (without dojot's wonderful
[Docker Compose](https://github.com/dojot/docker-compose), we suggest using the minimal
[Docker Compose file](local/compose.yml). It contains only the minimum set of external services. To
run them, follow these instructions:

```shell
# 1 - Install dependencies
npm install
# 2 - Build the development server
npm run build
# 3 - Start the development server
npm run dev
# 4 - Start the test unit
npm run test
```

## How to use

The usage is via the REST API. Check the
[API documentation](https://dojot.github.io/device-manager-batch/apiary_latest.html) for more details.

## Concepts

This service holds two of the most basic and essential concepts in the dojot platform: the `device`
and the `template`. Before reading about the events, it's important to understand what each one is
and know their parameters.

### **Device**

In dojot, a device is a digital representation of an actual device or gateway with one or more
sensors or of a virtual one with sensors/attributes inferred from other devices.

Consider, for instance, an actual device with temperature and humidity sensors; it can be
represented into dojot as a device with two attributes (one for each sensor). We call this kind of
device as regular device or by its communication protocol, for instance, MQTT device or CoAP device.

We can also create devices which don’t directly correspond to their associated physical ones, for
instance, we can create one with higher level of information of temperature (is becoming hotter or
is becoming colder) whose values are inferred from temperature sensors of other devices. This kind
of device is called virtual device.

The information model used for both “real” and virtual devices is as following:

| Attribute     | Type                                                    | Mode       | Required | Description                                                                                           |
| ------------- | ------------------------------------------------------- | ---------- | -------- | ----------------------------------------------------------------------------------------------------- |
| **attrs**     | Map of attributes                                       | read-only  | No       | Map of device's attributes (check the attributes in the next table)                                   |
| **created**   | DateTime (with timezone and µs precision) in ISO format | read-only  | No       | Device creation time.                                                                                 |
| **id**        | String (length of 8 bytes)                              | read-only  | No       | Unique identifier for the device.                                                                     |
| **label**     | String (length of 128 bytes)                            | read-write | Yes      | An user-defined label to facilitate the device's identification. The label is unique for this device. |
| **templates** | Strings list                                            | read-only  | No       | List of template IDs used by the device.                                                              |
| **updated**   | DateTime (with timezone and µs precision) in ISO format | read-only  | No       | Device last update time.                                                                              |

Example device:

```json
{
  "attrs": {
    "1": [
      {
        "created": "2020-09-16T14:50:09.297163+00:00",
        "id": 1,
        "is_static_overridden": false,
        "label": "rain",
        "static_value": "",
        "template_id": "1",
        "type": "dynamic",
        "value_type": "float"
      }
    ]
  },
  "created": "2020-09-16T14:50:34.749230+00:00",
  "updated": "2020-09-16T14:55:41.897400+00:00",
  "id": "e06357",
  "label": "teste",
  "templates": [1]
}
```

The accepted parameters in the `attrs` map are:

| Attribute                | Type                                                    | Mode       | Required | Description                                                         |
| ------------------------ | ------------------------------------------------------- | ---------- | -------- | ------------------------------------------------------------------- |
| **created**              | DateTime (with timezone and µs precision) in ISO format | read-only  | No       | Device creation time.                                               |
| **id**                   | Integer                                                 | read-write | No       | Unique identifier for the attribute (automatically generated).      |
| **is_static_overridden** | Bool                                                    | read-write | No       | Whether the static value were overridden.                           |
| **label**                | String (length of 128 bytes)                            | read-write | Yes      | An user-defined label to facilitate the attribute's identification. |
| **static_value**         | String (length of 128 bytes)                            | read-write | No       | The attribute's static value (if it is a static attribute).         |
| **template_id**          | Integer                                                 | read-write | No       | From which template did this attribute come from.                   |
| **type**                 | String (length of 32 bytes)                             | read-write | Yes      | Attribute type (`static`, `dynamic`, `actuator`).                   |
| **updated**              | DateTime (with timezone and µs precision) in ISO format | read-only  | No       | Attribute last update time.                                         |
| **value_type**           | String (length of 32 bytes)                             | read-write | Yes      | Attribute value type (`string`, `float`, `integer`, `geo`).         |

All attributes that are read/write can be used when creating or updating the device. All of them are
returned when retrieving device data.

An example of such structure would be:

```json
"attrs": {
  "1": [
    {
      "label": "rain",
      "value_type": "float",
      "template_id": "1",
      "id": 1,
      "static_value": "",
      "type": "dynamic",
      "created": "2020-09-16T14:50:09.297163+00:00",
      "is_static_overridden": false
    },
    {
      "label": "mark",
      "value_type": "string",
      "template_id": "1",
      "id": 2,
      "static_value": "efac",
      "type": "static",
      "created": "2020-09-16T14:58:25.905376+00:00",
      "is_static_overridden": false
    }
  ]
}
```

### **Template**

All devices are based on a **template**, which can be thought as a blueprint: all devices built
using the same template will have the same characteristics. Templates in dojot have one unique label (any
alphanumeric sequence), a list of attributes which will hold all the device emitted information, and
optionally a few special attributes which will indicate how the device communicates, including
transmission methods (protocol, ports, etc.) and message formats.

In fact, templates can represent not only _device models_, but it can also abstract a _class of
devices_. For instance, we could have one template to represent all thermometers that will be used
in dojot. This template would have also only one attribute called `temperature`. While creating the
device, the user would select its _physical template_, let's say _TexasInstr882_, and the
`thermometer` template. The user would have also to add the translation instructions in order to map
the temperature reading that will be sent from the device to the `temperature` attribute.

In order to create a device, a user selects which templates are going to compose this new device.
All their attributes are merged together and associated to it - they are tightly linked to the
original template so that any template update will reflect all associated devices.

The information model used for templates is:

| Attribute        | Type                                                    | Mode       | Required | Description                                                                                               |
| ---------------- | ------------------------------------------------------- | ---------- | -------- | --------------------------------------------------------------------------------------------------------- |
| **attrs**        | Map of attributes                                       | read-write | No       | Merges the `config_attrs` and the `data_attrs` parameters.                                                |
| **config_attrs** | Map of attributes                                       | read-write | No       | Stores attributes with the type `meta`.                                                                   |
| **created**      | DateTime (with timezone and µs precision) in ISO format | read-only  | No       | Device creation time.                                                                                     |
| **data_attrs**   | Map of attributes                                       | read-write | No       | Stores attributes with the types `dynamic`, `static` and `actuator`.                                      |
| **id**           | String (length of 8 bytes)                              | read-write | No       | Unique identifier for the template.                                                                       |
| **label**        | String (length of 128 bytes)                            | read-write | Yes      | An user-defined label to facilitate the template's identification. The label is unique for this template. |
| **updated**      | DateTime (with timezone and µs precision) in ISO format | read-only  | No       | Device last update time.                                                                                  |

An example template structure:

```json
{
  "label": "teste",
  "attrs": [
    {
      "label": "rain",
      "value_type": "float",
      "template_id": "1",
      "id": 1,
      "static_value": "",
      "type": "dynamic",
      "created": "2020-09-16T14:50:09.297163+00:00"
    },
    {
      "label": "mark",
      "value_type": "string",
      "template_id": "1",
      "id": 2,
      "static_value": "efac",
      "type": "static",
      "created": "2020-09-16T14:58:25.905376+00:00"
    }
  ],
  "data_attrs": [
    {
      "label": "rain",
      "value_type": "float",
      "template_id": "1",
      "id": 1,
      "static_value": "",
      "type": "dynamic",
      "created": "2020-09-16T14:50:09.297163+00:00"
    },
    {
      "label": "mark",
      "value_type": "string",
      "template_id": "1",
      "id": 2,
      "static_value": "efac",
      "type": "static",
      "created": "2020-09-16T14:58:25.905376+00:00"
    }
  ],
  "id": 1,
  "config_attrs": [],
  "created": "2020-09-16T14:50:09.292714+00:00"
}
```

All attributes that are read-write can be used when creating or updating the template. All of them
are returned when retrieving device data. You might also notice some new attributes:

- `data_attrs`: stores attributes with the types `dynamic`, `static` and `actuator`.
- `config_attrs`: stores attributes with the type `meta`. You can only create this type of attribute
  via API, check its [documentation](https://dojot.github.io/device-manager/apiary_latest.html) for
  more details.

These two parameters are merged in the `attrs`.

## Events

There are some messages that are published by Device Manager to Kafka. These messages are
notifications of device management operations, and they can be consumed by any component interested
in them, such as IoT agents.

For more information on the parameters of the messages, please refer to the [Device Manager concepts
topic](#concepts).

**NOTE THAT** all messages reside in Kafka's `dojot.device-manager.device` topic.

The events that are emitted by the Device Manager Batch are:

- `create`
- `remove`

### **Event: `create`**

This message is published whenever a new device is created. Its payload is:

````json
{
  "event": "create",
  "data": {
    "label": "teste",
    "templates": [
      1
    ],
    "id": "e06357",
    "created": "2020-09-16T14:50:34.749230+00:00",
    "attrs": {
      "1": [
        {
          "label": "rain",
          "value_type": "float",
          "template_id": "1",
          "id": 1,
          "static_value": "",
          "type": "dynamic",
          "created": "2020-09-16T14:50:09.297163+00:00",
          "is_static_overridden# Device Manager Batch

The Device Manager Batch

## Use cases

The service was tested (i.e., yielded the same results and patterns) in the following scenarios:

- Requesting one attribute, even when it’s not mapped in dojot’s template;
- Requesting with dateFrom, dateTo, lastN or firstN and any combination of these parameters;
- Returning the same syntax for all type of attributes, including JSON;
- When the attribute is not provided, the service sends back all attributes.
- If the request does not contain a valid attribute or has empty data, then the service responds HTTP 404 (not found attribute), as it is in the original History.
- If the request didn’t match in a route, sends back HTTP 404.
- Apply the two converter patterns (Using one for 1 attribute and the other for all attributes requests);

## Limitations or Drawbacks

- When receiving multiple attributes, will be only used the first one.
- If the device requested doesn't exist, the History endpoint returns HTTP 404 response with a message ‘device not found’. Now, it also gives HTTP 404 response, but with the message ‘attr not found’.

## Environment variables

The configuration used in service could be received from 2 ways: The environment variables and the configuration file. It's possible select the configuration file via the HISTORYPROXY_CONFIG_FILE variable. Its default value is production.conf.

Before running the History Proxy service within your environment, make sure you configure the environment variables to match your needs.

| Environment variable       | Description                         |
| -------------------------- | ----------------------------------- |
| HISTORYPROXY_HISTORY_URL   | URL to access the History service   |
| HISTORYPROXY_RETRIEVER_URL | URL to access the Retriever service |
| HISTORYPROXY_PORT          | Port for starting the service       |

## Configuration

```shell
npm install
````

## Available Scripts

In the project directory, you can run:

### To run

```shell
npm start
```

### To test

```shell
npm test
```

# Docker

The following command creates a docker image to be a proxy for InfluxDB, mocking the History responses.

```shell
docker build -f Dockerfile -t [username]/history-proxy:[tag] .
```

To run the created image:

```shell
docker run -d [-n name] history-proxy:[tag]
```

## License

The History Proxy source code is released under Apache License 2.0.

Check NOTICE and LICENSE files for more information.
": false
}
]
}
},
"meta": {
"service": "admin"
}
}

````

### **Event: `remove`**

This message is published whenever a device is removed. Its payload is:

```json
{
  "event": "remove",
  "meta": {
    "service": "admin"
  },
  "data": {
    "id": "efac"
  }
}
````
