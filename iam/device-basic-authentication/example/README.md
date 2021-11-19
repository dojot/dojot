# Example

This example shows how to issue credentials and authenticate

To run this example, type:

```sh
  cd docker-compose
  docker-compose up
```

## Using the example

As prerequisites this uses curl, jq, openssl and git.

On Debian-based Linux distributions, you can install these prerequisites by running:

```sh
  sudo apt install curl jq git
```

## Getting access token

To get a new token, send the following request:

```sh
  JWT=$(curl -s -X POST http://localhost:8000/auth \
  -H 'Content-Type:application/json' \
  -d '{"username": "admin", "passwd" : "admin"}' | jq -r ".jwt")
```

To check:

```sh
  echo $JWT
```

## Device creation

In order to properly configure a physical device in dojot, you must first create its representation in the platform.

First of all, let’s create a template for the device - all devices are based off of a template, remember.

```sh
curl -X POST http://localhost:8000/template \
-H "Authorization: Bearer ${JWT}" \
-H 'Content-Type:application/json' \
-d ' {
  "label": "Thermometer Template",
  "attrs": [
    {
      "label": "temperature",
      "type": "dynamic",
      "value_type": "float"
    },
    {
      "label": "velocity",
      "type": "dynamic",
      "value_type": "float"
    }
  ]
}'
```

The request should give back a message like this one:

```sh
  {
    "result": "ok",
    "template": {
      "created": "2018-01-25T12:30:42.164695+00:00",
      "data_attrs": [
        {
          "template_id": "1",
          "created": "2018-01-25T12:30:42.167126+00:00",
          "label": "temperature",
          "value_type": "float",
          "type": "dynamic",
          "id": 1
        },
        {
          "template_id": "1",
          "created": "2018-01-25T12:30:42.167126+00:00",
          "label": "velocity",
          "type": "dynamic",
          "value_type": "float",
          "id": 2
        }
      ],
      "label": "Thermometer Template",
      "config_attrs": [],
      "attrs": [
        {
          "template_id": "1",
          "created": "2018-01-25T12:30:42.167126+00:00",
          "label": "temperature",
          "value_type": "float",
          "type": "dynamic",
          "id": 1
        },
        {
          "template_id": "1",
          "created": "2018-01-25T12:30:42.167126+00:00",
          "label": "velocity",
          "type": "dynamic",
          "value_type": "float",
          "id": 2
        }
      ],
      "id": 1
    }
  }
```

To create a device based on template, send the following request to dojot:

```sh
  curl -X POST http://localhost:8000/device \
  -H "Authorization: Bearer ${JWT}" \
  -H 'Content-Type:application/json' \
  -d ' {
    "templates": [
      "1"
    ],
    "label": "device"
  }'
```

To check out the configured device, just send a GET request to /device:

```sh
  curl -X GET http://localhost:8000/device -H "Authorization: Bearer ${JWT}"
```

Which should give back:

```sh
  {
    "pagination": {
      "has_next": false,
      "next_page": null,
      "total": 1,
      "page": 1
    },
    "devices": [
      {
        "templates": [
          1
        ],
        "created": "2018-01-25T12:36:29.353958+00:00",
        "attrs": {
          "1": [
            {
              "template_id": "1",
              "created": "2018-01-25T12:30:42.167126+00:00",
              "label": "temperature",
              "value_type": "float",
              "type": "dynamic",
              "id": 1
            },
            {
              "template_id": "1",
              "created": "2018-01-25T12:30:42.167126+00:00",
              "label": "velocity",
              "value_type": "dynamic",
              "type": "float",
              "id": 2
           }
          ]
        },
        "id": '<deviceId>', # <-- this is the device-id
        "label": "device_0"
      }
    ]
  }
```

## Issuing credentials

Run the following command:

```sh
  curl -X POST \
    https://localhost:8080/basic-auth/v1/devices/:deviceId/basic-credentials \
    -H 'content-type: application/json' \
    -H 'Authorization: Bearer ${TOKEN}'
```

### Check if message has been sent

In the browser go to http://localhost:9090/topic/admin.dojot.device-manager.basic-credentials/messages and check if the message is there.

## Authentication

Run the following command:

```sh
  curl -X POST \
    https://localhost:8080/basic-auth/v1/internal/authentication \
    -H 'content-type: application/json' \
    -H 'Basic base64encode(username:password)'
```
