# Example

This example shows how to run an environment for testing message publishing to dojot through iotagent-http with HTTPS.

To run this example, type:

```sh
  cd docker-compose
  docker-compose up
```

## Using the example

As prerequisites this uses curl, jq, openssl and git.

On Debian-based Linux distributions, you can install these prerequisites by running:

```sh
  sudo apt install curl jq openssl git
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

To create a device based on one or more templates, send the following request to dojot:

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

## Generate certificates

Firstly, get the certreq tool:

```sh
  git clone https://github.com/dojot/dojot.git
  cd dojot
  git checkout v0.7.0
  cd tools/certreq
```

Run the script with the device id obtained in the previous steps:

```sh
  ./bin/certreq.sh \
    -h localhost \
    -p 8000 \
    -i '<deviceId>' \
    -u 'admin' \
    -s 'admin'
```

## Publish message

Run the following command with the certificate paths:

```sh
  curl -X POST \
    https://localhost:8080/http-agent/v1/incoming-messages \
    -H 'content-type: application/json' \
    -d '{
      "ts": "2021-06-16T09:32:01.683000Z",
      "data": {
        "temperature": 25.79,
        "heartRate": 70,
        "respiratoryRate": 40,
        "battery": 3
      }
  }' \
  --cacert ca/ca.pem \
  --cert cert_<deviceId>/cert.pem \
  --key cert_<deviceId>/private.key
```

or

```sh
  curl -X POST \
    https://localhost:8080/http-agent/v1/incoming-messages/create-many \
    -H 'content-type: application/json' \
    -d '[
      {
        "ts": "2021-06-16T09:32:01.683000Z",
        "data": {
          "temperature": 25.79,
          "heartRate": 70,
          "respiratoryRate": 40,
          "battery": 3
        }
      },
      {
        "data": {
          "temperature": 25.79,
          "heartRate": 70,
          "respiratoryRate": 40,
          "battery": 3
        }
    }
  ]' \
  --cacert ca/ca.pem \
  --cert cert_<deviceId>/cert.pem \
  --key cert_<deviceId>/private.key
```

## Check if message has been sent

In the browser go to http://localhost:9090/topic/admin.device-data/messages and check if the message is there.

## Notes

- If a timestamp is not part of the message sent by a device, it will be added by the http-agent.
- In the x509-identity-mgmt service be sure to set the X509IDMGMT_CERTIFICATE_CHECK_SUBJECTDN variable to "true", if it is passed the certificate cname will be tenant:device.
