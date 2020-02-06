# IoTAgent MQTT (VerneMQ)

## Overview

An IoTAgent is an adaptation service between physical devices and dojot’s core components. The IoTAgents are responsible for receiving messages from physical devices (directly or through a gateway) and sending them commands in order to configure. The dojot platform can have multiple iot-agents, each one of them being specialized in a specific protocol like in this case MQTT. It is also responsible to ensure that it communicates with devices using secure channels.

The IoTAgent MQTT is a extension of [VerneMQ](https://github.com/vernemq/vernemq) with some features e services for dojot case. The **VerneMQ** receiving messages from devices and sending messages for them. The V2K-bridge service receives messages from VerneMQ via MQTT and send this messages to Kafka to be use in dojot’s core components. While the K2V-bridge service receives messages from dojot’s core components via Kafka and send them for VerneMQ via MQTT, and then the device receives this message. See this all flux in Fig. 1.

The VerneMQ by default the protocol versions allowed are versions 3 and 4 (MQTT v3.1 and v3.1.1 respectively).

![image](http://www.plantuml.com/plantuml/proxy?src=https://raw.githubusercontent.com/dojot/dojot/epic-100kMqttDevices/connector/mqtt/vernemq/docs/plant_uml/mqtt/diag_mqtt)

Fig. 1 - VerneMQ with Dojot (Whereas V2K-Bridge, K2V-Bridge, VerneMQ and Client already have certificates, can occur later communications between PKI and these services. This is better explained later.)

### VerneMQ Broker with Custom to Dojot

The VerneMQ Broker with Custom to Dojot has some additions of scripts to integration with our CA for communications via TLS and security.

For more see [here](./broker)

### V2K-bridge

The V2K-bridge service implements a bridge between VerneMQ broker and Kafka broker. Basically, it subscribes to some MQTT topics and forward the messages to some Kafka topics following the dojot's topics rules. In order to scale the bridge service, shared MQTT subscriptions are used, which allows to instantiate a group of consumers, i.e. a group of bridge instance, when necessary. The communication between the bridge and the VerneMQ is secured with mutual TLS. Soon, the communication with Kafka will also use mutual TLS.

For more see [here](./v2k-bridge)

### K2V-bridge

The K2V-bridge service implements a bridge between Kafka broker and VerneMQ broker. It recevies messagens from some Kafka dojot's topics and publish the messages to a some MQTT topics.
The communication between the bridge and the VerneMQ is secured with mutual TLS. Soon, the communication with Kafka will also use mutual TLS.

For more see [here](./k2v-bridge)

## ACL (access-control list)

An **ACL** (access-control list) based authorization is provided to manage permissions, so a device can only publish and subscribe to its own topic, the topics are:

- For publish: ***tenant***_:_***device_id***_/attrs_
- For subscription: ***tenant***_:_***device_id***_/config_

Where ***tenant*** is the information context separation of dojot and ***device_id*** is a unique identification for the device.
The junction (***tenant:device_id***) of *tenant* and  *device_id* must be unique.

See more about [ACL Plugin](vernemq/plugins/dojot_acl_plugin) for VerneMQ.

## Security

We use **TLS** mutual authentication to provide a secure communication between devices and broker [VerneMQ](https://github.com/vernemq/vernemq) through **MQTT**.  Transport Layer Security (**TLS**) is a cryptographic protocol designed to provide communications security over a computer network.

In **TLS** each device and broker should be provisioned with a private key and corresponding public certificate sign from **CA** (certificate authority) and a root certificate (public certificate of **CA**), the **CA** is include in **PKI** (public key infrastructure). A **PKI**  ([EJBCA](./ejbca)) is a system for the creation, storage, and distribution of digital certificates.

Also a PKI includes the certificate revocation list (**CRL**), which is a list of certificates that have been revoked before reaching the expiration date of the certificate.

The process of obtaining certificates for client (Fig. 2):

- Creation of entity at EJBCA, usually this unique entity consists of ***tenant***_:_***device_id***.

- Obtaining a public certificate for the client, which happens with the creation of the CSR (Certificate Signing Request, it usually contains the public key, identifying information such as a hostname and unique identification) and its submission to obtain a public certificate.

- Obtaining the root certificate (public certificate of **CA**).

![image](http://www.plantuml.com/plantuml/proxy?src=https://raw.githubusercontent.com/dojot/dojot/epic-100kMqttDevices/connector/mqtt/vernemq/docs/plant_uml/mqtt/seq_sec_client)

Fig. 2 - Client retrives certificates from PKI (EJBCA)

The step by step on how to get a certificate for a client will be explained later.

The process of obtaining certificates for K2V Brige and K2V-bridge instances follows the same steps as for the client.

The process of obtaining certificates for VerneMQ instances follows the same steps as for the client, with some more followed by (Fig. 3):

- At each defined time (CHECK_EXPIRATION_TIME), it's checked if the root certificate and public certificate of the service instance will expire in the next CHECKEND_EXPIRATION_SEC seconds.

- At each defined time (CHECK_BROKER_CERT_REVOKED_TIME), it's checked if the public certificate of the service instance has been revoked.

- CRL updated the CRL certificate every time by setting in CRL_UPDATE_TIME

![image](http://www.plantuml.com/plantuml/proxy?src=https://raw.githubusercontent.com/dojot/dojot/epic-100kMqttDevices/connector/mqtt/vernemq/docs/plant_uml/mqtt/seq_sec_service1)

Fig. 3 - VerneMQ (Broker) retrive certificates from PKI (EJBCA)

The TLS connection has a maximum life time, see more about [Disconnect Plugin](vernemq/plugins/dojot_disconnect_plugin) for VerneMQ.
The TLS connection also has a configurable timeout, which is a VerneMQ configuration.

Environment variables mentioned above are more described in [here](./vernemq)

## How to connect a device with the IoTAgent-MQTT witn Security (via TLS)

### **Prerequisites**

- Create a device in Dojot and get a tenant and a device ID.
- Install [openssl](https://www.openssl.org/), [jq](https://stedolan.github.io/jq/) and [cURL](https://curl.haxx.se/).

NOTE: Ports and address may change depending on the deployment

Environment variables

### Steps


#### 1. Set variables environment

##### Address of the dojot

Change the value of the DOJOT_URL variable from the code block below and execute:

```console
export DOJOT_URL=http://myhost:8000
```

##### Server hostname

Change the value of the DOJOT_DNS variable from the code block below and execute:

```console
export DOJOT_DNS=myhost
```

##### Certificate authority Name

Change the value of the CA_NAME variable from the code block below and execute:

```console
export CA_NAME=IOTmidCA
```

##### Dojot Autentication Token

Change the value of the DOJOT_USERNAME and DOJOT_PASSWORD variables from the code block below and execute:

```console
export DOJOT_USERNAME=user
export DOJOT_PASSWORD=password
```


#### 2. GET JWT TOKEN

Get token from dojot

```console
export JWT=$(curl -X POST ${DOJOT_URL}/auth \
-H 'Content-Type:application/json' \
-d "{\"username\": \"${DOJOT_USERNAME}\", \"passwd\": \"${DOJOT_PASSWORD}\"}" | jq '.jwt' -r)
```

#### 3. Create a folder

It's  recommended to create a new folder for certificates.

```console
mkdir -p certs
cd certs
```

#### 4. Generate pair of (public and private) RSA key

Create a *client.key* file with a pair of keys.

```console
 openssl genrsa -out client.key 2048
```

#### 5. Create entity in ejbca

NOTE: Where ***tenant*** is the information context separation of dojot and ***device_id*** is a unique identification for the device.

##### Create environment variables with device unique identification

Change the value of TENANT and DEVICE_ID variables from block of code below and run:

```console
export TENANT=admin
export DEVICE_ID=a1998e
```

##### Create entity in EJBCA

```console
curl  -X POST ${DOJOT_URL}/user \
    -H "Authorization: Bearer ${JWT}" \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" \
    -d "{\"username\": \"${TENANT}:${DEVICE_ID}\"}"
```

#### 6. Create a CSR

Create CSR file (*client.csr*):

```console
openssl req -new  -sha256 -out client.csr -key client.key \
        -addext "subjectAltName = DNS: ${DOJOT_DNS}" \
        -addext "keyUsage = Digital Signature, Non Repudiation, Key Encipherment" \
        -addext "basicConstraints  =  CA:FALSE" \
        --subj "/CN=${TENANT}:${DEVICE_ID}"
```

And create a environment variable with CSR content:

```console
export CSR_CONTENT=$(cat client.csr | sed '1,1d;$ d' | tr -d '\r\n')
```

#### 7. Signing certificate for entity (Get public certificate for the client)

##### Send CSR

Send CSR to EJBCA and save this environment variable

```console
export CRT_ENTITY_CONTENT=$(curl  -X POST ${DOJOT_URL}/sign/${TENANT}:${DEVICE_ID}/pkcs10 \
-H "Authorization: Bearer ${JWT}" \
-H "Content-Type:application/json" \
-H "Accept:application/json" \
-d  "{\"certificate\": \"${CSR_CONTENT}\", \"passwd\": \"dojot\"}" | jq '.status.data' -r)
```

##### Create a public certificate

Create *client.crt* file from EJBCA return

```console
(echo  "-----BEGIN CERTIFICATE-----"
echo ${CRT_ENTITY_CONTENT}
echo "-----END CERTIFICATE-----" ) > client.crt
```

#### 8. Obtaining the root certificate (public certificate of **CA**)

##### Retrieve CA Certificate

Retrieve certificate and set in a environment variable

```console
export CRT_ROOT_CONTENT=$(curl  -X GET ${DOJOT_URL}/ca/${CA_NAME} \
-H "Authorization: Bearer ${JWT}" \
-H "Content-Type:application/json" \
-H "Accept:application/json" | jq '.certificate' -r )
```

##### Create a file certificate from Root

Create a root certificate *root.crt* file from EJBCA return

```console
(echo  "-----BEGIN CERTIFICATE-----"
echo ${CRT_ROOT_CONTENT}
echo "-----END CERTIFICATE-----" ) > root.crt
```

## Simulating a device with mosquitto

NOTE: Ports and address may change depending on the deployment

NOTE 2: The **tenant** is *admin* and **device_id** is *a1998e* for these examples. You must change them for your case.

In these examples we will use [mosquitto](https://mosquitto.org/) client.

##### Some options for mosquitto_pub and mosquitto_sub

- **-h**: Specify the host to connect to. Defaults to localhost. In this case: *myhost*.

- **-p**: Connect to the port specified. If not given, the default of 1883 for plain MQTT or 8883 for MQTT over TLS will be used. In this case: *30311*.

- **-t**: The MQTT topic on which to publish the message. See mqtt(7) for more information on MQTT topics. In this case: *admin:a1998e/attrs* to publish and *admin:a1998e/config* to subscription.

- **--cafile**: Define the path to a file containing PEM encoded CA certificates that are trusted. Used to enable SSL communication. In this case: *root.crt*

- **--cert**: Define the path to a file containing a PEM encoded certificate for this client, if required by the server. In this case: *client.crt*

- **--key**: Define the path to a file containing a PEM encoded private key for this client, if required by the server. In this case: *client.key*

- **-m**: Send a single message from the command line.

- **-u**: Provide a username to be used for authenticating with the broker.

### With security: use TLS to communicate with VerneMQ

The tree files are require: *client.crt*, *client.key* and *root.crt*. See example:

##### Example on how to publish

```console
mosquitto_pub -h myhost -p 8883 -t admin:a1998e/attrs -m '{"attr_example": 10 }' --cert client.crt  --key client.key --cafile root.crt
```

Note: In this case, the message is a publish  on an attribute with the label attr_example and a new value 10 in device *a1998e* with tenant *admin*.

##### Example on how to subscribe

```console
mosquitto_sub -h myhost -p 8883 -t admin:a1998e/config --cert client.crt  --key client.key --cafile root.crt
```

### Without security

**MQTT without security is not recommended, use this for testing only.**

##### Prerequisites

Create a device in Dojot and get a tenant and a device ID.

##### Example on how to publish

```console
 mosquitto_pub -h myhost -p 1883 -t admin:a1998e/attrs -m '{"attr_example": 10 }' -u admin:a1998e
```

Note: In this case, the message is a publish  on an attribute with the label attr_example and a new value 10 in device *a1998e* with tenant *admin*.

##### Example on how to subscribe

```console
 mosquitto_sub -h myhost -p 1883 -t admin:a1998e/attrs -u admin:a1998e
```
