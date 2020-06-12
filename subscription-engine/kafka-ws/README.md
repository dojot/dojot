# **Kafka WS**

The **Kafka WS** service provides support for pure WebSocket connections to retrieve data directly
from Kafka.

Work in progress...

# **Overview**

**Kafka WebSocket service** allows the user to retrieve conditional and/or partial data from a topic
in a Kafka cluster. It works with pure websocket connections, so you can create clients in any
language you want, as long as the client supports **RFC 6455** websockets.

---

## **Connecting to the service**

The connection is done via pure websockets in the `/v1/websocket/:topic` endpoint. See the
[examples](./examples) directory for a client example.

### **HTTP error codes**

- `426`: occurs when the received connection is not a Websocket one.

### **Websocket error codes**

- `4000` - INVALID_SYNTAX: there is a syntatic problem with `where`
- `4001` - INVALID_OPERATOR: an invalid operator has been passed to a condition, see the [list of
operators](#applying-conditions) for more info
- `4002` - INVALID_ESCAPE_VALUE: an unsupported escape character has been passed to a condition
- `4003` - INVALID_OPERATOR_ARITY: the number of values in a condition is invalid for the operator
- `4004` - INVALID_VALUE: a value with an invalid type was passed to a condition
- `4400` - INVALID_PATHNAME: a Malformed URI was passed
- `4401` - INVALID_TOKEN_JWT: it isn't possible to extract information (exp and service) from the JSON Web Token (JWT) 
- `4403` - FORBIDDEN_TOPIC: the tenant sent in the token jwt cannot access the kafka topic passed
- `4408` - EXPIRED_CONNECTION: connection lifetime is over
- `4999` - INTERNAL: there is an error in the server

---

## **Understanding the URI parts**

Before jumping in the explanation on how each filter works and its rules, we need to understand the
parts of the URI that applies these filters. The general format of it is:
```
/v1/websocket/:topic?fields=<selector>&where=<conditions>
```

### **Topic**

The `topic` parameter is the Kafka topic that you want to receive data from.

### **Fields**

The `fields` parameter tells Kafka WS to retrieve only determined parameters from the messages.

See [this section](#selecting-parameters) for an explanation.

### **Where**

The `where` parameter tells Kafka WS to retrieve only messages in which the parameters meet the
conditions.

Grammar:
- `where → expression:*`
- `expression → condition:+`
- `condition → selector=(operator:):?values;`
- `selector → parameter | parameter.selector`
- `operator → `[see here](#applying-conditions)
- `values → value | value,values`

Where:
- `:*` - zero or more
- `:+` - one or more
- `:?` - zero or one

### **Examples**

To ilustrate the parameters' usage, here are some examples of valid URIs:

Retrieving full messages from `topic.example` topic:
```
/v1/websocket/topic.example
```

Retrieving a sensor status and temperature when the status is `failed` or `stopped`:
```
/v1/websocket/topic.example?fields=sensor/status,temperature&where=sensor.status=in:failed,stopped;
```

Retrieving the temperature and location:
```
/v1/websocket/topic.example?fields=temperature,location
```

Retrieving full messages where 5.0 ≤ temperature < 10.0:
```
/v1/websocket/topic.example?where=temperature=lt:10.0;temperature=gte:5.0;
```

Retrieving the temperature and rain when rain ≤ 15:
```
/v1/websocket/topic.example?where=rain=lte:15;&fields=temperature,rain
```

---

## **Filtering flow**

The filtering happens right after a message is received in a Kafka topic that the client has
requested to subscribe. The message is then filtered by `where` (if present) and then by `fields`
(if present). This flow is better explained in the next image:

![image](http://www.plantuml.com/plantuml/proxy?src=https://raw.githubusercontent.com/dojot/dojot/epic-kafka-ws/subscription-engine/kafka-ws/docs/plant_uml/message_flow)

**Fig. 1** - Kafka messages' flow within the system.

---

## **Selecting parameters**

The rules to select parameters from a message are:
- `a,b,c`: select multiple parameters
- `a/b/c`: select a parameter from its parent
- `a(b,c)`: select multiple parameters from a specific parameter
- `a/*/c`: wildcard selection

Examples:
```
{a: 1, b: 2, c: 3} → f(a,b) → {a: 1, b: 2}
{a: {b: {c: 3, d: 4}}} → f(a/b/c) → {a: { b: {c: 3}}}
{a: {b: 2, c: 3, d: 4}} → f(a(b,c)) → {a: {b: 2, c: 3}}
{a: {b: {c: 1}, d: {e: 2}, f: {c: 2}}} → f(a/*/c) → {a: {b: {c: 1}, d: {}, f: {c: 2}}}
```

---

## **Applying conditions**

The conditions can be applied to any parameter in the message. The permitted operators are:

### **Set operators**

Applied to a parameter via a set of values. These operators are applied to N possible values.
- `in`: returns the parameter if the value is in the list
- `nin`: returns the parameter if the value is not in the list

Examples:
```
{ a: 'foo', b: 'bar' } → f(a=in:bar,baz) → discard
{ a: 'foo', b: 'bar' } → f(a=nin:bar,baz) → continue to process
```

### **Arythmetic operators**

Applied to a parameter via one value. These operators are applied to 1 possible value.
- `neq`: not equal
- `gt`: greater than
- `gte`: greater or equal to
- `lt`: less than
- `lte`: less or equal to

**Obs.:** if you do not pass an operator, it is equal.

Examples:
```
{ rain: 10, temperature: 30.0 } → f(rain=11) → discard
{ rain: 10, temperature: 30.0 } → f(rain=neq:11) → continue to process
{ rain: 10, temperature: 30.0 } → f(rain=gt:10) → discard
{ rain: 10, temperature: 30.0 } → f(rain=gte:10) → continue to process
{ rain: 10, temperature: 30.0 } → f(rain=lt:10) → discard
{ rain: 10, temperature: 30.0 } → f(rain=lte:10) → continue to process
```

# **Running the service**

## **Preparing the environment**

### **Configuration**

Before proceeding, **make sure you configure your environment**.

Key           | Purpose                                         | Default Value     | Valid Values             |
------------- | ----------------------------------------------- | ----------------- | ------------------------ |
LOG_LEVEL     | log level                                       | info              | info, warn, debug, error |
LOG_VERBOSE   | Enables verbose mode for logging                    | false              | string: "true" or "false" |
LOG_FILE   | Enables logging on files  (location: /var/log/kafka-ws-logs-%DATE%.log)                     | false              | string: "true" or "false" |
LOG_FILE_LEVEL     | Log level to log on files                                     | debug              | info, warn, debug, error |
KAFKA_HOSTS   | comma-separated list of Kafka hosts (with port) | kafka-server:9092 | list of hostname:port    |
KAFKA_WS_HOST | Kafka-ws address                                | 0.0.0.0         | hostname                 |
KAFKA_WS_PORT | Kafka-ws port                                   | 8080              | valid port               |
KAFKA_WS_TLS | Kafka-ws secure - enables TLS ( Needs: KAFKA_WS_TLS_CA_FILE, KAFKA_WS_TLS_KEY_FILE and KAFKA_WS_TLS_CERT_FILE )                                   | false              | string: "true" or "false"              |
KAFKA_WS_TLS_CA_FILE | Kafka-ws ca file location      | /opt/kafka-ws/certs/ca-cert.pem              | valid path               |
KAFKA_WS_TLS_KEY_FILE | Kafka-ws key file location      | /opt/kafka-ws/certs/server-key.pem              | valid path               |
KAFKA_WS_TLS_CERT_FILE | Kafka-ws certificate file location      | /opt/kafka-ws/certs/server-cert.pem              | valid path               |
KAFKA_WS_JWT_HEADER_AUTH   | Enables use token jwt in authorization header | false              | string: "true" or "false" |
KAFKA_WS_JWT_EXP_TIME   | Enables use exp, expiration time,  from jwt (Needs KAFKA_WS_JWT_HEADER_AUTH)                     | false               | string: "true" or "false" |
KAFKA_WS_MAX_LIFE_TIME   | Maximum lifetime of a connection   (-1 to disable)                 | 720              | seconds |
REDIS_HOST       | Redis host                   | redis                              | string |
REDIS_PORT       | Redis port                   | 6379                               | number |
REDIS_DATABASE   | Redis database               | 1                                  | number |

Note1: Maximum lifetime of a connection will be the highest value between the time calculate from expiration timestamp provided by the JSON Web Token (JWT)  (if KAFKA_WS_JWT_EXP_TIME is true) and KAFKA_WS_MAX_LIFE_TIME. If KAFKA_WS_MAX_LIFE_TIME is -1, it will be the value provided in the time calculate from expiration timestamp provided by the JSON Web Token (JWT)  (if KAFKA_WS_JWT_EXP_TIME is true), otherwise there would be no maximum connection lifetime.

Note2: When KAFKA_WS_JWT_HEADER_AUTH is true, it is checked whether the service (tenant) that is passed in the JSON Web Token (JWT) can access the kafka topic, generally topics start with `tenant.*`

### ** JSON Web Token (JWT) **

When KAFKA_WS_JWT_HEADER_AUTH is true, it is necessary to provide a JSON Web Token (JWT)  in the Header on the Autorization field, as in this example in js below  (see more https://tools.ietf.org/html/rfc7519#section-4):

```js

const makeJwtToken = (tenant, expirationTime) => {
  const payload = {
    service: tenant,
    exp: expirationTime,
  };
  return `${Buffer.from('jwt schema').toString('base64')}.${
    Buffer.from(JSON.stringify(payload)).toString('base64')}.${
    Buffer.from('dummy signature').toString('base64')}`;
};

// Unix time, time in seconds since the epoch.
const expirationTime = 1591369727;
new WebSocket('ws://localhost:8080/tenant.ws.example.test', {
  headers: {
    Authorization: `Bearer ${makeJwtToken('tenant', expirationTime)}`,
  },
});
```

### **Parser compilation**

Before running the program, you need to compile the Nearley parser:
```shell
npm run parser:compile
```

---

## **Standalone**

```shell
npm run kafka-ws
```

If you are developing, you can use `nodemon` too:
```shell
npm run dev
```

---

## NOTE

To use WebSocket with Nginx, Kong, Api gateway or similar, look for timeout settings. In Nginx, for example: proxy_connect_timeout, proxy_send_timeout and proxy_connect_timeout; And in Kong on a Service, for example: connect_timeout, write_timeout and read_timeout.

## **Examples**

Check the [examples](./examples) directory for more info.
