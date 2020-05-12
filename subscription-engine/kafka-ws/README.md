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
[examples](./examples) directory for an example client.

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
```
/v1/websocket/device-data
/v1/websocket/device-data?fields=sensor.status,temperature&where=sensor.status=in:failed,stopped
/v1/websocket/device-data?fields=temperature,location
/v1/websocket/device-data?where=temperature=lt:10.0;temperature=gte:5.0;
/v1/websocket/device-data?where=rain=lte:15;&fields=temperature,rain
```

---

## **Filtering flow**

The filtering happens right after a message is received in a Kafka topic that the client has
requested to subscribe. The message is then filtered by `where` (if present) and then by `fields`
(if present). This flux is better explained in the next image:

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
KAFKA_HOSTS   | comma-separated list of Kafka hosts (with port) | kafka-server:9092 | list of hostname:port    |
KAFKA_WS_HOST | Kafka WS address                                | localhost         | hostname                 |
KAFKA_WS_PORT | Kafka WS port                                   | 8080              | valid port               |

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

## **Examples**

Check the [examples](./examples) directory for more info.
