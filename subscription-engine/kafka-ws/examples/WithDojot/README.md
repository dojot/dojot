# Kafka WS example application - with Dojot

In the Dojot deployment, we provide Kafka WS service to allow anyone with access to the environment
to retrieve data with Websockets. This example runs a simple Websocket client that has this
capability.

## Usage

__Note__ You need to enable the `dev-test-cli` client in the keycloak. For security reasons it is disabled by default, after use it is recommended to disable it again.

First of all, you need access to some Dojot environment. If you don't have one, check out our
[installation guide](https://dojotdocs.readthedocs.io/en/latest/installation-guide.html).

It is provided a Docker Compose file with the whole environment to see Kafka WS in action,

To run this example, type:

```shell
docker-compose up
```

## Configuration

You need to change the environment variables in the Docker Compose file to reflect your needs.

| Key                       | Purpose                                                                                | Default Value     | Valid Values        |
| ------------------------- | -------------------------------------------------------------------------------------- | ----------------- | ------------------- |
| KAFKAWS_APP_FILTER_FIELDS | Field filtering, check the [Kafka WS documentation](../../README.md) for details       | empty string      | valid fields filter |
| KAFKAWS_APP_FILTER_WHERE  | Conditional filtering, check the [Kafka WS documentation](../../README.md) for details | empty string      | valid where filter  |
| KAFKAWS_APP_KAFKA_TOPIC   | The topic to subscribe in Kafka                                                        | admin.device-data | Kafka topic name    |
| KAFKAWS_DOJOT_ADDRESS     | Address where Dojot is located                                                         | localhost:3000    | IP/hostname:port    |
| KAFKAWS_DOJOT_PASSWORD    | Dojot user password                                                                    | admin             | password            |
| KAFKAWS_DOJOT_USER        | Dojot user                                                                             | admin             | username            |
| KAFKAWS_TLS_CA_FILE       | CA certificate file location. **Required if TLS is enabled**.                          | ./certs/ca.crt    | Path to a file      |
| KAFKAWS_TLS_ENABLE        | Enable TLS, the server must be configured accordingly                                  | false             | boolean             |

In the next sections we will show some configurations that are needed when using TLS.

## TLS configuration

If you want to use TLS, you need to activate it both in the server side (see the Dojot documentation
for more info) and in the client side. Check out the [Docker Compose file](./docker-compose.yml) for
this example client to configure its TLS.

Aside from it, you need some certificates. You can use our dummy certificate generator tool in the
`tools` directory in the repository root. You can also use real certificates, e.g. Let's Encrypt.
Just make sure that you have some CA certificate and it can be read by the client.

### Changing hosts file

Another important detail to keep in mind is the need to change your `/etc/hosts` file. If you used
something like Let's Encrypt, you will be fine as long as you use a hostname for your Dojot
installation (e.g. dojot.iotplat). For dummy certificates, like the ones generated with the
forementioned script, you will need to add a new entry in this file. The certificates do not work
very well with IPs, specially when running Dojot behind some load balancer like NGINX or HAProxy.

We need to make a simple addition in the hosts file for it to run smoothly. Example entry:
```
1.2.3.4 dojot.iotplat
```

This will make sure your requisitions work as expected.
