# Keycloak for dojot

The [Keycloak](https://www.keycloak.org/) is used by the dojot platform as an
_Identity and Access Manager_ ([IAM](https://en.wikipedia.org/wiki/Identity_management)).


In addition to generating access tokens for the platform's APIs, the Keycloak
has also been customized to run a specific [provider](https://www.keycloak.org/docs/13.0/server_development/index.html#_providers)
for the dojot. This is called [Dojot Provider](./dojot-provider) and has the
role of managing the creation of tenants (Keycloak Realms), applying
validations, customizations and publishing events on Kafka to notify the other
platform's microservices.

The dojot platform uses a _strong password policy_ and validates user passwords
against a [list](./dojot-password-blacklist.txt) of passwords that are not
allowed, but you can define your own list of passwords that are not allowed.
[Here](https://www.keycloak.org/docs-api/13.0/javadocs/org/keycloak/policy/BlacklistPasswordPolicyProviderFactory.html)
you can find out more details about this Keycloak functionality.

## Configuration

| Environment variable | Reference values | Description |
| -------------------- | ---------------- | ----------- |
| JAVA_OPTS_APPEND     | `-Dkeycloak.profile.feature.impersonation=disabled -Dkeycloak.profile.feature.upload_scripts=enabled` | Changes the default configuration of [Profiles](https://www.keycloak.org/docs/13.0/server_installation/#profiles) for dojot use. |
| KEYCLOAK_USER        | `admin` | General administrator `username` of the platform. In a real deployment this value must be changed. |
| KEYCLOAK_PASSWORD    | `admin` | General administrator `password` of the platform. In a real deployment this value must be changed. |
| DB_VENDOR            | `postgres` | Database identifier used by Keycloak. |
| DB_ADDR              | `postgres` | Host name or IP address of the Data Base Management System (DBMS). |
| DB_PORT              | `5432` | Database port. |
| DB_DATABASE          | `keycloak` | Database _name_ managed by DBMS. |
| DB_USER              | `postgres` | Database `username`. In a real deployment this value must be changed. |
| DB_PASSWORD          | `postgres` | Database `password`. In a real deployment this value must be changed. |
| KEYCLOAK_LOGLEVEL    | `INFO`     | Specify log level for Keycloak. |
| ROOT_LOGLEVEL        | `INFO`     | Specify log level for underlying container. |
| PROXY_ADDRESS_FORWARDING | `true` | When running Keycloak behind a proxy, you will need to enable proxy address forwarding. |
| KEYCLOAK_STATISTICS  | `all` | Enable some metrics. |
| DEBUG                | `true` | To attach a Java debugger, set this environment variable and the _DEBUG_PORT_ will listen. |
| DEBUG_PORT           | `*:8787` | The port that Keycloak listens to for connections from a debugger. By default, JDK 9+ only listens on localhost, so you'll want the *:8787 syntax to make it listen for connections from all hosts. |
| JAVA_OPTS | `-agentlib:jdwp=transport=dt_socket,address=$$DEBUG_PORT,server=y,suspend=y` | alternative debug: suspends the JVM until a debugger is attached (necessary to disable the DEBUG variable) |

_Note that_ these environment variables are defined by the Keycloak and more
variables can be configured. For more details, see the Keycloak page on the
[Docker Hub](https://hub.docker.com/r/jboss/keycloak/).

The Dojot Provider settings are defined by environment variables and are
documented on the page of this component.

### How to use keycloak with a nginx proxy

First set `PROXY ADDRESS FORWARDING` to the value *true*. In the example below, there is an example configuration in **nginx** to access keycloak through port *80* running on host *keycloak* on port *8080*.

```nginx
server {
    listen      80;
    location /auth {

        proxy_set_header X-Forwarded-For $proxy_protocol_addr; # To forward the original client's IP address
        proxy_set_header X-Forwarded-Proto $scheme; # to forward the  original protocol (HTTP or HTTPS)
        proxy_set_header Host $host; # to forward the original host requested by the client
        proxy_set_header X-Forwarded-Port $server_port; # to forward the original port requested by the client

        proxy_pass           http://keycloak:8080/auth;

    }
}

```
