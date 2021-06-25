# Kong plugin PEPkong

A plugin for the [Kong Microservice API Gateway](https://konghq.com/solutions/gateway/) intended to work with [Keycloak](https://www.keycloak.org/)
to build an authorization layer for Rest API.

This plugin interact with Keycloak through [Authorization Service Endpoint](https://www.keycloak.org/docs/12.0/authorization_services/#_service_authorization_api),
while this plugin is the PEP (Policy Enforcement Point) and Keycloak is the PDP (Policy Decision Point).

## Table of Contents

- [Table of Contents](#table-of-contents)
- [Tested and working for](#tested-and-working-for)
- [Installation](#installation)
  - [From source](#from-source)
- [Configuration](#configuration)
  - [Environment variables](#environment-variables)
- [Usage](#usage)
  - [Enabling on endpoints](#enabling-on-endpoints)
    - [Configuring on a service registered in kong](#configuring-on-a-service-registered-in-kong)
  - [Parameters](#parameters)
- [Running tests](#running-tests)

## Tested and working for

| Kong Version |   Tests passing    |
| ------------ | :----------------: |
| 2.0.x        | :white_check_mark: |
| 2.3.x        | :white_check_mark: |
| 2.4.x        | :white_check_mark: |

| Keycloak Version |   Tests passing    |
| ---------------- | :----------------: |
| 12.0.2           | :white_check_mark: |
| 12.0.4           | :white_check_mark: |
| 13.0.0           | :white_check_mark: |

## Installation

### From source

```bash
luarocks make
```

## Configuration

### Environment variables

Key    | Purpose        | Default Value      | Valid Values  |
-------------- | ----------------- | ---------------| -----------|
DOJOT_PLUGIN_CLIENT_ID     | Client ID with permissions to invoke the Keycloack authorization service | kong  | string
DOJOT_PLUGIN_SSL_VERIFY  |  Enables certificate validation  | "true"  | "true" or "false" as a string
DOJOT_PLUGIN_REQUEST_TIMEOUT | Timeout in milliseconds for requests made to the Keycloack | 500 |  number



__NOTE__  To load a CA certificate, for example a self-signed one, it is necessary to pass the path to environment variable ``KONG_LUA_SSL_TRUSTED_CERTIFICATE``.


## Usage

### Enabling on endpoints

The same principle applies to this plugin as the [standard jwt plugin that comes with kong](https://docs.konghq.com/hub/kong-inc/jwt/). You can enable it on service, routes and globally.

#### Configuring on a service registered in kong

An example of how to configure this plugin in kong on a service (`{service}`), where `{resource_name}` is `Resources` configured in `Authorization` inside on a `Client`with name defined by  `DOJOT_PLUGIN_CLIENT_ID` and configured in **Keycloak**. And kong running in administrative port 8001.

```bash
curl -X POST http://localhost:8001/services/{service}/plugins \
    --data "name=pepkong" \
    --data "config.resource={resource_name}"
```

### Parameters

| Parameter  | Requied | Default | Description |
| ---------- | ------- | ------- | ---------- |
| name       | yes     |         | The name of the plugin to use, in this case `pepkong`. |
| enabled    | no      | `true`  | Whether this plugin will be applied.                   |
| config.resource_server  | yes  | `kong`  | Resource server name.                         |
| config.resource    | yes    | `Default Resource`| Resource name. |
| config.scope       | yes   | `POST=>create`, `GET=>view`, `PATCH=>update`, `PUT=>update`, `DELTE=>delete`       | Map HTTPS verbs to scopes.  |

## Running tests

```bash
docker build -t pepkong -f tests/unit_tests/Dockerfile ../.. && docker container run pepkong
```
