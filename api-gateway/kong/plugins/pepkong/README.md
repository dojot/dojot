# Kong plugin PEPkong

A plugin for the [Kong Microservice API Gateway](https://konghq.com/solutions/gateway/) intended to work with [Keycloak](https://www.keycloak.org/)
to build an authorization layer for Restful APIs.

This plugin interact with Keycloak through [Authorization Service Endpoint](https://www.keycloak.org/docs/12.0/authorization_services/#_service_authorization_api),
while this plugin is the PEP (Policy Enforcement Point) and Keycloak is the PDP (Policy Decision Point).

## Table of Contents

- [Table of Contents](#table-of-contents)
- [Tested and working for](#tested-and-working-for)
- [Installation](#installation)
  - [From source](#from-source)
  - [Changing default resource server](#Changing-default-resource-server)
- [Usage](#usage)
  - [Enabling on endpoints](#enabling-on-endpoints)
    - [Service](#service)
  - [Parameters](#parameters)
- [Testing](#testing)
  - [Running tests](#running-tests)

 ## Tested and working for

| Kong Version |   Tests passing    |
| ------------ | :----------------: |
| 2.0.x        | :white_check_mark: |
| 2.3.x        | :white_check_mark: |

| Keycloak Version |   Tests passing    |
| ---------------- | :----------------: |
| 12.0.2           | :white_check_mark: |

## Installation

### From source

```bash
luarocks make
```

## Changing default resource server

To invoke Keycloak authorization service is necessary define a resource server (client), the default value is `kong`,
but is possible to change it through `CLIENT_ID` environment variable.

## Usage

### Enabling on endpoints

The same principle applies to this plugin as the [standard jwt plugin that comes with kong](https://docs.konghq.com/hub/kong-inc/jwt/). You can enable it on service, routes and globally.

#### Service

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

### Running tests

```bash
docker build -t pepkong -f tests/unit_tests/Dockerfile ../.. && docker container run pepkong
```
