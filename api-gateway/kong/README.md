# Kong

## Kong's authorization layer

The [Kong Microservice API Gateway](https://konghq.com/solutions/gateway/) was extended by dojot in order to perform authorization task, it was done by adding two plugins: [jwt-keycloak](https://github.com/gbbirkisson/kong-plugin-jwt-keycloak) and PEPKong.

### Kong plugin jwt-keycloak

The [jwt-keycloak](https://github.com/gbbirkisson/kong-plugin-jwt-keycloak) is responsible to validate access tokens issued by Keycloak, like validate token signature and `exp` claim. It uses the Well-Known Uniform Resource Identifiers provided by Keycloak to load JWK public keys from issuers that are specifically allowed for each endpoint.

### Kong plugin PEPKong

PEPKong is a dojot plugin responsible for validations using Role-based access control (RBAC) policies with the [Keycloak authorization service](https://www.keycloak.org/docs/12.0/authorization_services/index.html#_service_overview). See more about this plugin in the folder [./plugins/pepkong](./plugins/pepkong).

![Component architecture](./kong-authz-architecture.png?raw=true)

### Examples

There is an example of how to use kong with Keycloak and some explanations in the folder [./examples](./examples).
