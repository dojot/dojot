# Keycloak for dojot

The [Keycloak](https://www.keycloak.org/) is used by the dojot platform as an
_Identity and Access Manager_ ([IAM](https://en.wikipedia.org/wiki/Identity_management)).


In addition to generating access tokens for the platform's APIs, the Keycloak
has also been customized to run a specific [provider](https://www.keycloak.org/docs/12.0/server_development/index.html#_providers)
for the dojot. This is called [Dojot Provider](./dojot-provider) and has the
role of managing the creation of tenants (Keycloak Realms), applying
validations, customizations and publishing events on Kafka to notify the other platform's microservices.
The Dojot Provider settings are defined by environment variables and are
documented on the page of this component.

