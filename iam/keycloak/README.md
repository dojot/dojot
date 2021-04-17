# Keycloak for dojot

The _Keycloak_ is used by the dojot platform as an _Identity and Access Manager_
([IAM](https://en.wikipedia.org/wiki/Identity_management)).
In addition to generating access tokens for the platform's APIs, the Keycloak
has also been customized to run a specific module for the dojot. This module is
called [Dojot Provider](./dojot-provider) and has the role of managing the
creation of tenants (Realms), applying validations, personalizations and
publishing events on Kafka to notify the other platform's microservices.