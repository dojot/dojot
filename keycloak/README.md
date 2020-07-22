# Keycloak

Keycloak is an Open Source Identity and Access Management solution for modern Applications and Services, for more details about it visit [Keycloak oficial repository](https://github.com/keycloak/keycloak)

## Keycloak inside dojot
The Keycloak was adopt by dojot in order to centralize the access control and offer SSO (Single sing-on)

### Keycloak and Kafka
The dojot needs publish messages in Kafka after tenant creation and removal events, as this feature is not offered natively by Keycloak, the dojot team reach this target:

- Adopting and customizing [keycloak-kafka](https://github.com/SnuK87/keycloak-kafka) SPI (Service Provider Interface)
- Customizing Keycloak services module to trigger admin events for tenants's CRUD operations (It can be removed when newer versions of Keycloak implement it natively, to go further in this thread visit [KEYCLOAK-14313](https://issues.redhat.com/browse/KEYCLOAK-14313)) 

### HTTPS
It's highly recommended to communicate with Keycloak over HTTPS when it is not on the same network as API gateway, visit the session **Setting up TLS(SSL)** in [Keycloak Docker image page](https://hub.docker.com/r/jboss/keycloak/) for more details.
