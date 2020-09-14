# Keycloak

Keycloak is an Open Source Identity and Access Management solution for modern Applications and Services, for more details about it visit [Keycloak oficial repository](https://github.com/keycloak/keycloak)

## Keycloak inside dojot
The Keycloak was adopted by dojot in order to centralize the access control and offer SSO (Single sing-on)

### Keycloak and Kafka
The dojot microservices needs to be notified through Kafka when a tenant is created or removed, but Keycloack doesn't offer it natively. This feature was achieved by dojot team:

- Adopting and customizing [keycloak-kafka](https://github.com/SnuK87/keycloak-kafka) SPI (Service Provider Interface)
- Customizing Keycloak services module to trigger admin events for tenants's CRUD operations (It can be removed when newer versions of Keycloak implement it natively, to go further in this thread visit [KEYCLOAK-14313](https://issues.redhat.com/browse/KEYCLOAK-14313)) 

### HTTPS
It's highly recommended to communicate with Keycloak over HTTPS, visit the session **Setting up TLS(SSL)** in [Keycloak Docker image page](https://hub.docker.com/r/jboss/keycloak/) for more details.
