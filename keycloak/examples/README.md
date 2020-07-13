# Docker-compose example

This example makes available an enviroment with Keycloak integrated with Kafka.

To run this example, type:
```
docker-compose up
```

To explore Keycloak GUI (graphical user interface) access http://localhost:8080 and log in with user ```admin``` and password ```admin```

Keycloak also makes available a set of APIs to manage users, groups, clients, permissions ..., visit [Keycloak Admin REST API
](https://www.keycloak.org/docs-api/5.0/rest-api/index.html) for more details.

## Kafka integration
To test Kafka integration follow these instructions:

- Get an admin token:
```
ADMIN_JWT=$(curl --location --request POST localhost:8080/auth/realms/master/protocol/openid-connect/token \
--data-urlencode 'username=admin' \
--data-urlencode 'password=admin' \
--data-urlencode 'client_id=admin-cli' \
--data-urlencode 'grant_type=password' 2>/dev/null | jq -r '.access_token')
``` 

- Create a new tenant:
```
curl --location --request POST 'http://localhost:8080/auth/admin/realms' -H "Authorization: Bearer ${ADMIN_JWT}" -H 'Content-Type:application/json' \
--data-raw ' {
   "realm": "myTenant",
   "displayName": "myTenant",
   "enabled": true
 }'
``` 

- Delete tenant:
```
curl --location --request DELETE 'http://localhost:8080/auth/admin/realms/myTenant' -H "Authorization: Bearer ${ADMIN_JWT}" -H 'Content-Type:application/json'
 ```
 
Now check ```dojot-management.dojot.tenancy``` topic to verify if the messages was sent:
```
docker container exec -it dojot_keycloak_kafka_1 bash -c 'unset JMX_PORT && kafka-console-consumer.sh --bootstrap-server localhost:9092 --topic dojot-management.dojot.tenancy --from-beginning'
```

The following result is expected:
```
{"type":"CREATE","tenant":"myTenant"}
{"type":"DELETE","tenant":"myTenant"}
```
 
