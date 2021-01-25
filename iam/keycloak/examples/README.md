# Keycloak + Kafka Provider example

This example demonstrates that it is possible to use the _Kafka Provider_ to
publish Keycloak _realm creation and removal events_ to Kafka.

To run this example, enter the following command in the same directory as
`docker-compose.yml`:

```bash
docker-compose up --build
```

This example will run a Keycloak container already configured with the
_Kafka Provider_, to access the Keycloak **administration console**, open the
browser and enter the following address at the URL: `http://localhost:8080/auth/`.

Access the _administration console_ link and log in with username = `admin` and
password = `admin`.

Once inside the _administration console_, we must create a new Realm, for that,
locate in the upper left corner of the screen a label with the name **Master**,
when placing the cursor on the label, a button will be presented with the option
to _add a new realm_, click this button and then enter a name for the new realm
to be created.

The operation is successfully executed and we can already see that a realm
creation event has been published in Kafka.
To confirm this, we can access Kafka through [Kafdrop](https://github.com/obsidiandynamics/kafdrop),
enter the following address at the URL: `http://localhost:9090/` and navigate to
the topic: `dojot-management.dojot.tenancy`.

Click the button to view the _topic's messages_, then click the button to filter
the messages. The realm creation event must be displayed.

The realm removal process follows in the same way. To remove a Realm in the
Keycloak, first you need to access the main screen of the Realm and next to the
_name_ (title) of the realm, there will be a _trash can_ icon, when you click on
this icon and confirm the execution of the operation, the realm will be removed
and an event will be published on Kafka.
Again, use _Kafdrop_ to view the topic's messages (you may need to update the page).