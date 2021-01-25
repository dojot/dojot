# Kafka Provider (Keycloak SPI implementation)

The _Kafka Provider_ is a type of "plugin" created for the [Keycloak](https://www.keycloak.org/)
to listen to events and publish them on [Kafka](https://kafka.apache.org/).
In this way the other _dojot_ microservices can be notified about the things
that happen in the Keycloak, for example, when a _tenant_ (known as _Realm_ in
the Keycloak) is created or removed.

To create a custom [Service Provider](https://www.keycloak.org/docs/latest/server_development/#_providers),
you need to understand how Keycloak works and how to capture the events it triggers.

First, it is necessary to keep in mind that Keycloak is a modular software based
on the Java EE [Wildfly Application Server](https://www.wildfly.org/),
therefore, the Keycloak are packaged following the [Wildfly implementation](https://jboss-modules.github.io/jboss-modules/manual/)
of a _modular (non-hierarchical) class loading and execution environment_ for Java.

Keycloak modules are built based on the concept of [Service Provider Interface](https://en.wikipedia.org/wiki/Service_provider_interface) (SPI),
whose java offers [native support](https://docs.oracle.com/javase/tutorial/sound/SPI-intro.html).

Communication between the Keycloak modules is done through _Service Providers_
(SPI implementations), which in turn trigger typical events for each module. The
Keycloak _core_ is responsible for correctly loading all modules and registering
their event listeners.

In order to be able to capture the events triggered by the Keycloak modules, it
is necessary to implement an SPI and deploy it to Wildfly so that Keycloak knows
how to load it.

In summary, this README is intended to explain how the _Kafka Provider_ is
compiled and packaged according th the modular structure of Wildfly. After being
packaged correctly, you need to move it into the Wildfly _modules_ directory and
finally, use the [Wildfly CLI](https://docs.jboss.org/author/display/WFLY/Command%20Line%20Interface.html)
to register the Provider so that Keycloak can load it as one of its modules.

Last but not least, we will have a brief explanation of how (the _hotspot_) our
_Service Provider_ is notified by Keycloak when its _internal providers_ trigger
events.


## Compilation, packaging and deployment

_Kafka Provider_ uses the [Maven](https://docs.jboss.org/author/display/WFLY/Command%20Line%20Interface.html)
tool to manage its dependencies, compilation and packaging, so, just run the
following command to have everything ready to be deployed as a _Wildfly module_.

```bash
# In the project's root directory, where the pom.xml file is located, run the following command:
mvn clean package
```

For the project to be packaged in the modular structure of Wildfly, the
[JBoss Modules Maven Plugin](https://www.smartics.eu/confluence/display/SJBMMP/smartics+JBoss+Modules+Maven+Plugin)
is used, which in turn is responsible for creating the module that represents
our _Kafka Provider_ and also creates the modules with the libraries that the
project depends on and that does not are provided by Wildfly/Keycloak.
To better understand how this plugin works, you have
[this very good article](https://www.smartics.eu//confluence/display/BLOG/2013/10/18/Maven+Plugin+to+generate+a+Modules+Directory+for+JBoss+AS+7)
about it.

This will cause the project's dependencies to be resolved, the code will be
compiled and the necessary structure to register the component as a
_Wildfly module_ will be generated.

The result of executing the command is the `./target` directory. What really
matters to us is the content of directory `./target/jboss-modules`, where the
modular structure to be deployed in Wildlfy is found.

The next step is to move the contents of the `./target/jboss-modules` directory
into Wildfly (in which the keycloak [is distributed](https://www.keycloak.org/downloads)),
like this:

```bash
# In the project's root directory, run the following command:
cp -a ./target/jboss-modules/. $JBOSS_HOME/modules/system/layers/keycloak/
```

Now we need to [configure Keycloak](https://www.keycloak.org/docs/latest/server_installation/#_config_spi_providers)
to recognize our _service provider_ as one of its modules, that is, to
[load it](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/ServiceLoader.html)
as an SPI.

We could perform the configuration by editing the correct file manually, but the
ideal is to do this through the CLI of Wildlfy, so first of all we will access it.

To start the [WildFly CLI](https://www.keycloak.org/docs/latest/server_installation/#_start_cli),
you need to run `jboss-cli`:

```bash
# The JBOSS_HOME environment variable symbolizes the
# Wildlfy root directory where Keycloak is installed
$JBOSS_HOME/bin/jboss-cli.sh
```

This will bring you to a prompt like this:

```
[disconnected /]
```

To register our _service provider_, Wildfly does not need to be running, so we
will use the CLI in [embedded mode](https://www.keycloak.org/docs/latest/server_installation/#cli-embedded-mode):

> Note: Ignore comment lines as they are not accepted by the Wildfly CLI.

```bash
# The file we are going to configure is the 'standalone-ha.xml'
[disconnected /] embed-server --server-config=standalone-ha.xml

# This prompt tells us that we are in embedded mode
[standalone@embedded /]
```

With the CLI in embedded mode, we will execute the following commands:

```bash
# This command tells Keycloak that our Service Provider must be loaded as a Wildfly Module.
[standalone@embedded /] /subsystem=keycloak-server/:list-add(name=providers, value="module:com.github.dojot.keycloak.providers.kafka-provider:dojot")

# This command adds a new SPI config to the Keycloak.
[standalone@embedded /] /subsystem=keycloak-server/spi=kafka:add()

# This command tells Keycloak that the SPI config is enabled.
[standalone@embedded /] /subsystem=keycloak-server/spi=kafka/provider=dojot:add(enabled=true)

# This command tells our Service Provider where to connect to Kafka.
[standalone@embedded /] /subsystem=keycloak-server/spi=kafka/provider=dojot:write-attribute(name=properties.servers,value=${env.KAFKA_SERVERS})

# This command tells Kafka which is the 'Client ID' of our Service Provider.
[standalone@embedded /] /subsystem=keycloak-server/spi=kafka/provider=dojot:write-attribute(name=properties.clientId,value=${env.KAFKA_CLIENT_ID})

# This command tells our Service Provider which topic in Kafka should be used to publish data.
[standalone@embedded /] /subsystem=keycloak-server/spi=kafka/provider=dojot:write-attribute(name=properties.topic,value=${env.KAFKA_TOPIC})
```

Finally, just exit the CLI and put the Keycloak to work. If you want to include
these settings in a [script file](https://www.keycloak.org/docs/latest/server_installation/#cli-scripting)
and run them all at once, do the following.

Create a file called `kafka-provider.cli` with the following content:

```bash
embed-server --server-config=standalone-ha.xml --std-out=echo
batch
/subsystem=keycloak-server/:list-add(name=providers, value="module:com.github.dojot.keycloak.providers.kafka-provider:dojot")
/subsystem=keycloak-server/spi=kafka:add()
/subsystem=keycloak-server/spi=kafka/provider=dojot:add(enabled=true)
/subsystem=keycloak-server/spi=kafka/provider=dojot:write-attribute(name=properties.servers,value=${env.KAFKA_SERVERS})
/subsystem=keycloak-server/spi=kafka/provider=dojot:write-attribute(name=properties.clientId,value=${env.KAFKA_CLIENT_ID})
/subsystem=keycloak-server/spi=kafka/provider=dojot:write-attribute(name=properties.topic,value=${env.KAFKA_TOPIC})
run-batch
stop-embedded-server
```

Execute the script from the command line as follows:

```bash
$JBOSS_HOME/bin/jboss-cli.sh --file=kafka-provider.cli
```

To boot the Keycloak server:

```bash
$JBOSS_HOME/bin/standalone.sh --server-config=standalone-ha.xml
```

## The _hotspot_ where Kafka Provider is triggered

The starting point that connects _Kafka Provider_ to the Keycloak is the
`./src/main/resources/META-INF/services/org.keycloak.provider.Spi` file, inside
it is the full name of the class that implements the SPI interface, so that the
Keycloak can load it through the java [ServiceLoader](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/ServiceLoader.html) class.

After Keycloak loads our `org.keycloak.provider.Spi` implementation (that is,
the `com.github.dojot.keycloak.providers.kafka.KafkaSpi` class), it will know it
has access to the `./src/main/resources/META-INF/services/com.github.dojot.keycloak.providers.kafka.KafkaProviderFactory`
file and load our implementation of the `org.keycloak.provider.ProviderFactory`
interface (that is, the `com.github.dojot.keycloak.providers.kafka.impl.KafkaProviderFactoryImpl`
class).

Keycloak knows that every class that implements the `org.keycloak.provider.ProviderFactory`
interface has an `init()` method, so that's when Keycloak passes the SPI
settings (to connect to Kafka) to our _factory_.
The factory also implements the `postInit()` method tha gives access to an
instance of `org.keycloak.provider.ProviderEventManager`, which is where we must
register our event listeners to trigger our _Kafka Provider_ and let it do the
work it has to do.
Any `RuntimeException` thrown by our _Service Provider_ causes the rollback of
the entire operation.