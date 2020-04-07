# microservice-sdk

This nodejs module aims to help developers to build microservices in the dojot's context.
It gives utility features that most of the dojot microservices needs to implement.

__NOTE THAT__ this module does not intend to be a generic microservice SDK, but a
library scoped to attend the dojot microservices necessities.

## Examples

You can find out how to use each feature in the `examples` directory.

### BackPressure

Before building, check examples/back-pressure/config.js for necessary settings.
You can build a docker image for the example by running:

```
sudo docker build -t sdk-backpressure:example -f examples/back-pressure/Dockerfile  .
```

### Producer

Before building, check examples/producer/config.js for necessary settings.
You can build a docker image for the example by running:

```
sudo docker build -t sdk-producer:example -f examples/producer/Dockerfile  .
```