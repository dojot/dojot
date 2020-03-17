# microservice-sdk

This nodejs module aims to help developers to build microservices in the dojot's context.
It gives utility features that most of the dojot microservices needs to implement.

__NOTE THAT__ this module does not intend to be a generic microservice SDK, but a
library scoped to attend the dojot microservices necessities.

## Examples

You can find out how to use each feature in the `examples` directory.

## BackPressure 

Before building, check examples/backPressure/config.js for necessary settings.
You can build the example docker image by running:

```
sudo docker build -t sdk-backpressure:example -f examples/backPressure/Dockerfile  . 
```