# GUI Proxy [![CodeFactor](https://www.codefactor.io/repository/github/cfrancisco/forwards-history-proxy/badge)](https://www.codefactor.io/repository/github/cfrancisco/forwards-history-proxy) [![codecov](https://codecov.io/gh/cfrancisco/forwards-history-proxy/branch/development/graph/badge.svg)](https://codecov.io/gh/cfrancisco/forwards-history-proxy) [![License badge](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


The GUI Proxy allows the Dojot GUI v1 to be compliant with InfluxDb, which is used in newer versions of Dojot. To archive this 


## Use cases
The service was tested (i.e., yielded the same results and patterns) in the following scenarios:
* Requesting one attribute, even when it’s not mapped in dojot’s template;
* Requesting with dateFrom, dateTo, lastN or firstN and any combination of these parameters;
* Returning the same syntax for all type of attributes, including JSON;
* When the attribute is not provided, the service sends back all attributes. 
* If the request does not contain a valid attribute or has empty data, then the service responds HTTP 404  (not found attribute), as it is in the original History.
* If the request didn’t match in a route, sends back HTTP 404. 
* Apply the two converter patterns (Using one for 1 attribute and the other for all attributes requests); 

## Limitations or Drawbacks
* When receiving multiple attributes, will be only used the first one. 	
* If the device requested doesn't exist, the History endpoint returns HTTP 404 response with a message ‘device not found’. Now, it also gives HTTP 404 response, but with the message ‘attr not found’. 

## Environment variables

The environment variables are used in configuration parameters 

| Environment variable     | Description                         |
| ------------------------ | ----------------                    |
| HISTORY_URL              | URL to access the History service   |
| RETRIEVER_URL            | URL to access the Retriever service |
| PORT                     | Port for starting the service       |
 
## Configuration

```shell
yarn install
```
## Available Scripts

In the project directory, you can run:

## To run

```shell
yarn start
```

## To test

```shell
yarn test
```


# Docker

The following command creates a docker image to be a proxy for InfluxDB, mocking the History responses.

```
It has three optional arguments:
 DOJOT_VERSION: Set the GUI version
 HISTORY_URL: URL to access the History service
 RETRIEVER_URL: URL to access the Retriever service
 PORT: Service's port
```

```shell
docker build -f Dockerfile -t [tag name] --build-arg DOJOT_VERSION=[version] --build-arg RETRIEVER_URL=[retriever url] --build-arg HISTORY_URL=[history url] --build-arg PORT=[port] .
```

To run the created image:

```shell
docker run -d [-n name] <tag name>
```