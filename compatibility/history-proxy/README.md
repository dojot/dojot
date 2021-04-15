# History Proxy

The History Proxy imitates the legacy History service, allowing old applications to be compliant with InfluxDb, which is used in newer versions of Dojot. 

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

The configuration used in service could be received from 2 ways: The environment variables and the configuration file. It's possible select the configuration file via the HISTORYPROXY_CONFIG_FILE variable. Its default value is production.conf. 

Before running the History Proxy service within your environment, make sure you configure the environment variables to match your needs.


| Environment variable           | Description                         |
| ------------------------       | ----------------                    |
| HISTORYPROXY_HISTORY_URL       | URL to access the History service   |
| HISTORYPROXY_RETRIEVER_URL     | URL to access the Retriever service |
| HISTORYPROXY_PORT              | Port for starting the service       |
 
## Configuration

```shell
npm install
```
## Available Scripts

In the project directory, you can run:

### To run

```shell
npm start
```

### To test

```shell
npm test
```


# Docker

The following command creates a docker image to be a proxy for InfluxDB, mocking the History responses.

```shell
docker build -f Dockerfile -t [username]/history-proxy:[tag] .
```

To run the created image:

```shell
docker run -d [-n name] history-proxy:[tag]
```