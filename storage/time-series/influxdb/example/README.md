
# Example

## Simulating dojot kafka messages with InfluxDB

This example simulates dojot kafka messages that are consumed by InfluxDB-Storer and can be accessed by InfluxDB-Retriever via REST.

To run this example, type:

```sh
docker-compose up
```

Then, you can access [http://localhost:3000/tss/v1/api-docs/](http://localhost:3000/tss/v1/api-docs/) in your browser and use the iterative API documentation to use the REST API provided by InfluxDB-Retriever.

To obtain the data entered by kafka you can use these values:

- In *Server variables*  you can provide the following **url**:
  - http://localhost:3000/

- In **Authorized** you can provide the following token:

`eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzZXJ2aWNlIjoidGVuYW50MSIsIm5hbWUiOiJKb2huIERvZSIsImlhdCI6MTUxNjIzOTAyMn0.pmuFC9u1K3oxHlf4nUsjJp1KyRz-JxWN1QS7L5AJMno`

- In **deviceId** you can provide the following device id:
  - 1234

- In **attr** you can provide the following attribute labels:
  - geo
  - string
  - int
  - float
  - bool
  - nulled
  - array
  - obj
  - string_empty

__NOTE THAT__ The kafka data entry script runs infinity by adding the same data set, some with the same date and others with current dates. In influxDB if the date is exactly the same, the previous value is overwritten.
