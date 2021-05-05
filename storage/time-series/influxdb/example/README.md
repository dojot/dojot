# Example

## Influxdb initial settings

In this example, a script is executed for the initial settings of influxdb (creation of organization, user, password, token, ...). The `influxdb-setup` service in`docker-compose.yml` is responsible for running the `init-influx.sh` script that creates such initial configurations.

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

`eyJhbGciOiJSUzI1NiIsInR5cCIgOiAiSldUIiwia2lkIiA6ICJlTkZWZmJCa0RiMlNpTW1YT3ZzS2oxUjBpdU9Qc3dlZWc4N2RxbEYxeVg0In0.eyJleHAiOjE2MTg4NjcyODEsImlhdCI6MTYxODg2Njk4MSwiYXV0aF90aW1lIjoxNjE4ODY2OTgwLCJqdGkiOiI4ZTdjZjRkZC1lNzgyLTRiNDAtOTNlMi0wNjMzOGMwYTE2MWYiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwMDAvYXV0aC9yZWFsbXMvYWRtaW4iLCJhdWQiOiJhY2NvdW50Iiwic3ViIjoiNjliNDNjZGQtMDE3ZS00MzU3LWIyNzYtZmU5MTY0YjFmMjRjIiwidHlwIjoiQmVhcmVyIiwiYXpwIjoiZ3VpIiwic2Vzc2lvbl9zdGF0ZSI6ImYwMDlhYzQ4LTgwNzItNGJhMS05Yzg5LTM5MjA1MDI4MmJlMSIsImFjciI6IjEiLCJyZWFsbV9hY2Nlc3MiOnsicm9sZXMiOlsib2ZmbGluZV9hY2Nlc3MiLCJhZG1pbiIsInVtYV9hdXRob3JpemF0aW9uIiwidXNlciJdfSwicmVzb3VyY2VfYWNjZXNzIjp7ImFjY291bnQiOnsicm9sZXMiOlsibWFuYWdlLWFjY291bnQiLCJtYW5hZ2UtYWNjb3VudC1saW5rcyIsInZpZXctcHJvZmlsZSJdfX0sInNjb3BlIjoib3BlbmlkIGVtYWlsIHByb2ZpbGUiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwicHJlZmVycmVkX3VzZXJuYW1lIjoiYWRtaW4ifQ.Mt267jx_jDouq5Q5Mfi3J3Qrxr5se6vV42sWamjlGgVPfdHAJec3d0bIXXJquOIpaWfHVM95Al2X8UkHvpmOfQ838t9DChpZNYG1rOXKFRu0lLuyBlVk5ADahMpY-OhnZ-1DGw-cclXAy2Aa1leSZlL49yKQ1naDZspMfA11gQI9Emy_OmG58JuZIo9txDVXvfgAoI3zHuIaKgv9aib11mzbQtTTmAqb9XZu4HMyWawAkMzst6J6SbeOeYXBwG4cUZWH2r3sD0ykss4nYguHhK6f5nVeloEX0Oht2hOYoL6p8gyVp5pmc9mTzql78N1mLx2TLoso6NL6AoU2Tvbf5w`

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
