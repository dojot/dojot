# Kafka2Ftp

## Service of forwarding messages to the FTP server

The kafka2ftp service provides a connector solution for forwarding messages from Apache Kafka to FTP servers.

It subscribes to the topic *tenant*.dojot.ftp (*tenant* is defined in the environment variable) where in which messages are produced with information about the file name, encoding format and file content. These messages are processed by the service and sent to the corresponding FTP server.  Example of message received by this service below:

```json
{
  "metadata": {
     "msgId": "33846252-659f-42cc-8831-e2ccb923a702",
     "ts": 1571858674,
     "service": "ServiceA",
     "contentType": "application/vnd.dojot.ftp+json"
  },
  "data": {
      "filename": "filename.jpg",
      "encoding": "base64",
      "content": "..."
   }
}

```

Where the keys above are:

Key             | Purpose
--------------- | --------------------------------------------------------
msgId           | Value of type uuidv4 used to uniquely identify the message in the context of dojot.
ts              | Timestamp in Unix Timestamp format from the moment the message was produced.
service         | Name of the service that generated the message.
contentType     | Type identifier of the content of the data attribute.
filename        | Name of the file to be sent to the FTP server.
encoding        | Encoding the contents of the file. Valid values are: ascii, base64, hex, utf16le, utf8 and binary.
content         | File contents.

## **Environment variables**

Key                        | Purpose                                                  | Default Value      | Valid Values |
-------------------------- | -------------------------------------------------------- | ---------------    | -----------  |
KAFKA_GROUP_ID             | The Kafka consumer group ID to be used                   | kafka2ftp          | String       |
KAFKA_HOSTS                | Addresses of the kafka brokers separated by a comma      | kafka:9092         | hostname/IP  |
LOG_LEVEL                  | logger level                                             | info               | debug, error, warning, info  |
FTP_TENANT                 | Tenant is a context identifier into dojot. This defines the topic that the service will subscribes *tenant*.dojot.ftp                  | admin               | String  |
FTP_HOST                   | Server FTP host                                          | localhost          | String                       |
FTP_PORT                   | Server FTP port                                          | 21                 | Natural number                      |
FTP_FTPS                   | Explicit FTPS over TLS                                   | false              | String (true or false)                |
FTP_USER                   | Server FTP Username                                      | anonymous          | String                       |
FTP_PASSWORD               | Server FTP Password                                      | guest              | String                       |
FTP_REMOTE_DIR             | Path where files will be sent (This path must exist previously)   | /                  | Path                         |
MAX_CONCURRENT_CONNECTIONS | The maximum number of FTP connections at the same time.                                            | 10                 | Natural number               |
RETRIES      | The maximum amount of times to retry the upload          | 13                 | Natural number               |


Note: Number of RETRIES resend in an "exponential backoff" strategy. When RETRIES=13 using an exponential backoff strategy, this means the last attempt is made after 2h 16m 31 sec. Total time in ms can be calculate with *Sum[1000*2^k, {k, 0, n-1}]* where *n=retries*.