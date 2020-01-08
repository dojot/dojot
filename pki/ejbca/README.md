# Dojot EJBCA Nodejs

Security Rest API using EJBCA backend

## Overview

The EJBCA Nodejs service is a PKI  (public key infrastructure) for all dojot micro-services. The EJBCA Nodejs use the [EJBCA](https://www.ejbca.org/) platform for the creation, storage, and distribution of digital certificates for all users/services of Dojot. All the users certificates are stored in a dedicated Postgres database.

## EJBCA Persister

The EJBCA persists all the data created in dojot (clients certificate, CA's p12, etc..) in  database. If the system administrator needs to export the CA information (keys and certificates) he can extract the p12 file from EJBCA with the respectively .p12 password.

## EJBCA Profiles configuration

Profiles of CA/clients can be configured [here](./profiles). With those profiles, the system administrator can configure parameters like certificate expiration and data-base write/read control.

## API Use

The Registration Authority (RA) is automatically created in EJBCA with the respectivelly .p12 file (check [here](config_ra.sh) for more configuration information). The RA communicates to EJBCA backend using SOAP. The client of dojot can communicate to EJBCA Nodejs service using the REST API. For more information about the endpoints check [here](https://dojot.github.io/ejbca-rest/apiary_latest.html).

# **Configuration**

## **Environment Variables**

The environment variables can be configured in the .yaml file of the service.

Key                      | Purpose                                                             | Default Value   | Valid Values   |
------------------------ | ------------------------------------------------------------------- | --------------- | -------------- |
DATABASE_JDBC_URL                 | database URL used by EJBCA                   | jdbc:h2 | database url |
EJBCA_PASS                 | password for RA .p12                 | secret | string |
EJBCA_WSDL_ADDR                 | address for ejbca wsdl              | https://localhost:8443/ejbca/ejbcaws/ejbcaws?wsdl | URL |
EJBCA_CA_CRT_DIR                 | path of EJBCA certificate              | /opt/p12/ca.crt | path |
EJBCA_VA_P12_DIR                 | path of RA .p12 file              | /opt/p12/soap_client.p12 | path |
EJBCA_PORT                 | port for ejbca service              | 5583 | port |
EJBCA_API_URL               | URL for ejbca api service doc              | https://dojot.github.io/ejbca-rest/apiary_latest.html | URL |
KAFKA_HOSTS                 | kafka host address             | kafka-server:9092 | address |
AUTH_URL                 | Address of the auth service                                         | http://auth:5000| hostname/IP    |
DATA_BROKER              | Address of the data broker                                          | data-broker:80  | hostname/IP    |
DATA_BROKER_PORT         | Port of the data broker                                             | 80              | integer        |
DATA_BROKER_CONN_RETRIES | How many time data broker tries to reconnect when fails             | 10              | integer        |


# **Issues and help**

If you found a problem or need help, leave an issue in the main [Dojot repository](https://github.com/dojot/dojot) and we will help you!