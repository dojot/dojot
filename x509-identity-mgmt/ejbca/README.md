# EJBCA server customization

The scripts in this directory are designed to customize the [EJBCA server](https://www.ejbca.org/) to meet the needs of the dojot platform.
Listed below are the environment variables that can be used to refine component customization.


## Enable the settings on the first run of the container

For the settings related to the EJBCA server to take effect, it is necessary to enable a specific environment variable, otherwise the EJBCA will perform the original settings of its docker image (it will not go through the customized settings for the dojot platform). It is important to emphasize that these configurations only need to be performed only once per container and will be valid for the entire existence of the container. Once the container has been configured, it is not necessary to run the configuration scripts again, this is reflected in the container's initialization performance. If you use a configuration and deployment management tool, the manipulation of this variable can become interesting, otherwise, just set it to true and keep it that way (remembering that the initialization of the container will always be slower).

| Variable | Description |
|----------|-------------|
| EJBCA_PERFORM_DOJOT_SETUP | Set to "true" when running the container at least the first time the container is run. |


## Settings to enable the super admin user to access the EJBCA

During the EJBCA configuration process at container startup, it is possible to enable the creation of the super admin user. This option is interesting in a development environment, but not advisable in production environments for security reasons.

| Variable | Description |
|----------|-------------|
| EJBCA_ADMIN_USER | This variable set to true causes the super admin user to be generated, so it is possible to access the EJBCA in its entirety. By default, this variable is not defined, but in a development environment, it can be set to "true". |
| EJBCA_ADMIN_USERNAME | Name of the End Entity created in the EJBCA.  <br>**Default**: admin |
| EJBCA_ADMIN_COMMONNAME | Value to be added in the SubjectDN (Common Name) field of super admin certificate. <br>**Default**: Super Admin |
| EJBCA_ENROLLMENT_INFO_DIR | Directory where the enrollment-info file for the super admin user will be generated. <br>**Default**: /mnt/persistent/private <br><br>**Note**: The file name is made up of ${EJBCA_ADMIN_USERNAME} + '-enrollment-info.txt' |


## Settings for external access to the EJBCA

In development environments, it is interesting to be able to access the EJBCA server to check the settings through a friendly graphical interface.
**Note:** To access EJBCA, you must also enable the *super admin* user and be in the hands of the [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) file and configure it in the browser.

| Variable | Description |
|----------|-------------|
|EJBCA_EXTERNAL_ACCESS| Set the value to "true" so that the EJBCA server port is accessible on the container ip. By default, this variable is not defined and in production environments it is advisable to remain so. <br>Once enabled, it is possible to access the EJBCA through the URL: *https:[container IP]:8443/ejbca* <br>Another way is to map port 8443 of the container to your localhost via the docker parameter. |


## Settings for a correct connection over TLS to the EJBCA server

EJBCA makes use of the `hostname` command of linux to generate the certificate of the application server that it runs on. When the container does not have a defined hostname and domainname, a random value is assumed and the EJBCA generates a self-signed certificate for the Wildfly server. This impacts the communication of the Node.js application that runs in the same container and has to communicate with the EJBCA via the web service in TLS.

These settings must be made directly when executing the container via Docker:
| docker run | value |
|----------|-------------|
| \-\-hostname | 'x509-identity-mgmt' |
| \-\-domainname | 'dojot.iot' |

This is necessary because the EJBCA server expects a specific value for the `hostname`.

 - See how to use it in [docker-compose](https://docs.docker.com/compose/compose-file/#domainname-hostname-ipc-mac_address-privileged-read_only-shm_size-stdin_open-tty-user-working_dir);
 - See how to use it in [Kubernetes](https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pods-hostname-and-subdomain-fields);


#### Common configurations between the x.509 Identity Service client and the EJBCA server

The access to EJBCA's functionalities is done through the certificate that it issues to client applications, in this way, it is also possible to establish communication over TLS. Although the x.509 identity service runs in the same container as the EJBCA server, a certificate is still required to serve as an access token to EJBCA functionalities.

| Variable | Description |
|----------|-------------|
| EJBCA_CLIENT_USERNAME | This value is used to create an End Entity in the EJBCA, as well as to name the file [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) used by the client application to communicate with the EJBCA server via SOAP. <br>**Default**: ejbcaclient |
| EJBCA_CLIENT_COMMONNAME | Value to be added in the SubjectDN (Common Name) field of client application certificate. <br>**Default**: EJBCA Client Application |
| EJBCA_TLS_CLIENT_DIR | Directory where the PKCS#12 file will be generated so that the client application can establish communication with the EJBCA server. <br>**Default**: /opt/tls |


## Configuration for Profiles

| Variable | Description |
|----------|-------------|
| **Certificate Profiles** | |
| EJBCA_DEVICES_CA_CERT_PROFILE | Name of the certificate profile of the root CA used to sign certificates for IoT devices. <br>**Default**: X509IdentitiesCA |
| EJBCA_SERVICES_CA_CERT_PROFILE | Name of the certificate profile of the root CA used to sign certificates for dojot services. <br>**Default**: ServicesCA |
| EJBCA_APP_SERVER_CERT_PROFILE | Name of the profile used to generate the EJBCA server certificate. Required to establish communication over TLS between the *dojot x.509 Identities* service and the *EJBCA*. <br>**Default**: ApplicationServerEJBCA |
| EJBCA_DEVICES_CERT_PROFILE | Name of the profile used to generate certificates for IoT devices. <br>**Default**: X509Identity |
| EJBCA_SERVICES_CERT_PROFILE | Name of the profile used to generate certificates for the other services on the dojot platform. <br>**Default**: Service |
| **End Entities Profiles** | |
| EJBCA_APP_SERVER_ENTITY_PROFILE | Name of the profile of the End Entity that represents the EJBCA server. <br>**Default**: ApplicationServerEJBCA |
| EJBCA_DEVICES_ENTITY_PROFILE | Name of the profile of the End Entity that represents the IoT devices. <br>**Default**: X509Identity |
| EJBCA_SERVICES_ENTITY_PROFILE | Name of the profile of the End Entity that represents the other services of the dojot platform. <br>**Default**: Service |


## Configuration for CAs

| Variable | Description |
|----------|-------------|
| EJBCA_PKI_VALIDITY | Validity of the platform's Public Key Infrastructure (Validity of the root CA's certificates). After expiration, all certificates issued by the platform will become invalid, so it is a good idea to keep this validity period very long. <br>**Default**: "30y" (about 30 years. See [Temporal Definition](#how-to-define-relative-time-in-environment-variables)) <br><br>Note: CA certificates are automatically renewed, but the certificate generation process for IoT devices and platform services must be performed manually by the administrator. This is a feature of PKI and not exactly the dojot platform. |
| EJBCA_DISTNAME_O | Value to be added in the SubjectDN (Organization) field of CA certificates. <br>**Default**: dojot IoT Platform |
| EJBCA_DISTNAME_OU | Value to be added in the SubjectDN (Organization Unit) field of CA certificates. <br>**Default**: Certificate Issuer |
| EJBCA_DEVICES_CA | Name of the CA that signs certificates for IoT devices. This value is also added in the SubjectDN (Common Name) field of this CA's certificate <br>**Default**: X509 Identity CA |
| EJBCA_SERVICES_CA | Name of the CA that signs certificates for dojot platform services. This value is also added in the SubjectDN (Common Name) field of this CA's certificate <br>**Default**: Services CA |


## Configuration for CRL Period

For the value of these variables see [Temporal Definition](#how-to-define-relative-time-in-environment-variables).

| Variable | Description |
|----------|-------------|
| **Devices CA** |  |
| EJBCA_CRL_EXPIRE_PERIOD_DEVICES_CA | The validity period for generated CRLs. If set to for example 24h, the next update for a generated CRL will be the issue time + 24 hours <br>**Default**: 1d |
| EJBCA_CRL_ISSUE_INTERVAL_DEVICES_CA | A fixed interval when CRLs are issued. If set to for example 1h, a new CRL will be issued every hour, even though the old one is still valid for another 23 hours, corresponding to a CRL Overlap Time of 23h. The default value here is 0, which means that a new CRL will be issued when the old one is about to expire. <br>**Default**: 0m |
| EJBCA_CRL_OVERLAP_TIME_DEVICES_CA | The new CRL is generated this amount of time before the old CRL expires. The default value is 10 minutes, meaning that if the CRL Expire period is 24 hours, a new CRL will be issued after 23h50m. <br>**Default**: 10m |
| EJBCA_DELTA_CRL_PERIOD_DEVICES_CA | The validity period for generated delta CRLs if delta CRLs are issued. Delta CRLs are only issued if this period is larger than 0. <br>**Default**: 1h |
| **Services CA** |  |
| EJBCA_CRL_EXPIRE_PERIOD_SERVICES_CA | The validity period for generated CRLs. If set to for example 24h, the next update for a generated CRL will be the issue time + 24 hours <br>**Default**: 1d |
| EJBCA_CRL_ISSUE_INTERVAL_SERVICES_CA | A fixed interval when CRLs are issued. If set to for example 1h, a new CRL will be issued every hour, even though the old one is still valid for another 23 hours, corresponding to a CRL Overlap Time of 23h. The default value here is 0, which means that a new CRL will be issued when the old one is about to expire. <br>**Default**: 0m |
| EJBCA_CRL_OVERLAP_TIME_SERVICES_CA | The new CRL is generated this amount of time before the old CRL expires. The default value is 10 minutes, meaning that if the CRL Expire period is 24 hours, a new CRL will be issued after 23h50m. <br>**Default**: 10m |
| EJBCA_DELTA_CRL_PERIOD_SERVICES_CA | The validity period for generated delta CRLs if delta CRLs are issued. Delta CRLs are only issued if this period is larger than 0. <br>**Default**: 1h |


## Configuration for EJBCA Services

| Variable | Description |
|----------|-------------|
| **Updater Service** | The CRL Updater checks if any of the configured CAs need a new CRL and generates it if necessary. The worker has no additional settings and only supports the periodical interval. <br>For more details, see [EJBCA Docs](https://doc.primekey.com/ejbca6152/ejbca-operations/ejbca-concept-guide/services#Services-CRL_Updater_Service). |
| EJBCA_CRL_UPDATER_SERVICE_NAME | The service name for the EJBCA <br>**Default**: CRL Updater |
| EJBCA_CRL_UPDATER_SERVICE_INTERVAL_VALUE | Times in times when the service will be performed by the EJBCA. <br>**Default**: 30 |
| EJBCA_CRL_UPDATER_SERVICE_INTERVAL_UNIT | Times in times when the service will be performed by the EJBCA. <br>**Default**: MINUTES |
| **Renewer Service** | The renew CA service can be used to automatically renew CAs that are about to expire. <br>For more details, see [EJBCA Docs](https://doc.primekey.com/ejbca6152/ejbca-operations/ejbca-concept-guide/services#Services-RenewCAService). |
| EJBCA_CA_RENEWER_SERVICE_NAME | The service name for the EJBCA <br>**Default**: CAs Renewer |
| EJBCA_CA_RENEWER_SERVICE_INTERVAL_VALUE | Times in times when the service will be performed by the EJBCA. <br>**Default**: 1 |
| EJBCA_CA_RENEWER_SERVICE_INTERVAL_UNIT | Times in times when the service will be performed by the EJBCA. <br>**Default**: DAYS |
| EJBCA_CA_RENEWER_SERVICE_TIME_BEFORE_EXP | Time before CA expires to renew: Amount of time before the CA actually expires, that the service should renew the CA. <br>**Default**: 10 |
| EJBCA_CA_RENEWER_SERVICE_TIME_BEFORE_EXP_UNIT | Time before CA expires to renew: Amount of time before the CA actually expires, that the service should renew the CA. <br>**Default**: DAYS |


## Lock configuration to synchronize redundant containers

If it is necessary to run two instances of this container (high availability scenario), it is necessary to take extra care with the synchronization of the configuration scripts between the containers, since the EJBCA database is centralized.
There is a simple locking mechanism used to prevent two containers from executing the settings at the same time, for this purpose a lock file is created on the volume shared between the containers.

The lock file is created at: `/mnt/persistent/.lock`

The lock file remains in existence until the settings of the container that created the lock are completed or until it reaches its timeout (to avoid deadlock), whichever comes first.

The lock timeout can be changed by an environment variable:
| docker run | value |
|----------|-------------|
| EJBCA_LOCK_FILE_TIMEOUT | Lock file timeout (in minutes). <br>**Default**: 10 |


## How to define Relative Time in environment variables

A relative time is represented in the form: "\*y \*mo \*d \*h \*m \*s", where (\*) can be any positive integer value in terms of *years*, *months*, *days*, *hours*, and *seconds*.
