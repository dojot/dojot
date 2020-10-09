# Example using **Cert-sidecar**

This example shows how to use **cert-sidecar** with a simple https client-server with mutual authentication and deployment in docker-compose and kubernetes.

The main idea here is if the **cert-sidecar** for the server or client is unhealthy checking by the `/ live` resource that responds liveness from the **cert-sidecar**, the server or client should try autoheal (automatic recovery), trying to restart because the files have been deleted and the service should not continue to run as if nothing happened (remembering that there are several attempts before something as drastic as that).

__ATTENTION__  This example is not intended to be a rule, configure the deployment in the way that best matches your case.

__NOTE THAT__ In this example, we are not using CRL and Cron.

## Docker-compose

This example was tested with docker-compose 1.27 and Docker CE 19.03.4.

To run this the docker-compose example, type:

```sh
docker-compose up
```

## Kubernetes

This example was tested with Kubeadm 1.17 and Docker CE 19.03.4.

To run this the kubernetes example, type:

```sh
kubectl apply  -f k8s
```

__NOTE THAT__ When using the kubernetes example, it may be necessary to build the docker images for customer and service.