#!/usr/bin/env bash

mkdir -p tmp
readonly LOG_FILE="./tmp/$(basename "$0").log"
success() { echo -e "$(date +%T) \e[32;1m[SUCCESS]\e[0m $*" | tee -a "$LOG_FILE" >&2 ; }
info()    { echo -e "$(date +%T) \e[34m[INFO]\e[0m    $*" | tee -a "$LOG_FILE" >&2 ; }
warning() { echo -e "$(date +%T) \e[33m[WARNING]\e[0m $*" | tee -a "$LOG_FILE" >&2 ; }
error()   { echo -e "$(date +%T) \e[31m[ERROR]\e[0m   $*" | tee -a "$LOG_FILE" >&2 ; }
fatal()   { echo -e "$(date +%T) \e[101m[FATAL]\e[0m   $*" | tee -a "$LOG_FILE" >&2 ; exit 1 ; }

readonly CONTAINER_IP_ADDRESS=$(ip -4 addr show ${DOCKER_NET_INTERFACE:-eth0} | grep -oE '[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}' | sed -e "s/^[[:space:]]*//" | head -n 1)
IP_ADDRESS=${DOCKER_IP_ADDRESS:-${CONTAINER_IP_ADDRESS}}


# Ensure the Erlang node name is set correctly
if env | grep "DOCKER_VERNEMQ_NODENAME" -q; then
    NODE_NAME_PART2=${DOCKER_VERNEMQ_NODENAME}
else
    if [ -n "$DOCKER_VERNEMQ_SWARM" ]; then
        NODE_NAME_PART2=$(hostname -i)
    else
        NODE_NAME_PART2=${IP_ADDRESS}
    fi
fi


if env | grep "DOCKER_VERNEMQ_DISCOVERY_NODE" -q; then
    discovery_node=$DOCKER_VERNEMQ_DISCOVERY_NODE
    if [ -n "$DOCKER_VERNEMQ_SWARM" ]; then
        tmp=''
        while [[ -z "$tmp" ]]; do
            tmp=$(getent hosts tasks.$discovery_node | awk '{print $1}' | head -n 1)
            sleep 1
        done
        discovery_node=$tmp
    fi
    if [ -n "$DOCKER_VERNEMQ_COMPOSE" ]; then
        tmp=''
        while [[ -z "$tmp" ]]; do
            tmp=$(getent hosts $discovery_node | awk '{print $1}' | head -n 1)
            sleep 1
        done
        discovery_node=$tmp
    fi
    sed -i.bak -r "/-eval.+/d" /vernemq/etc/vm.args
    echo "-eval \"vmq_server_cmd:node_join('VerneMQ@$discovery_node')\"" >> /vernemq/etc/vm.args
fi

# If you encounter "SSL certification error (subject name does not match the host name)", you may try to set DOCKER_VERNEMQ_KUBERNETES_INSECURE to "1".
insecure=""
if env | grep "DOCKER_VERNEMQ_KUBERNETES_INSECURE" -q; then
    insecure="--insecure"
fi

if env | grep "DOCKER_VERNEMQ_DISCOVERY_KUBERNETES" -q; then
    DOCKER_VERNEMQ_KUBERNETES_CLUSTER_NAME=${DOCKER_VERNEMQ_KUBERNETES_CLUSTER_NAME:-cluster.local}
    # Let's get the namespace if it isn't set
    DOCKER_VERNEMQ_KUBERNETES_NAMESPACE=${DOCKER_VERNEMQ_KUBERNETES_NAMESPACE:-`cat /var/run/secrets/kubernetes.io/serviceaccount/namespace`}
    # Let's set our nodename correctly
    VERNEMQ_KUBERNETES_SUBDOMAIN=${DOCKER_VERNEMQ_KUBERNETES_SUBDOMAIN:-$(curl -X GET $insecure --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt https://kubernetes.default.svc.$DOCKER_VERNEMQ_KUBERNETES_CLUSTER_NAME/api/v1/namespaces/$DOCKER_VERNEMQ_KUBERNETES_NAMESPACE/pods?labelSelector=$DOCKER_VERNEMQ_KUBERNETES_LABEL_SELECTOR -H "Authorization: Bearer $(cat /var/run/secrets/kubernetes.io/serviceaccount/token)" | jq '.items[0].spec.subdomain' | sed 's/"//g' | tr '\n' '\0')}
    if [ $VERNEMQ_KUBERNETES_SUBDOMAIN == "null" ]; then
        VERNEMQ_KUBERNETES_HOSTNAME=${MY_POD_NAME}.${DOCKER_VERNEMQ_KUBERNETES_NAMESPACE}.svc.${DOCKER_VERNEMQ_KUBERNETES_CLUSTER_NAME}
    else
        VERNEMQ_KUBERNETES_HOSTNAME=${MY_POD_NAME}.${VERNEMQ_KUBERNETES_SUBDOMAIN}.${DOCKER_VERNEMQ_KUBERNETES_NAMESPACE}.svc.${DOCKER_VERNEMQ_KUBERNETES_CLUSTER_NAME}
    fi

    NODE_NAME_PART2=${VERNEMQ_KUBERNETES_HOSTNAME}

    # Hack into K8S DNS resolution (temporarily)
    kube_pod_names=$(curl -X GET $insecure --cacert /var/run/secrets/kubernetes.io/serviceaccount/ca.crt https://kubernetes.default.svc.$DOCKER_VERNEMQ_KUBERNETES_CLUSTER_NAME/api/v1/namespaces/$DOCKER_VERNEMQ_KUBERNETES_NAMESPACE/pods?labelSelector=$DOCKER_VERNEMQ_KUBERNETES_LABEL_SELECTOR -H "Authorization: Bearer $(cat /var/run/secrets/kubernetes.io/serviceaccount/token)" | jq '.items[].spec.hostname' | sed 's/"//g' | tr '\n' ' ')
    for kube_pod_name in $kube_pod_names;
    do
        if [ $kube_pod_name == "null" ]
            then
                echo "Kubernetes discovery selected, but no pods found. Maybe we're the first?"
                echo "Anyway, we won't attempt to join any cluster."
                break
        fi
        if [ $kube_pod_name != $MY_POD_NAME ]
            then
                echo "Will join an existing Kubernetes cluster with discovery node at ${kube_pod_name}.${VERNEMQ_KUBERNETES_SUBDOMAIN}.${DOCKER_VERNEMQ_KUBERNETES_NAMESPACE}.svc.${DOCKER_VERNEMQ_KUBERNETES_CLUSTER_NAME}"
                echo "-eval \"vmq_server_cmd:node_join('VerneMQ@${kube_pod_name}.${VERNEMQ_KUBERNETES_SUBDOMAIN}.${DOCKER_VERNEMQ_KUBERNETES_NAMESPACE}.svc.${DOCKER_VERNEMQ_KUBERNETES_CLUSTER_NAME}')\"" >> /vernemq/etc/vm.args
                break
        fi
    done
fi

# Consul-based node discovery
if env | grep "DOCKER_VERNEMQ_DISCOVERY_CONSUL" -q; then
    # Ask Consul for services
    readonly CONSUL_HOST=${DOCKER_VERNEMQ_DISCOVERY_CONSUL_HOST:-consul.service.consul}
    readonly CONSUL_PORT=${DOCKER_VERNEMQ_DISCOVERY_CONSUL_PORT:-8500}
    readonly SVC_NAME=${DOCKER_VERNEMQ_DISCOVERY_CONSUL_SERVICE_NAME:-vernemq}

    NODE_NAME_PART2=${IP_ADDRESS}

    # Because all tasks start in parallel the first time (Nomad only do rolling update for updates)
    # We'll wait for some delay here (0 for the first alloc, then 15*n for the next allocs)
    info "Waiting for $((15 * ${DOCKER_VERNEMQ_DISCOVERY_CONSUL_STAGGER_IND:-0})) seconds before starting the discovery"
    sleep $((15 * ${DOCKER_VERNEMQ_DISCOVERY_CONSUL_STAGGER_IND:-0}))

    consul_vernemq_services_ip=$(curl -s -X GET http://${CONSUL_HOST}:${CONSUL_PORT}/v1/catalog/service/${SVC_NAME} | jq -r '.[] | .ServiceAddress' | tr '\n' ' ')
    for service_addr in $consul_vernemq_services_ip; do
        info "MQTT services found at ${service_addr}"
    done
    for service_addr in $consul_vernemq_services_ip; do
        if [[ "${service_addr}" != "${NODE_NAME_PART2}" ]]; then
            info "Will join an existing cluster with discovery node at ${service_addr}"
            echo "-eval \"vmq_server_cmd:node_join('VerneMQ@${service_addr}')\"" >> /vernemq/etc/vm.args
            break
        fi
    done
fi

info "Setting nodename to VerneMQ@${NODE_NAME_PART2}"
sed -i.bak -r "s/-name VerneMQ@.+/-name VerneMQ@${NODE_NAME_PART2}/" /vernemq/etc/vm.args

info "Setting distributed cookie value"
sed -i.bak -r "s/-setcookie .+/-setcookie ${DOCKER_VERNEMQ_DISTRIBUTED_COOKIE:-vmq}/" /vernemq/etc/vm.args

if [ -f /vernemq/etc/vernemq.conf.local ]; then
    info "Finish config using /vernemq/etc/vernemq.conf.local"
    cp /vernemq/etc/vernemq.conf.local /vernemq/etc/vernemq.conf
    sed -i -r "s/###IPADDRESS###/${IP_ADDRESS}/" /vernemq/etc/vernemq.conf
else
    info "Finish config using Docker env vars"
    cat <<EOF >> /vernemq/etc/vernemq.conf
########## Config from start_vernemq at $(date +"%F %T") ##########
# From Docker env vars
EOF

    env | grep DOCKER_VERNEMQ | grep -v 'DISCOVERY_NODE\|DISTRIBUTED_COOKIE\|KUBERNETES\|SWARM\|COMPOSE\|CONSUL\|DOCKER_VERNEMQ_USER' | cut -c 16- | awk '{match($0,/^[A-Z0-9_]*/)}{print tolower(substr($0,RSTART,RLENGTH)) substr($0,RLENGTH+1)}' | sed 's/__/./g' >> /vernemq/etc/vernemq.conf

    users_are_set=$(env | grep DOCKER_VERNEMQ_USER)
    if [ ! -z "$users_are_set" ]; then
        echo "vmq_passwd.password_file = /vernemq/etc/vmq.passwd" >> /vernemq/etc/vernemq.conf
        touch /vernemq/etc/vmq.passwd
    fi

    for vernemq_user in $(env | grep DOCKER_VERNEMQ_USER); do
        username=$(echo $vernemq_user | awk -F '=' '{ print $1 }' | sed 's/DOCKER_VERNEMQ_USER_//g' | tr '[:upper:]' '[:lower:]')
        password=$(echo $vernemq_user | awk -F '=' '{ print $2 }')
        /vernemq/bin/vmq-passwd /vernemq/etc/vmq.passwd $username <<EOF
$password
$password
EOF
    done

    cat <<EOF >> /vernemq/etc/vernemq.conf
# Set in startup script
erlang.distribution.port_range.minimum = 9100
erlang.distribution.port_range.maximum = 9109
########## End ##########
EOF

fi

# Check configuration file
/vernemq/bin/vernemq config generate 2>&1 > /dev/null | tee /tmp/config.out | grep error

if [ $? -ne 1 ]; then
    error "configuration error, exit"
    echo "$(cat /tmp/config.out)"
    exit $?
fi

pid=0

# SIGUSR1-handler
siguser1_handler() {
    info "stopped"
}

# SIGTERM-handler
sigterm_handler() {
    if [ $pid -ne 0 ]; then
        # this will stop the VerneMQ process
        /vernemq/bin/vmq-admin cluster leave node=VerneMQ@NODE_NAME_PART2 -k > /dev/null
        wait "$pid"
    fi
    exit 143; # 128 + 15 -- SIGTERM
}

# Setup OS signal handlers
trap 'siguser1_handler' SIGUSR1
trap 'sigterm_handler' SIGTERM

# Start VerneMQ
/vernemq/bin/vernemq console -noshell -noinput $@
pid=$(ps aux | grep '[b]eam.smp' | awk '{print $2}')
wait $pid
