#!/bin/bash

##################################################################
#                                                                #
# Copyright (c) 2018-2019 PrimeKey Solutions AB.                 #
#                                                                #
# This software is free software; you can redistribute it and/or #
# modify it under the terms of the GNU Lesser General Public     #
# License as published by the Free Software Foundation; either   #
# version 2.1 of the License, or any later version.              #
#                                                                #
# See terms of license at gnu.org.                               #
#                                                                #
##################################################################

if [ ! -z "$DEBUG" ] ; then
    set -x
fi

# Redirect stderr to stdout to make output easier to consume by third party tools
exec 2>&1

baseDir="/opt/primekey"
tempDir="$(mktemp -d --tmpdir=${baseDir}/tmp)"

# Setup defaults for environment
if [ -f ${baseDir}/bin/internal/environment-pre       ] ; then source ${baseDir}/bin/internal/environment-pre      "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/environment-app       ] ; then source ${baseDir}/bin/internal/environment-app      "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/environment-defaults  ] ; then source ${baseDir}/bin/internal/environment-defaults "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/environment-post      ] ; then source ${baseDir}/bin/internal/environment-post     "${baseDir}" "${tempDir}" ; fi

# Import common functions like logger etc
if [ -f ${baseDir}/bin/internal/functions-common    ] ; then source ${baseDir}/bin/internal/functions-common    "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/functions-appserver ] ; then source ${baseDir}/bin/internal/functions-appserver "${baseDir}" "${tempDir}" ; fi

# Setup a username for the assigned user id that we run under unless it is already present
id | log "INFO" || log "INFO" "Failed to retrieve current user id. Ignoring."

if ! whoami &> /dev/null; then
  if [ -w /etc/passwd ]; then
    echo "${APPLICATION_NAME}:x:$(id -u):0:${APPLICATION_NAME} user:/opt:/sbin/nologin" >> /etc/passwd
  fi
fi

### Perform sanity checks ###

if [ "x$(java -version 2>&1 | grep -v bash)" == "x" ] ; then
  log "ERROR" "No 'java' found."
  exit 1
fi

if [ "x$BASH" == "x" ] ; then
  log "ERROR" "This is not a bash shell which is expected. Aborting."
  exit 1
fi

### Check cgroup limitations that OpenJDK 8 wont take into account automatically to prevent bad tuning of the JVM ###

# Calculate the number of available cores
cpuPeriod=$(cat /sys/fs/cgroup/cpu/cpu.cfs_period_us)
cpuQouta=$(cat /sys/fs/cgroup/cpu/cpu.cfs_quota_us)
cpuTotal=$(cat /proc/cpuinfo | grep ^processor | wc -l)
if [ ${cpuQouta:-0} -ne -1 ]; then
  coreLimit=$((cpuQouta/cpuPeriod))
else
  coreLimit=$cpuTotal
fi
log "INFO" "Detected $coreLimit available cores."

# Approx of: 3/2 max(log2(cores)*log2(max(log2(cores),1)))
#1:  1, 16:  4, 32:  5, 64:  6, 128:  7, 256:  12, 512:  13, 1024:  15, 2048:  16
# CICompilerCount of 1 is invalid; must be at least 2
ciCompilerCount=$(echo $coreLimit | awk '{print 2+int(log($1)/log(2))}')
if [ ! -z "$DEBUG" ] ; then
    log "DEBUG" "Estimated ciCompilerCount to $ciCompilerCount."
fi

# Calculate memory available to the container
memCgroup=$(cat /sys/fs/cgroup/memory/memory.limit_in_bytes)
memTotal=$(cat /proc/meminfo | awk '/MemTotal/ {print $2*1024}')
if [ ${memCgroup:-0} != -1 ] && [ ${memCgroup:-0} -lt ${memTotal} ] ; then
  memLimit=${memCgroup}
  log "INFO" "Detected $memLimit bytes available memory assigned to this container."
else
  memLimit=${memTotal}
  log "INFO" "Detected $memLimit bytes available host memory."
fi
if [ $(($memLimit/1024/1024)) -gt 4096 ] ; then
  memLimit=$((4096*1024*1024))
  log "INFO" "Limiting application JVM to only use ${memLimit} bytes of RAM."
fi

if [ -z "$JAVA_OPTS_CUSTOM" ] ; then
  memRequired=1024
  #if [[ "$DATABASE_JDBC_URL" =~ ^jdbc:h2:mem:.* ]]; then
  #  # Require additional minimal amount of memory when an in memory database is specified.
  #  memRequired=$(($memRequired+192))
  #fi
  if [ $(($memLimit/1024/1024)) -lt $memRequired ] ; then
    log "ERROR" "You need to assign at least '${memRequired}m' to this container."
    exit 1
  fi
  memLimitMiB=$((memLimit/1024/1024))
  # The CLIs used during startup will allocation 64m, but before app server uses up all heap
  otherProcesses=$((64-64))
  jvmTotal=$((memLimitMiB-otherProcesses))
  jvmXXMaxMetaspaceSize=256
  # -Xss defaults to 1m on x64 Linux ... 200 threads at 256k ≃ 50 MiB
  jvmOther=64
  jvmXMx=$((jvmTotal-jvmXXMaxMetaspaceSize-jvmOther))
  # Maximum used memory = Xmx + MetaspaceSize + #threads*Xss
  JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -Xms128m -Xmx${jvmXMx}m -Xss256k -XX:MetaspaceSize=160m -XX:MaxMetaspaceSize=${jvmXXMaxMetaspaceSize}m"
fi

# -XX:MinHeapFreeRatio=X (Grow heap when less than X% is free)
# -XX:MaxHeapFreeRatio=Y (Shrink heap when more than Y% is free).

JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -XX:+UseParallelGC -XX:GCTimeRatio=4 -XX:AdaptiveSizePolicyWeight=90 -XX:MinHeapFreeRatio=10 -XX:MaxHeapFreeRatio=20"
#JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -XX:+UseG1GC -XX:+UseStringDeduplication -XX:StringDeduplicationAgeThreshold=1 -XX:InitiatingHeapOccupancyPercent=20"
#JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -XX:+UnlockExperimentalVMOptions -XX:G1NewSizePercent=10 -XX:G1MaxNewSizePercent=20"
JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -XX:ParallelGCThreads=${coreLimit} -XX:ConcGCThreads=${coreLimit} -Djava.util.concurrent.ForkJoinPool.common.parallelism=${coreLimit} -XX:CICompilerCount=${ciCompilerCount}"

### Additional runtime tweaks to Java ###

# Just kill the process if it runs out of memory instead of stalling with massive GC
#JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -XX:OnOutOfMemoryError='kill -9 %p'"
if [ "x$DEBUG" != "xtrue" ] ; then
    JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -XX:+ExitOnOutOfMemoryError"
else
    JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -XX:+CrashOnOutOfMemoryError"
fi

# Force use of 2048-bit DH keys in order to mitigate https://weakdh.org/
JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -Djdk.tls.ephemeralDHKeySize=2048"

### Show detailed JVM info if DEBUG environment variable is configured ###

if [ ! -z "$DEBUG" ] ; then
  # Enable GC diagnostics if env.DEBUG is set
  JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -XX:+PrintGC -XX:+PrintGCDateStamps -XX:+PrintGCTimeStamps"
  # Show JVM settings based on current JVM options
  eval java "$JAVA_OPTS_CUSTOM -XX:+PrintFlagsFinal -XX:+UnlockDiagnosticVMOptions -XX:NativeMemoryTracking=summary -XX:+PrintNMTStatistics" -version
  # Enable additional logging from the app-server
  #export LOG_LEVEL_SERVER_SUBSYSTEMS=${LOG_LEVEL_SERVER_SUBSYSTEMS:-INFO}
  export LOG_LEVEL_SERVER_SUBSYSTEMS="INFO"
  log "DEBUG" "Available entropy: $(cat /proc/sys/kernel/random/entropy_avail)"
fi

# By default JBoss EAP 7.2 for the Openshift base image has this configured. It would potentially be secure to enable this automatically
# * once entropy_avail has reached a certain level since system boot
# * x bytes of /dev/random has been read to ensure that also /dev/urandom has been properly initialized
# BUT this means that SHA1PRNG will be used instead of NativePRNG in Java.
SECURE_RANDOM_SOURCE="/dev/random"
case "$JAVA_SECURITY_USE_URANDOM" in
    force )
        SECURE_RANDOM_SOURCE="/dev/./urandom"
        ;;
    true )
        # Read 256 bits of random from the blocking source to ensure the pool is properly initialized
        log "INFO" "Reading 256 bits of random from blocking source before allowing /dev/urandom to be used."
        dd if=/dev/random count=1 bs=32 2>/dev/null > /dev/null
        SECURE_RANDOM_SOURCE="/dev/./urandom"
        ;;
esac

export SECURE_RANDOM_SOURCE
JAVA_OPTS_CUSTOM="$JAVA_OPTS_CUSTOM -Djava.security.egd=file:${SECURE_RANDOM_SOURCE}"

export JAVA_OPTS_CUSTOM

### Prepare for application startup ###

# Set a unique transaction node identifier of max 23 bytes to be used by the application server
export TRANSACTION_NODE_ID="$(echo ${HOSTNAME:-localhost} | sha256sum | cut -c1-23)"

appserver_reset_config

if [ -f ${baseDir}/bin/internal/after-init-pre.sh  ] ; then . ${baseDir}/bin/internal/after-init-pre.sh  "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/after-init.sh      ] ; then . ${baseDir}/bin/internal/after-init.sh      "${baseDir}" "${tempDir}" ; fi
if [ -f ${baseDir}/bin/internal/after-init-post.sh ] ; then . ${baseDir}/bin/internal/after-init-post.sh "${baseDir}" "${tempDir}" ; fi
### Start up the application and monitor progress in a separate thread ###

appserver_prepare_startup

deployment_monitor_thread() {
    sleep 5
    deploymentDone=0
    while true ; do
        if appserver_deployment_failed ; then
            appserver_shutdown
            deploymentDone=1
        fi
        if appserver_deployment_success ;  then
            log "INFO" "Application ${f} successfully started."
            ### Call post startup hooks ###
            if [ -f ${baseDir}/bin/internal/after-deployed-pre.sh  ] ; then . ${baseDir}/bin/internal/after-deployed-pre.sh  "${baseDir}" "${tempDir}" ; fi
            if [ -f ${baseDir}/bin/internal/after-deployed.sh      ] ; then . ${baseDir}/bin/internal/after-deployed.sh      "${baseDir}" "${tempDir}" ; fi
            if [ -f ${baseDir}/bin/internal/after-deployed-post.sh ] ; then . ${baseDir}/bin/internal/after-deployed-post.sh "${baseDir}" "${tempDir}" ; fi
            if [ -f /opt/app/init/entrypoint.sh ] ; then . /opt/app/init/entrypoint.sh ; fi
            deploymentDone=1
            if [ "x${SHUTDOWN_AFTER_DEPLOY}" == "xtrue" ] ; then
                appserver_shutdown
            fi
        fi
        if [ $deploymentDone = 1 ] ; then
            break
        fi
        sleep 2
    done
}

(deployment_monitor_thread || log "ERROR" "Deployment monitor failed." ; ) &
deploymentMonitorThreadPid=$!

log "INFO" "Starting application server:"

appserver_start
exitValue=$?

rm -rf "${tempDir}"

kill $deploymentMonitorThreadPid >/dev/null 2>&1

exit $exitValue