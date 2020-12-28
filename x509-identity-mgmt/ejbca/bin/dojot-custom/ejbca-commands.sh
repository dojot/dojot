#!/bin/bash

function ejbca_cmd() {

    local javaOpts=( \
        '-Xms32m' \
        '-Xmx32m' \
        '-XX:MetaspaceSize=32M' \
        '-XX:MaxMetaspaceSize=32m' \
        '-XX:MaxRAM=64m' \
        '-XX:ParallelGCThreads=1' \
        '-XX:ConcGCThreads=1' \
        '-Djava.util.concurrent.ForkJoinPool.common.parallelism=1' \
        '-XX:CICompilerCount=2' \
        "-Djava.security.egd=file:${SECURE_RANDOM_SOURCE:-/dev/random}" \
        "-Duser.home=${EJBCA_CLI_USER_HOME}" \
    )

    # Redirect stderr to stdout to make output easier to consume by third party tools
    java "${javaOpts[@]}" -jar "${EJBCA_CLI_JAR}" "$@" 2>&1
}

function ejbca_cmd_debug() {

    local javaOpts=( \
        '-Xdebug' \
        "-Xrunjdwp:transport=dt_socket,address=${EJBCA_CLI_DEBUG_PORT},server=y,suspend=y" \
        '-Xms32m' \
        '-Xmx32m' \
        '-XX:MetaspaceSize=32M' \
        '-XX:MaxMetaspaceSize=32m' \
        '-XX:MaxRAM=64m' \
        '-XX:ParallelGCThreads=1' \
        '-XX:ConcGCThreads=1' \
        '-Djava.util.concurrent.ForkJoinPool.common.parallelism=1' \
        '-XX:CICompilerCount=2' \
        "-Djava.security.egd=file:${SECURE_RANDOM_SOURCE:-/dev/random}" \
        "-Duser.home=${EJBCA_CLI_USER_HOME}" \
    )

    echo
    echo "EJBCA-CLI: JVM Remote Debug at ${CONTAINER_IP}:${EJBCA_CLI_DEBUG_PORT}"
    echo "${javaOpts[@]}"

    # Redirect stderr to stdout to make output easier to consume by third party tools
    java "${javaOpts[@]}" -jar "${EJBCA_CLI_JAR}" "$@" 2>&1
}
