FROM confluentinc/cp-kafkacat:5.4.1

RUN apt-get update \
    && apt-get install -y --no-install-recommends curl \
    expect \
    jq \
    tclsh \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/loopback/

COPY ./bin/entrypoint.sh ./bin/entrypoint.sh

ENTRYPOINT [ "./bin/entrypoint.sh" ]