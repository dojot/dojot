#!/bin/bash

file=$1

cmd=`bash -c "cat $file"`
cmd2=`echo "kafkacat -b kafka:9092 -P -t example.dojot.x509-identity-mgmt.certificates"`
container="confluentinc/cp-kafkacat:latest"

final_cmd="docker run --network=dojot_default $container bash -c 'echo $cmd | $cmd2'"

echo "running command: \"$final_cmd\""

eval $final_cmd
