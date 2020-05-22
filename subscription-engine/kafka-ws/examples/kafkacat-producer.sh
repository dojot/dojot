#!/bin/bash

for ((i=0; ; i++))
do

    if (( $i % 10 == 0 ))
    then
        TEMP=0
        RAIN=0
        STATUS="stopped"
        LOCA="-22.9064,47.0616"
    elif (( $i % 2 == 0 ))
    then
        TEMP=35
        RAIN=10
        STATUS="failed"
        LOCA="-21.9064,47.0616"
    else
        TEMP=20
        RAIN=15
        STATUS="running"
        LOCA="-20.9064,47.0616"
    fi


    echo "{ \"sensor\": { \"status\": \"${STATUS}\"}, \"temperature\": ${TEMP} }"  |\
    kafkacat -b kafka:9092 -P -t ws.example.test

    echo "{ \"temperature\": ${TEMP+2}, \"rain\": ${RAIN} }"  |\
    kafkacat -b kafka:9092 -P -t ws.example.test

    echo "{ \"location\": \"${LOCA}\", \"temperature\": ${TEMP}}"  |\
    kafkacat -b kafka:9092 -P -t ws2.example.test

    sleep 10
done