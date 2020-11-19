#!/bin/bash

echo "simulate kafka dojot msgs"

sleep 30

for ((i=0; ; i++))
do
    echo "Creating tenant2 in dojot-management2.dojot.tenancy"

    echo "{ \"type\": \"CREATE\", \"tenant\":\"tenant2\"}"  |\
    kafkacat -b kafka:9092 -P -t dojot-management2.dojot.tenancy

    sleep 5

    echo "Creating tenant1 in dojot-management.dojot.tenancy"

    echo "{ \"type\": \"CREATE\", \"tenant\":\"tenant1\"}"  |\
    kafkacat -b kafka:9092 -P -t dojot-management.dojot.tenancy

    sleep 40

    echo "Publish in tenant1.device-data with timestamp 1604587064592 ms (Thu Nov 05 2020 14:37:44)"

    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": 1604587064592 }, \"attrs\":  { \"geo\": \"-20.9064,47.0616\", \"string\": \"Thu Nov 05 2020 14:37:44\", \"int\": 20, \"float\": 15.5, \"bool\": false, \"nulled\": null,  \"array\": [\"array1\", { \"a\": \"abcd\"}],  \"obj\": { \"a\": \"abcd\"} }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data


    sleep 5

    echo "Publish in tenant1.device-data with should pesister = false and timestamp 1604587074598 ms (Thu Nov 05 2020 14:37:54)"

    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": 1604587074598 }, \"attrs\":  { \"string\": \"Thu Nov 05 2020 14:37:54\", \"shouldPersist\": false }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data without timestamp and shouldPersist=true"

    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\"}, \"attrs\":  { \"string\": \"without timestamp and shouldPersist=true\", \"shouldPersist\": true }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data with timestamp 2020-10-07T13:58:10.104999999Z"
    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-10-07T13:58:10.104999999Z\" }, \"attrs\":  { \"geo\": \"-20.9064,47.0616\", \"string\": \"2020-10-07T13:58:10.104999999Z\", \"int\": 20, \"float\": 15.5, \"bool\": false, \"nulled\": null,  \"array\": [\"array1\", { \"a\": \"rray\"}] }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data with timestamp invalid 2020-10-07T13:58:10"
    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-10-07T13:58:10\" }, \"attrs\":  { \"string\": \"Invalid 2020-10-07T13:58:10\"}}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data with timestamp 2020-10-01T13:58:10Z"
    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-10-01T13:58:10Z\" }, \"attrs\":  { \"string\": \"2020-10-01T13:58:10Z\"}}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data with timestamp 2020-10-07T13:58:10.10+01:00"
    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-10-07T13:58:10.10+01:00\" }, \"attrs\":  { \"string\": \"2020-10-07T13:58:10.10+01:00\"}}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data with timestamp 2020-10-11T13:58:10+01:00"
    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-10-11T13:58:10+01:00\" }, \"attrs\":  { \"string\": \"2020-10-11T13:58:10+01:00\"}}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data with timestamp 2020-10-12T13:58:10.999999999999999+01:00 (more than nano)"
    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-10-12T13:58:10.999999999999999+01:00\" }, \"attrs\":  { \"string\": \"2020-10-12T13:58:10.999999999999999+01:00 (more than nano)\"}}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5


    echo "Publish in tenant1.device-data with timestamp 2020-10-13T13:58:10.999999999999999Z (more than nano)"
    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-10-13T13:58:10.999999999999999Z\" }, \"attrs\":  { \"string\": \"2020-10-13T13:58:10.999999999999999Z (more than nano)\"}}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5


    echo "Publish in tenant1.device-data with timestamp invalid month and day 2020-14-32T13:58:10.104999999Z"
    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-14-32T13:58:10.104999999Z\" }, \"attrs\":  { \"geo\": \"-20.9064,47.0616\", \"string\": \"invalid month and day 2020-14-32T13:58:10.104999999Z\", \"int\": 20, \"float\": 15.5, \"bool\": false, \"nulled\": null,  \"array\": [\"array1\"],  \"obj\": { \"a\": \"abcd\"} }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data with timestamp invalid year 1020-12-31T13:58:10.104999999Z"

    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"1020-12-31T13:58:10.104999999Z\" }, \"attrs\":  { \"geo\": \"-20.9064,47.0616\", \"string\": \"invalid month and day 1020-12-31T13:58:10.104999999Z\", \"int\": 20, \"float\": 15.5, \"bool\": false, \"nulled\": null,  \"array\": [\"array1\"],  \"obj\": { \"a\": \"abcd\"} }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data with timestamp 2020-10-07T13:58:10.10Z"

    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-10-07T13:58:10.10Z\" }, \"attrs\":  {  \"string\": \"2020-10-07T13:58:10.10Z\" }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data a invalid json in attrs"

    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-11-07T13:58:10.10Z\" }, \"attrs\":  {  \"string\": 2020-10-07T13:58:10.10Z  }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data with a attr empty and  2020-11-07T14:58:10.10Z"

    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-11-07T14:58:10.10Z\" }, \"attrs\":  {  \"string\": \"2020-11-07T14:58:10.10Z\", \"string_empty\": \"\" }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Publish in tenant1.device-data a without attrs"

    echo "{ \"metadata\": { \"deviceid\": \"1234\", \"tenant\":\"tenant1\", \"timestamp\": \"2020-11-08T13:58:10.10Z\" }}"  |\
    kafkacat -b kafka:9092 -P -t tenant1.device-data

    sleep 5

    echo "Atuation in dojot.device-manager.device with timestamp 1604587186699 ms (Thu Nov 05 2020 14:39:46)"

    echo "{ \"event\" : \"configure\", \"meta\": { \"service\":\"tenant1\", \"timestamp\": 1604587086699 }, \"data\": { \"id\": \"1234\", \"attrs\":  { \"geo\": \"20.9064,47.0616\", \"string\": \"Thu Nov 05 2020 14:39:46\", \"int\": 30, \"float\": 25.5, \"bool\": true, \"obj\": { \"b\": \"bcde\"} }} }"  |\
    kafkacat -b kafka:9092 -P -t tenant1.dojot.device-manager.device

    sleep 30

    echo "Delete device in dojot.device-manager.device"

    echo "{ \"event\" : \"remove\", \"meta\": { \"service\":\"tenant1\" }, \"data\": { \"id\": \"1234\"} } "  |\
    kafkacat -b kafka:9092 -P -t tenant1.dojot.device-manager.device

    sleep 20

    echo "Deleting tenant2 in dojot-management2.dojot.tenancy"

    echo "{ \"type\": \"DELETE\", \"tenant\":\"tenant2\"}"  |\
    kafkacat -b kafka:9092 -P -t dojot-management2.dojot.tenancy

    sleep 10
done