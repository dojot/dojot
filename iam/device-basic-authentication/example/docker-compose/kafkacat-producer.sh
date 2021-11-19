#!/bin/bash

echo "simulate kafka dojot msgs"


echo "Remove tenant in dojot-management.dojot.device-manager.device"

echo "{ \"type\": \"remove\", \"tenant\":\"admin\", \"deviceId\":\"123abc\"}"  |\
kafkacat -b kafka:9092 -P -t dojot-management.dojot.device-manager.device

sleep 5

echo "Delete tenant in dojot-management.dojot.tenancy"

echo "{ \"type\": \"DELETE\", \"tenant\":\"admin\"}"  |\
kafkacat -b kafka:9092 -P -t dojot-management.dojot.tenancy