#!/usr/bin/env bash

docker run -d \
       --name mongooseim-rmq \
       -p 5672:5672 \
       rabbitmq:3.7
