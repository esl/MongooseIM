#!/usr/bin/env bash
# We cannot just connect to 127.0.0.1:9042 because Docker is very "smart" and
# exposes ports before the service is ready
IP=$(/usr/bin/docker inspect -f {{.NetworkSettings.IPAddress}} cassandra)
echo "cassandra IP is $IP"
erl -eval 'try lists:map(fun(N) -> case gen_tcp:connect("'"$IP"'", 9042, [], 1000) of {error,econnrefused} -> io:format("Retry, n=~p~n", [N]), timer:sleep(1000); Other -> io:format("~p", [Other]), init:stop(0) end end, lists:seq(1,60)) after init:stop(1) end.'
