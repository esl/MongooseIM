FROM michalwski/docker-riak:1.0.6
ADD advanced.config /etc/riak/advanced.config
RUN sed -i.bak "s/^search = \(.*\)/search = on/" /etc/riak/riak.conf
