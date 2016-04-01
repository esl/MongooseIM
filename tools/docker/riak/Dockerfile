FROM hectcastro/riak
ADD advanced.config /etc/riak/advanced.config
RUN sed -i.bak "s/^search = \(.*\)/search = on/" /etc/riak/riak.conf
