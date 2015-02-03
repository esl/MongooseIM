Logs
---

It is a good idea to store logs in one centralized place when working in a clustered environment.
MongooseIM uses Lager - the logging framework. Its backend can be easy replaced.
Some of the recommended backends are:

- https://github.com/basho/lager_syslog to use syslog and 
- https://github.com/mhald/lager_logstash_backend for logstash (http://logstash.net/).

To change the backend you have to edit `rel/files/app.config`. Before that you need
to add the backend to deps in `rebar.config` file:

    {lager_syslog, ".*", {git, "git://github.com/basho/lager_syslog.git"}},

and execute:

    ./rebar get-deps

The following entry to rel/reltool.config has to be added

    {app, lager_syslog, [{incl_cond, include}]},


Monitoring
---

### graphite-collectd


To monitor MongooseIM during load testing we recommend the following open source applications:

- Graphite (http://graphite.wikidot.com/) is used for data presentation 
- collectd (http://collectd.org/) is a daemon running on monitored nodes capturing data related to CPU and Memory usage, IO etc. 

### mod_metrics

It provides REST interface for Mongoose's metrics, so it can be easily integrated
with other services.

You can read more about it here:

https://github.com/esl/MongooseIM/wiki/REST-interface-to-folsom-metrics

### Wombat OAM

WombatOAM is an operations and maintenance framework for Erlang based systems. Its Web Dashboard displays this data in an aggregated manner and provides interfaces to feed the data to other OAM tools such as Graphite, Nagios or Zabbix.
For more information see:
https://www.erlang-solutions.com/products/wombat