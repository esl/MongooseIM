## Logs

We strongly recommend storing logs in one centralized place when working in a clustered environment.
MongooseIM uses Lager - the logging framework. 
Its backend can be easily replaced; the `syslog` backend is included by default in MongooseIM.


### Using syslog

To activate the syslog backend you have to edit `rel/files/app.config` and uncomment the line:

    %% use below line to add syslog backend for Lager
    %        {lager_syslog_backend, [ "mongooseim", local0, info]},

Remember to provide a parameter list to make your lager syslog backend running:

* The first parameter is a string to tag all the syslog messages with. 
 The default is `mongooseim`.
* The second one is the facility to log to (see the syslog documentation).
* The last parameter is the lager level at which the backend accepts messages. 
 In our case it's `info`.

Depending on the system platform you use, remember also to add the appropriate line in the syslog config file:

    local0.info                     /var/log/mongooseim.log

Now all the logs of level `info` will be passed to the `/var/log/mongooseim.log` file.

Example log (e.g `tail -f /var/log/mongooseim.log`):

    Apr  1 12:36:49 User.local mongooseim[6068]: [info] <0.7.0> Application mnesia started on node mongooseim@localhost

### Further / multiserver integration

For more advanced processing and analysis of logs, including gathering logs from multiple machines, you can use one of the many available systems (e.g. logstash/elasticsearch/kibana, graylog, splunk), which collect data from the syslog and are beyond the scope of this documentation. 

## Monitoring

### WombatOAM

WombatOAM is an operations and maintenance framework for Erlang based systems.
Its Web Dashboard displays this data in an aggregated manner.
What is more, WombatOAM provides interfaces to feed the data to other OAM tools such as Graphite, Nagios or Zabbix.

For more information see: [WombatOAM](https://www.erlang-solutions.com/products/wombat-oam.html).

### graphite-collectd

To monitor MongooseIM during load testing, we recommend the following open source applications:

- [Graphite](http://graphite.wikidot.com/) is used for data presentation.
- [collectd](http://collectd.org/) is a daemon running on the monitored nodes capturing data related to CPU and Memory usage, IO etc.


### Built-in Exometer reporters

MongooseIM uses the Exometer libary for collecting the metrics. 
Exometer has many build-in reporters that can send metrics to external services like:

* graphite
* amqp
* statsd
* snmp
* opentsdb

It is possible to enable them in MoongooseIM via the `app.config` file.
The file sits next to the `ejabberd.cfg` file in the `rel/files` and `_REL_DIR_/etc` directories.
For more details, please visit the Exometer's project page: [Exometer](https://github.com/Feuerlabs/exometer).

**Note that we are using the 1.2.1 version with our patches.**

Below you can find a sample configuration. 
It shows setting up a reporter connecting to graphite running on localhost.

You can see an additional option not listed in the Exometer docs - `mongooseim_report_interval`.
That option sets the metrics resolution: how often Exometer gathers and sends metrics through reporters. 
By default that is 60 seconds.

```erl
...
{exometer, [
    {mongooseim_report_interval, 60000}, %% 60 seconds
    {report, [
        {reporters, [
                     {exometer_report_graphite, [
                                                 {prefix, "mongooseim"},
                                                 {connect_timeout, 5000},
                                                 {host, "127.0.0.1"},
                                                 {port, 2003},
                                                 {api_key, ""}
                                                ]}
                    ]}
    ]}
  ]}
...
```

### Run graphite in Docker - quick start

If you don't have a default docker machine created yet:

    docker-machine create --driver=virtualbox default

Start a docker machine:

    docker-machine start

Make sure it is running:

    $ docker-machine status
    Running

Run the following command:

    $ docker run -d --name graphite --restart=always hopsoft/graphite-statsd

Get the "local" IP address of the container:

    $ docker inspect graphite | grep IPAdd | grep -v Secon | cut -d '"' -f 4 | head -n 1
    172.17.0.2

Get IP address of the machine:

    $ docker-machine ip
    192.168.99.100

Route subnet:

    $ sudo route add -net 172.17.0.0 192.168.99.100

#### Verification

And now http://172.17.0.2 should show a graphite page.

Check if the data collection works - run:

    $ while true; do echo -n "example:$((RANDOM % 100))|c" | nc -w 1 -u 172.17.0.2 8125; done

Wait a while, then open:

    http://172.17.0.2/render?from=-10mins&until=now&target=stats.example

Then, if you configure your MongooseIM to send Exometer reports to that IP address and run it for a while, you should be able to see some interesting charts.

### Run graphite in Docker - alternative method

If one of these steps above doesn't work for you (e.g. they may be incompatible with your Docker on Mac), please try a less advanced alternative.
It doesn't require docker machine and will expose Graphite ports on localhost, instead of using a new route.

Start a container with Graphite:

    $ docker run -d --name graphite --restart=always -p 80:80 -p 2003-2004:2003-2004 -p 2023-2024:2023-2024 -p 8125:8125/udp -p 8126:8126 hopsoft/graphite-statsd

Now do the "Verification" part from previous subsection. There is one difference: you need to replace every occurrence of `172.17.0.2` with `localhost`.

