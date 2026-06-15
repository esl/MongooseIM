In this short guide we will set up MongooseIM using Docker and run it as part of a cluster.

## Running MongooseIM

The container can be created and started with the following command:

```bash
docker run -dtP -h mongooseim-1 --name mongooseim-1 docker.trymongoose.im/mongooseim
```

In order to run image built for PR 1006, the command will look like the following:

```bash
docker run -dtP -h mongooseim-1 --name mongooseim-1 docker.trymongoose.im/mongooseim:PR-1006
```

You can check that `docker logs mongooseim-1` shows something similar to:

```
...
MongooseIM cluster primary node mongooseim@mongooseim-1
Clustered mongooseim@mongooseim-1 with mongooseim@mongooseim-1
Root: /usr/lib/mongooseim
Exec: /usr/lib/mongooseim/erts-13.1.5/bin/erlexec -boot /usr/lib/mongooseim/releases/6.1.0/start -embedded -config /usr/lib/mongooseim/etc/app.config -args_file /usr/lib/mongooseim/etc/vm.args -args_file /usr/lib/mongooseim/etc/vm.dist.args -- live --noshell -noinput +Bd
when=2023-10-24T14:28:03.386139+00:00 level=warning what=report_transparency pid=<0.574.0> at=service_mongoose_system_metrics:report_transparency/1:175 text="We are gathering the MongooseIM system's metrics to analyse the trends and needs of our users, improve MongooseIM, and know where to focus our efforts. For more info on how to customise, read, enable, and disable these metrics visit: \
- MongooseIM docs - \
      https://esl.github.io/MongooseDocs/latest/operation-and-maintenance/System-Metrics-Privacy-Policy/ \
- MongooseIM GitHub page - https://github.com/esl/MongooseIM \
The last sent report is also written to a file log/system_metrics_report.json" report_filename=log/system_metrics_report.json
```

We can health-check the MongooseIM node with `telnet`.
To do that, you need to provide the IP of the container (usually 127.0.0.1) and the published TCP port which translates to container’s port 5222.
In order to find the port you can use the following docker command:

```
$ docker ps -f "name=mongooseim-1" --format "{{.Names}}: {{.Ports}}"
mongooseim-1: 0.0.0.0:32772->4369/tcp, 0.0.0.0:32771->5222/tcp, 0.0.0.0:32770->5269/tcp, 0.0.0.0:32769->5280/tcp, 0.0.0.0:32768->9100/tcp
```

In the example above you can see that port 5222 inside the container was published on port 32771 on the docker host machine.
It can be used to check if the server is really listening on that port:

```
$ telnet 127.0.0.1 32771
Connected to localhost.
Escape character is '^]'.
```

Success! MongooseIM is accepting XMPP connections.

### Adding some users

```
docker exec mongooseim-1 /usr/lib/mongooseim/bin/mongooseimctl account registerUser --username $USER --domain $XMPP_HOST --password $PASSWORD
```

Where:
* $USER - a username
* $XMPP_HOST - the XMPP host served by MongooseIM - by default it's localhost
* $PASSWORD - password used for authentication

#### Example

```
$ docker exec mongooseim-1 /usr/lib/mongooseim/bin/mongooseimctl account registerUser --username alice --domain localhost --password secret123
{
  "data" : {
    "account" : {
      "registerUser" : {
        "message" : "User alice@localhost successfully registered",
        "jid" : "alice@localhost"
      }
    }
  }
}
```

## Customising the configuration

You can override the default configuration files by providing them using docker volumes. Let's assume on the local machine there is directory `mongooseim-1` with the following content:

```bash
$ tree mongooseim-1
mongooseim-1
└── mongooseim.toml
└── app.config
└── vm.args
```

Now we can run the container:

```bash
docker run -dt -h mongooseim-1 --name mongooseim-1 -p 5222:5222 -v ./mongooseim-1:/member docker.trymongoose.im/mongooseim
```

The server will use the customised configuration files.
There is also a `vm.dist.args` file which can be overwritten in the same way.

### Database setup

MongooseIM can be integrated with various databases and other external services.
For example, let's run a [PostgreSQL](https://hub.docker.com/_/postgres/) container:

```bash
docker run -d --name mongooseim-postgres --net mim \
       -e POSTGRES_PASSWORD=mongooseim -e POSTGRES_USER=mongooseim \
       -v ${PATH_TO_MONGOOSEIM_PGSQL_FILE}:/docker-entrypoint-initdb.d/pgsql.sql:ro \
       -p 5432:5432 postgres
```

`${PATH_TO_MONGOOSEIM_PGSQL_FILE}` is an absolute path to `priv/pgsql.sql`, which can be found in the MongooseIM repo.

Don't forget to configure the [outgoing connection pools](https://esl.github.io/MongooseDocs/latest/configuration/outgoing-connections/) in `mongooseim.toml` to connect with the services you set up!

#### Persisting Mnesia files

Mnesia files are kept in `/var/lib/mongooseim` directory inside the container. In some cases it is desired to keep them while updating the image.
In this case the `/var/lib/mongooseim` dir should be mounted to a host directory.

#### Using CETS

You can use CETS, which is the recommended backend for transient data, instead of Mnesia - see the [tutorial](https://esl.github.io/MongooseDocs/latest/tutorials/CETS-configure/) for more details.

### Setting up a cluster

There are two methods of clustering: CETS (recommended) and Mnesia, which can be automatic (default) or manual, giving move control over the cluster formation.

#### CETS

Ensure that outgoing pools are configured with an RDBMS so that CETS can retrieve a list of MongooseIM nodes using the same relational database and cluster them together.

Create a user-defined bridge network and start two nodes connected to it:

```bash
docker network create mim
docker run -dt --net mim --name mongooseim-1 -v ./mongooseim-1:/member docker.trymongoose.im/mongooseim
docker run -dt --net mim --name mongooseim-2 -v ./mongooseim-2:/member docker.trymongoose.im/mongooseim
```

The nodes should already form a cluster. Let's check it:

```bash
$ docker exec mongooseim-1 /usr/lib/mongooseim/bin/mongooseimctl cets systemInfo
{
  "data" : {
    "cets" : {
      "systemInfo" : {
        (...)
        "availableNodes" : [
          "mongooseim@b5b6a9ac9df8",
          "mongooseim@f04c2070b082"
        ]
      }
    }
  }
}

$ docker exec mongooseim-2 /usr/lib/mongooseim/bin/mongooseimctl cets systemInfo
{
  "data" : {
    "cets" : {
      "systemInfo" : {
        (...)
        "availableNodes" : [
          "mongooseim@b5b6a9ac9df8",
          "mongooseim@f04c2070b082"
        ]
      }
    }
  }
}
```

##### File-based discovery

It is possible to read a list of nodes to cluster from a file. See [CETS with the file discovery backend](https://esl.github.io/MongooseDocs/latest/tutorials/CETS-configure/#cets-with-the-file-discovery-backend).

#### Mnesia

To use the automatic clustering method, your containers need both container names (`--name` option) and host names (`-h` option) with the `-n` suffix,
where `n` are consecutive integers starting with `1` (configurable with `MASTER_ORDINAL` env variable), e.g. `mongooseim-1`, `mongooseim-2` and so on.
Make sure you have started a node with `-${MASTER_ORDINAL}` suffix first (e.g. `-h mongooseim-1` and `--name mongooseim-1`), as all the other nodes will connect to it when joining the cluster.

Few things are important here:

1. The following parameters must be set to the same value if used in docker/docker-compose. The second and all the subsequent containers have the same requirement.

    * `-h` option sets `HOSTNAME` environment variable for the container which in turn sets long hostname of the machine. The [start.sh](https://github.com/esl/mongooseim-docker/blob/bcaa3c17/member/start.sh#L19) script uses it to generate the Erlang node name if `NODE_TYPE=name`.
    If `NODE_TYPE=sname` (default), short hostname will be used instead. If the value provided to `-h` option is already a short hostname, it will be used as is,
    otherwise it will be shortened (longest part that doesn't contain '.' character).
    If you need to make the host part of the node name different from `HOSTNAME` (or use an IP address instead), you can do it with the `NODE_HOST` environment variable, e.g. `-e NODE_HOST=192.168.1.1`.
    * `--name` is required to provide automatic DNS resolution between the containers. See [Docker network documentation](https://docs.docker.com/network/bridge/#differences-between-user-defined-bridges-and-the-default-bridge) page for more details.

1. Format of the host name:

    * Host name of the first container must be in the `${NODE_NAME}-${MASTER_ORDINAL}` format. That allows [start.sh](https://github.com/esl/mongooseim-docker/blob/bcaa3c17/member/start.sh#L75) to identify the primary node of the cluster.
    * All the subsequent containers must follow the `${NODE_NAME}-N` host name format, where `N` > `${MASTER_ORDINAL}`.

##### Example

Create a user-defined bridge network and start two nodes connected to it:
```
docker network create mim
docker run -dt --net mim -h mongooseim-1 --name mongooseim-1 -e JOIN_CLUSTER=true docker.trymongoose.im/mongooseim
docker run -dt --net mim -h mongooseim-2 --name mongooseim-2 -e JOIN_CLUSTER=true docker.trymongoose.im/mongooseim
```

The nodes should already form a cluster. Let's check it:

```bash
$ docker exec mongooseim-1 /usr/lib/mongooseim/bin/mongooseimctl mnesia systemInfo --keys '["running_db_nodes"]'
{
  "data" : {
    "mnesia" : {
      "systemInfo" : [
        {
          "result" : [
            "mongooseim@mongooseim-2",
            "mongooseim@mongooseim-1"
          ],
          "key" : "running_db_nodes"
        }
      ]
    }
  }
}
$ docker exec mongooseim-2 /usr/lib/mongooseim/bin/mongooseimctl mnesia systemInfo --keys '["running_db_nodes"]'
{
  "data" : {
    "mnesia" : {
      "systemInfo" : [
        {
          "result" : [
            "mongooseim@mongooseim-1",
            "mongooseim@mongooseim-2"
          ],
          "key" : "running_db_nodes"
        }
      ]
    }
  }
}
```

##### Kubernetes notes

Default clustering may work as part of Kubernetes StatefulSet deployment with only two changes:

* `MASTER_ORDINAL` has to be set to `0` as `StatefulSet` starts counting instances from 0
* `NODE_TYPE` has to be set to `name` (use of long names) as Kubernetes uses FQDN within internal DNS to resolve `pod's` IP address.
  Please note that for `pod` domain to work you have to have headless service running that matches your `StatefulSet`
  (see https://kubernetes.io/docs/concepts/services-networking/service/#headless-services)

##### Manual clustering

With the manual clustering method, you need to explicitly specify the name of the node to join the cluster with via the `CLUSTER_WITH` environment variable.

###### Examples

Let's try providing a name of the node to join the cluster with manually:

```bash
docker run -dt --net mim -h first-node --name first-node docker.trymongoose.im/mongooseim
docker run -dt --net mim -h second-node --name second-node -e JOIN_CLUSTER=true -e CLUSTER_WITH=mongooseim@first-node docker.trymongoose.im/mongooseim
```

The first command starts a node which, by default, does not to try to join any clusters (since `JOIN_CLUSTER` is set to `false` by default).
We then tell the second node to join the cluster with the first node.

You can now check that the nodes have formed the cluster:

```bash
$ docker exec first-node /usr/lib/mongooseim/bin/mongooseimctl mnesia systemInfo --keys '["running_db_nodes"]'
{
  "data" : {
    "mnesia" : {
      "systemInfo" : [
        {
          "result" : [
            "mongooseim@second-node",
            "mongooseim@first-node"
          ],
          "key" : "running_db_nodes"
        }
      ]
    }
  }
}
$ docker exec second-node /usr/lib/mongooseim/bin/mongooseimctl mnesia systemInfo --keys '["running_db_nodes"]'
{
  "data" : {
    "mnesia" : {
      "systemInfo" : [
        {
          "result" : [
            "mongooseim@first-node",
            "mongooseim@second-node"
          ],
          "key" : "running_db_nodes"
        }
      ]
    }
  }
}
```
