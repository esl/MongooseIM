# Push notifications with MongoosePush

MongoosePush is a simple RESTful service written in Elixir. It provides the ability to send push
notifications to FCM (Firebase Cloud Messaging) and/or APNs (Apple Push Notification Service) via
their HTTP/2 API.

To take advantage of MongoosePush's functionality, you will need to enable the
`mod_push_service_mongoosepush` module: this module acts as a bridge between the
`push_notifications` hook and [MongoosePush][] itself.

## Getting started

To enable integration with MongoosePush, it is as simple as the next two steps.
First, you need to define a pool of HTTPS connections to MongoosePush in the
`outgoing_pools` section:

```toml
[outgoing_pools.http.mongoose_push_http]
  scope = "global"
  strategy = "available_worker"

  [outgoing_pools.http.mongoose_push_http.connection]
    host = "https://localhost:8443"
```

And second, you need to add `mod_push_service_mongoosepush` to the `modules` section in the config file:

```toml
[modules.mod_push_service_mongoosepush]
  pool_name = "mongoose_push_http"
  api_version = "v3"
```

Here, we assume that [MongoosePush][] will be available on the localhost on port 8443, which is the
default one — note the `host` option in the outgoing pool definition.
Next we enable [mod_push_service_mongoosepush][]. The first option is the name of the HTTP pool to
use and the second one is the version of [MongoosePush][]'s API ("_v2_" or "_v3_" are supported).

And that's it, we've just completed the entire MongooseIM configuration.
All we need to do now is to set up [MongoosePush][].

## Starting [MongoosePush][]

The easiest way to start [MongoosePush][] is with its
[Docker image](https://hub.docker.com/r/erlangsolutions/mongoose-push).
Before starting it, prepare the HTTPS credentials for its API and credentials for at least one
delivery provider: an FCM service account file, APNs credentials, or both. The corresponding
provider and connection pool must also be enabled in the MongoosePush configuration.

See the MongoosePush documentation for the current
[Docker setup](https://esl.github.io/MongoosePush/v2.3.0/docker.html#running-from-dockerhub)
and [configuration options](https://esl.github.io/MongoosePush/v2.3.0/configuration.html).
Once MongoosePush is listening at the address configured in the outgoing HTTP pool above,
MongooseIM can send push notifications through it.

[MongoosePush]: https://github.com/esl/MongoosePush
[mod_push_service_mongoosepush]: ../../modules/mod_push_service_mongoosepush.md
