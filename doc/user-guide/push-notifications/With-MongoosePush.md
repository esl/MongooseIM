## Push notifications with MongoosePush

MongoosePush is a simple RESTful service written in Elixir. It provides the ability to send push
notifications to FCM (Firebase Cloud Messaging) and/or APNS (Apple Push Notification Service) via
their HTTP/2 API.

To take advantage of MongoosePush's functionality, you will need to enable the
`mod_push_service_mongoosepush` module: this module acts as a bridge between the
`push_notifications` hook and [MongoosePush][] itself.

### Getting started

To enable integration with MongoosePush, it is as simple as the next two steps.
First, you need to define a pool of HTTPS connections to MongoosePush in the
`outgoing_pools` section:

```Erlang
{outgoing_pools, [
    (...)
    {http, global, mongoose_push_http,
        [{strategy, available_worker}],
        [{server, "https://localhost:8443"}]},
    (...)
    ]
}.
```

And second, you need to add `mod_push_service_mongoosepush` to the `modules` section in the config file:

```Erlang
{modules, [
    (...)
    {mod_push_service_mongoosepush, [
        {pool_name, mongoose_push_http},
        {api_version, "v3"}]},
    (...)
    ]
}.
```

Here, we assume that [MongoosePush][] will be available on the localhost on port 8443, which is the
default one — note the `server` option in the outgoing pool definition.
Next we enable [mod_push_service_mongoosepush][]. The first option is the name of the HTTP pool to
use and the second one is the version of [MongoosePush][]'s API ("_v2_" or "_v3_" are supported).

And that's it, we've just completed the entire MongooseIM configuration.
All we need to do now is to set up [MongoosePush][].

### Starting [MongoosePush][]

The easiest way to start [MongoosePush][] is using its [docker image](https://hub.docker.com/r/mongooseim/mongoose-push).
But before you can set [MongoosePush][] up, you need a _FCM_ application token and/or an _APNS_ application certificate.
You can get the _FCM_ token [here](https://console.firebase.google.com/) and the easiest way of getting an _APNS_ application certificate is by running [this](https://github.com/fastlane/fastlane/tree/master/pem) script (please note that you need the certificate in `pem` format).

After you get the _FCM_ application token and/or the _APNS_ application certificate, you can prepare to start [MongoosePush][].
Firstly, prepare the following files structure:

* priv/
    * ssl/
        * rest_cert.pem - The REST endpoint certificate
        * rest_key.pem - private key for the REST endpoint certificate
    * apns/
        * prod_cert.pem - Production APNS app certificate
        * prod_key.pem - Production APNS app certificate's private key
        * dev_cert.pem - Development APNS app certificate
        * dev_key.pem - Development APNS app certificate's private key

If your _FCM_ app token is `MY_FCM_SECRET_TOKEN` and you have the `priv` directory with all
certificates in the current directory, start MongoosePush with the following command:

```bash
docker run -v `pwd`/priv:/opt/app/priv \
  -e PUSH_FCM_APP_KEY="MY_FCM_SECRET_TOKEN" \
  -e PUSH_HTTPS_CERTFILE="/opt/app/priv/ssl/rest_cert.pem" \
  -e PUSH_HTTPS_KEYFILE="/opt/app/priv/ssl/rest_key.pem" \
  -it --rm mongooseim/mongoose-push:2.0.0
```

If you don't want to use either _APNS_ or _FCM_, you simply need to pass `PUSH_APNS_ENABLED=0` or
`PUSH_FCM_ENABLED=0` respectively as additional env variables in your `docker run` command.
For more advanced options and configuration please refer to _"Quick start / Configuring"_ in
[MongoosePush][]'s [_README.md_][MongoosePushReadme].

When your [MongoosePush][] docker is up and running, Push Notifications can be used in your
MongooseIM instance.

[MongoosePush]: https://github.com/esl/MongoosePush
[MongoosePushReadme]: https://github.com/esl/MongoosePush/blob/master/README.md
[mod_push_service_mongoosepush]: ../../modules/mod_push_service_mongoosepush.md
