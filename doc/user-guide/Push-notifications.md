## How to set up Push Notifications with MongoosePush

<TODO MongoosePush2 is cool>

MongooseIM server supports push notifications using FCM (**F**irebase **C**loud **M**essaging)
and APNS (**A**pple **P**ush **N**otification **S**ervice) providers.

MongooseIM provides two ways of pushing notifications:

  * Fully compliant with [XEP-0357 Push Notifications][XEP-0357], described [here](Push-notifications-pubsub.md),
  * Simpler version without PubSub, which is described [here](Push-notifications-without-pubsub.md).
  push notifications in the simplest configuration, just send the following stanza:

[XEP-0357]: https://xmpp.org/extensions/xep-0357.html
