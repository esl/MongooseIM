## Module Description

This module is a backend of [mod_event_pusher] that enables support for the Amazon SNS service. Currently there are 3 available notifications:

* **user presence changed** - Carries the user id (bare jid by default) and a boolean field corresponding to the current user online status.
* **private message sent** - Carries the user ids (both sender and receiver) along with the message body.
* **group message sent** - Carries the user id and the room id (bare jids by default) along with the message body.

All these notifications are sent as a JSON string to Amazon SNS along with custom MessageAttributes (see http://docs.aws.amazon.com/sns/latest/api/API_Publish.html). MessageAttributes can be specified via a plugin module (more details in *Options* section).

Full topics for notifications (ARN as defined in [Amazon Resource Names][aws-arn]) are constructed as `arn:aws:sns:{region}:{account_id}:{topic}` where `{region}` and `{account_id}` are substituted with corresponding values from configuration options. `{topic}` is pulled from configuration option `presence_updates_topic`, `pm_messages_topic` or `muc_messages_topic` based on the notification type.

## Options

### `modules.mod_event_pusher.sns.presence_updates_topic`
* **Syntax:** string
* **Default:** no default is given
* **Example:** `presence_updates_topic = "user_presence_updated"`

Defines Amazon SNS Topic for presence change notifications. Remove this option to disable these notifications.

### `modules.mod_event_pusher.sns.pm_messages_topic`
* **Syntax:** string
* **Default:** no default is given
* **Example:** `pm_messages_topic = "user_message_sent"`

Defines Amazon SNS Topic for private message notifications. Remove this option to disable these notifications.

### `modules.mod_event_pusher.sns.muc_messages_topic`
* **Syntax:** string
* **Default:** no default is given
* **Example:** `muc_messages_topic = "user_messagegroup_sent"`

Defines Amazon SNS Topic for group message notifications. Remove this option to disable these notifications.

### `modules.mod_event_pusher.sns.plugin_module`
* **Syntax:** string
* **Default:** `"mod_event_pusher_sns_defaults"`
* **Example:** `plugin_module = "mod_event_pusher_sns_defaults"`

Sets a callback module used for creating user's GUID used in notifications (from user's JID) and for defining custom attributes attached to a published SNS message.

### `modules.mod_event_pusher.sns.sns_host`
* **Syntax:** string
* **Default:** none, this option is mandatory
* **Example:** `sns_host = "sns.eu-west-1.amazonaws.com"`

URL to the Amazon SNS service. The URL may be in [virtual host form][aws-virtual-host], and for AWS needs to point at a specific regional endpoint. The scheme, port and path specified in the URL will be used to publish notifications via HTTP POST method.

### `modules.mod_event_pusher.sns.region`
* **Syntax:** string
* **Default:** none, this option is mandatory
* **Example:** `region = "eu-west-1"`

The [AWS region][aws-region] to use for requests.

### `modules.mod_event_pusher.sns.access_key_id`
* **Syntax:** string
* **Default:** none, this option is mandatory
* **Example:** `access_key_id = "AKIAIOSFODNN7EXAMPLE"`

[ID of the access key][aws-keys] to use for authorization.

### `modules.mod_event_pusher.sns.secret_access_key`
* **Syntax:** string
* **Default:** none, this option is mandatory
* **Example:** `secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"`

[Secret access key][aws-keys] to use for authorization.

### `modules.mod_event_pusher.sns.account_id`
* **Syntax:** string
* **Default:** none, this option is mandatory
* **Example:** `account_id = "123456789012"`

12 digit number as defined in [AWS Account Identifiers][aws-acct-identifier] to use for creating TopicArn for publishing notifications.

### `modules.mod_event_pusher.sns.pool_size`
* **Syntax:** positive integer
* **Default:** `100`
* **Example:** `pool_size = 100`

Worker pool size for publishing notifications

### `modules.mod_event_pusher.sns.publish_retry_count`
* **Syntax:** non-negative integer
* **Default:** `2`
* **Example:** `publish_retry_count = 2`

Retry count in case of a publish error.

### `modules.mod_event_pusher.sns.publish_retry_time_ms`
* **Syntax:** non-negative integer
* **Default:** `50`
* **Example:** `publish_retry_time_ms = 50`

Base exponential backoff time (in ms) for publish errors.

[aws-acct-identifier]: http://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html
[aws-virtual-host]: https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html
[aws-region]: https://docs.aws.amazon.com/general/latest/gr/rande.html#regional-endpoints
[aws-keys]: https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html?shortFooter=true#access-keys-and-secret-access-keys
[aws-arn]: http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html

## Example configuration

```toml
[modules.mod_event_pusher.sns]
  presence_updates_topic = "user_presence_updated"
  pm_messages_topic = "user_message_sent"
  muc_messages_topic = "user_messagegroup_sent"
  sns_host = "sns.eu-west-1.amazonaws.com"
  region = "eu-west-1"
  access_key_id = "AKIAIOSFODNN7EXAMPLE"
  secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  account_id = "123456789012"
```

## JSON Schema examples

The different kinds of notifications deliver slightly different messages. The messages are delivered in a JSON format.

### Presence updates

The JSON format for an online presence update notification is:
```JSON
{
    "user_id": "alice@localhost",
    "present": true
}
```

For offline presence updates, the `present` boolean value is set to false:

```JSON
{
    "user_id": "alice@localhost",
    "present": false
}
```

### Sent messages

The JSON format for a private message notification is:
```JSON
{
    "to_user_id": "bob@localhost",
    "message": "Hello, Bob",
    "from_user_id": "alice@localhost"
}
```

The notification is similar for group messages except that the `to_user_id` is the recipient room JID. For example:

```JSON
{
    "to_user_id": "muc_publish@muc.localhost",
    "message": "Hi, Everyone!",
    "from_user_id": "bob@localhost"
}
```

[mod_event_pusher]: ./mod_event_pusher.md
