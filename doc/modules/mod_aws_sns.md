### Module Description
This module enables support for the Amazon SNS service. Currently there are 3 available notifications:
* **user presence changed** - Carries the user id (bare jid by default) and a boolean field corresponding to the current user online status.
* **private message sent** - Carries the user ids (both sender and receiver) along with the message body.
* **group message sent** - Carries the user id and the room id (bare jids by default) along with the message body.

All those notifications are sent as a JSON string to Amazon SNS along with custom MessageAttributes (see http://docs.aws.amazon.com/sns/latest/api/API_Publish.html). MessageAttributes can be specified via a plugin module (more details in *Options* section).

Full topics for notifications (ARN as defined in [Amazon Resource Names][aws-arn]) are constructed as `arn:aws:sns:{region}:{account_id}:{topic}` where `{region}` and `{account_id}` are substituted with corresponding values from configuration options. `{topic}` is pulled from configuration option `presence_updates_topic`, `pm_messages_topic` or `muc_messages_topic` based on notification type.


### Options

* **presence_updates_topic** (optional, string, default: unset) - Defines Amazon SNS Topic for presence change notifications. Remove this option to disable those notifications.
* **pm_messages_topic** (optional, string, default: unset) - Defines Amazon SNS Topic for private messages notifications. Remove this option to disable those notifications.
* **token_muc_messages_topic** (optional, string, default: unset) - Defines Amazon SNS Topic for group messages notifications. Remove this option to disable those notifications.
* **plugin_module** (atom, default: 'mod_aws_sns_defaults') - Sets a callback module used for creating user's GUID used in notifications (from user's JID) and for defining custom attributes attached to a published SNS message.
* **muc_host** (string, default: "conference.@HOST@") - Messages from this MUC host will be sent to the set SNS topic for MUCs.
* **sns_host** (string, default: unset) - URL to the Amazon SNS service. The URL may be in [virtual host form][aws-virtual-host], and for AWS needs to point at a specific regional endpoint. The scheme, port and path specified in the URL will be used to publish notifications via HTTP POST method.
* **region** (string, default: unset) - The [AWS region][aws-region] to use for requests.
* **access_key_id** (string, default: unset) - [ID of the access key][aws-keys] to use for authorization.
* **secret_access_key** (string, default: unset) - [Secret access key][aws-keys] to use for authorization.
* **account_id** (string, default: unset) - 12 digit number as defined in [AWS Account Identifiers][aws-acct-identifier] to use for creating TopicArn for publishing notifications.
* **pool_size** (integer, default: 100) - Worker pool size for publishing notifications
* **publish_retry_count** (integer, default: 2) - Retry count in case of a publish error
* **publish_retry_time_ms** (integer, default: 50) - Base exponential backoff time (in ms) for publish errors

[aws-acct-identifier]: http://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html
[aws-virtual-host]: https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html
[aws-region]: https://docs.aws.amazon.com/general/latest/gr/rande.html?shortFooter=true#s3_region
[aws-keys]: https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html?shortFooter=true#access-keys-and-secret-access-keys
[aws-arn]: http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html

### Example configuration

```Erlang
{mod_aws_sns, [
    {access_key_id, "AKIAIOSFODNN7EXAMPLE"},
    {secret_access_key, "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"},
    {region, "eu-west-1"},
    {account_id, "123456789012"},
    {sns_host, "sns.eu-west-1.amazonaws.com"},
    {plugin_module, mod_aws_sns_defaults},
    {presence_updates_topic, "user_presence_updated"},
    {pm_messages_topic, "user_message_sent"},
    {muc_messages_topic, "user_messagegroup_sent"},
    {pool_size, 100},
    {publish_retry_count, 2}, 
    {publish_retry_time_ms, 50}
   ]}.
```
