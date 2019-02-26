### Module Description
This module implements [XEP-0363: HTTP File Upload](https://xmpp.org/extensions/xep-0363.html). 
It enables a service that on user request creates an upload "slot". 
A slot is a pair of URLs, one of which can be used with a `PUT` method to upload user's file, the other with a `GET` method to retrieve the file.

Currently, the module supports only the S3 backend using [AWS Signature Version 4](https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html).

### Options

* **iqdisc** (default: `one_queue`)
* **host** (string, default: `"upload.@HOST@"`): Subdomain for the upload service to reside under. `@HOST@` is replaced with each served domain.
* **backend** (atom, default: `s3`) - Backend to use for generating slots. Currently only `s3` can be used.
* **expiration_time** (integer, default: `60`) - Duration (in seconds) after which the generated `PUT` URL will become invalid.
* **token_bytes** (integer, default: `32`) - Number of random bytes of a token that will be used in a generated URL. 
 The text representation of the token will be twice as long as the number of bytes, e.g. for the default value the token in URL will be 64 characters long.
* **max_file_size** (integer, default: 10485760 (10 MB)) - Maximum file size (in bytes) accepted by the module. Disabled if set to `undefined`.
* **s3** (list, default: unset) - Options specific to S3 backend.

#### S3 backend options

* **bucket_url** (string, default: unset) - A complete URL pointing at the used bucket. The URL may be in [virtual host form][aws-virtual-host], and for AWS needs to point at a specific regional endpoint for the bucket. The scheme, port and path specified in the URL will be used to create `PUT` URLs for slots, e.g. specifying a value of `"https://s3-eu-west-1.amazonaws.com/mybucket/custom/prefix"` will result in `PUT` URLs of form `"https://s3-eu-west-1.amazonaws.com/mybucket/custom/prefix/<RANDOM_TOKEN>/<FILENAME>?<AUTHENTICATION_PARAMETERS>"`.
* **add_acl** (boolean, default: `true`) - If `true`, adds `x-amz-acl=public-read` parameter to the PUT request.
This allows users to read the uploaded files even if the bucket is private.
* **region** (string, default: unset) - The [AWS region][aws-region] to use for requests.
* **access_key_id** (string, default: unset) - [ID of the access key][aws-keys] to use for authorization.
* **secret_access_key** (string, default: unset) - [Secret access key][aws-keys] to use for authorization.

[aws-virtual-host]: https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html
[aws-region]: https://docs.aws.amazon.com/general/latest/gr/rande.html?shortFooter=true#s3_region
[aws-keys]: https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html?shortFooter=true#access-keys-and-secret-access-keys

### Example configuration

```Erlang
{mod_http_upload, [
        {host, "upload.@HOST@"},
        {backend, s3},
        {expiration_time, 120},
        {s3, [
              {bucket_url, "https://s3-eu-west-1.amazonaws.com/mybucket"},
              {region, "eu-west-1"},
              {access_key_id, "AKIAIOSFODNN7EXAMPLE"},
              {secret_access_key, "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"}
             ]}
       ]}.
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend action | Description (when it gets incremented) |
| ---- | -------------------------------------- |
| `create_slot` | An upload slot is allocated. |

