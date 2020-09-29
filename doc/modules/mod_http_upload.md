### Module Description
This module implements [XEP-0363: HTTP File Upload](https://xmpp.org/extensions/xep-0363.html). 
It enables a service that on user request creates an upload "slot". 
A slot is a pair of URLs, one of which can be used with a `PUT` method to upload a user's file, the other with a `GET` method to retrieve such file.

Currently, the module supports only the [S3][s3] backend using [AWS Signature Version 4](https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html).

### Options

#### `modules.mod_http_upload.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"one_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../../advanced-configuration/Modules/#iq-processing-policies).

#### `modules.mod_http_upload.host`
* **Syntax:** string
* **Default:** `"upload.@HOST@"`
* **Example:** `host = "upload.@HOST@"`

Subdomain for the upload service to reside under. `@HOST@` is replaced with each served domain.

#### `modules.mod_http_upload.backend`
* **Syntax:** non-empty string
* **Default:** `"s3"`
* **Example:** `backend = "s3"`

Backend to use for generating slots. Currently only `"s3"` can be used.

#### `modules.mod_http_upload.expiration_time`
* **Syntax:** non-negative integer
* **Default:** `60`
* **Example:** `expiration_time = 120`

Duration (in seconds) after which the generated `PUT` URL will become invalid.

#### `modules.mod_http_upload.token_bytes`
* **Syntax:** positive integer
* **Default:** `32`
* **Example:** `token_bytes = 32`

Number of random bytes of a token that will be used in a generated URL. 
The text representation of the token will be twice as long as the number of bytes, e.g. for the default value the token in the URL will be 64 characters long.

#### `modules.mod_http_upload.max_file_size`
* **Syntax:** non-negative integer
* **Default:** not set - no size limit
* **Example:** `max_file_size = 10485760`

Maximum file size (in bytes) accepted by the module. Disabled if set to `"undefined"`.

#### `modules.mod_http_upload.s3`
* **Syntax:** Array of TOML tables. See description.
* **Default:** see description
* **Example:** see description

Options specific to [S3][s3] backend.

#### [S3][s3] backend options

##### `s3.bucket_url`
* **Syntax:** non-empty string
* **Default:** none, this option is mandatory
* **Example:** `s3.bucket_url = "https://s3-eu-west-1.amazonaws.com/mybucket"`

A complete URL pointing at the used bucket. The URL may be in [virtual host form][aws-virtual-host], and for AWS it needs to point to a specific regional endpoint for the bucket. The scheme, port and path specified in the URL will be used to create `PUT` URLs for slots, e.g. specifying a value of `"https://s3-eu-west-1.amazonaws.com/mybucket/custom/prefix"` will result in `PUT` URLs of form `"https://s3-eu-west-1.amazonaws.com/mybucket/custom/prefix/<RANDOM_TOKEN>/<FILENAME>?<AUTHENTICATION_PARAMETERS>"`.

##### `s3.add_acl`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `s3.add_acl = true`

If `true`, adds `x-amz-acl: public-read` header to the PUT URL.
This allows users to read the uploaded files even if the bucket is private. The same header must be added to the PUT request.

##### `s3.region`
* **Syntax:** string
* **Default:** `""`
* **Example:** `s3.region = "https://s3-eu-west-1.amazonaws.com/mybucket"`

The [AWS region][aws-region] to use for requests.

##### `s3.access_key_id`
* **Syntax:** string
* **Default:** `""`
* **Example:** `s3.access_key_id = "AKIAIOSFODNN7EXAMPLE"`

[ID of the access key][aws-keys] to use for authorization.

##### `s3.secret_access_key`
* **Syntax:** string
* **Default:** `""`
* **Example:** `s3.secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"`

[Secret access key][aws-keys] to use for authorization.

[s3]: https://aws.amazon.com/s3/
[aws-virtual-host]: https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html
[aws-region]: https://docs.aws.amazon.com/general/latest/gr/rande.html?shortFooter=true#s3_region
[aws-keys]: https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html?shortFooter=true#access-keys-and-secret-access-keys

### Example configuration

```
[modules.mod_http_upload]
  host = "upload.@HOST@"
  backend = "s3"
  expiration_time = 120
  s3.bucket_url = "https://s3-eu-west-1.amazonaws.com/mybucket"
  s3.region = "eu-west-1"
  s3.add_acl = true     
  s3.access_key_id = "AKIAIOSFODNN7EXAMPLE"
  s3.secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
```

### Testing [S3][s3] configuration

Since there is no direct connection between MongooseIM and an [S3][s3] bucket,
it is not possible to verify the provided [S3][s3] credentials during startup.
However, the testing can be done manually. MongooseIM provides a dedicated
`mongooseimctl http_upload` command for the manual URLs generation, it accepts
the following parameters:

* **Host** - XMPP host name.
* **FileName** - The name of the file.
* **FileSize** - The size of the file (positive integer).
* **ContentType** - [Content-Type][Content-Type], optional parameter. If not provided, must be an empty string `""`.
* **ExpirationTime** - Duration (in seconds, positive integer) after which the generated `PUT` URL will become invalid. This parameter shadows the **expiration_time** configuration.

The generated URLs can be used to upload/download a file using the `curl` utility:

```bash
# Create some text file
echo qwerty > tmp.txt

# Get the size of the file
filesize="$(wc -c tmp.txt | awk '{print $1}')"

# Set the content type
content_type="text/plain"

# Generate upload/download URLs
urls="$(./mongooseimctl http_upload localhost test.txt "$filesize" "$content_type" 600)"
put_url="$(echo "$urls" | awk '/PutURL:/ {print $2}')"
get_url="$(echo "$urls" | awk '/GetURL:/ {print $2}')"

# Try to upload a file. Note that if 'add_acl' option is
# enabled, then you must also add 'x-amz-acl' header:
#    -H "x-amz-acl: public-read"
curl -v -T "./tmp.txt" -H "Content-Type: $content_type" "$put_url"

# Try to download a file
curl -i "$get_url"
```

[Content-Type]: https://www.rfc-editor.org/rfc/rfc7231.html#section-3.1.1.5

### Using S3 backend with [min.io][minio]

[min.io][minio] doesn't support [ObjectACL][minio-limits], so enabling `add_acl`
makes no sense. The [bucket policies][bucket-policies] must be used instead,
it is enough to set the bucket policy to `download`.

Please note that there is no error if you keep `add_acl` enabled. [min.io][minio] just
ignores the `x-amz-acl` header. This might be useful to simplify the migration from [S3][s3]
to [min.io][minio]

[minio]: https://min.io
[minio-limits]: https://docs.minio.io/docs/minio-server-limits-per-tenant.html
[bucket-policies]: https://docs.min.io/docs/minio-client-complete-guide#policy

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend action | Description (when it gets incremented) |
| ---- | -------------------------------------- |
| `create_slot` | An upload slot is allocated. |
