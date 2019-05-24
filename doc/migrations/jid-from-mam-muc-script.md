## The purpose of sender-jid-from-mam-message.escript

This script may be used as a part of migration from MongooseIM 3.3.0 (or older).
It is able to extract a JID of a groupchat message sender from an XML payload.
This piece of information is essential for GDPR commands (retrieve data and remove user) to work properly, as without it the operations on MAM MUC data in DB would be extremely inefficient.

Please consult "3.3.0 to..." migration guide for details.
DB-specific sections describe where the payloads are stored and what you should do with the extracted JID.

## Requirements

This script may be executed in every \*nix environment which has OTP 19.0 (or newer) installed and `escript` executable is in `PATH`.

It doesn't depend on any MongooseIM code or library, so it may be used as a standalone file.

## How to use?

`sender-jid-from-mam-message.escript (eterm | xml)`

The only parameter required by the script is the input format.

You should use `eterm` if (in MongooseIM config file):

* You haven't set `db_message_format` option for MAM at all.
* `db_message_format` is set to `mam_message_compressed_eterm` or `mam_message_eterm`

You should use the `xml` option if:

* `db_message_format` is set to `mam_message_xml`.

Once started, the script will run in an infinite loop (until killed or interrupted), expecting a stream of inputs.
For every provided payload, a JID will be returned immediately.
All communication with the script is done via `stdio`.

### Input format

For both `eterm` and `xml` mode, the script expects an input in a very similar format.
The high-level overview is:

```
LENGTH\nPAYLOAD
```

* `LENGTH` is the `PAYLOAD` length in bytes; if the data retrieved from a DBMS is a Unicode string, `LENGTH` is equal to the number of bytes used to encode this string
* `PAYLOAD` is a sequence of bytes; if a DBMS returns binary data encoded as hex, then it has to be decoded to raw bytes
* `LENGTH` and `PAYLOAD` are separated with a newline character (ASCII code 10 / 0x0a)

### Output format

The script output format is very similar to the input:

```
LENGTH\nJID
```

* `LENGTH` is the number of bytes in a `JID`
* `JID` is a sequence of bytes, which encodes a Unicode string
* `LENGTH` and `PAYLOAD` are separated with a newline character (ASCII code 10 / 0x0a)

If JID couldn't be extracted for some reason (and it's not a critical error, like I/O failure), the script will continue to work and will return `-1\n`.
It's `-1` for `LENGTH`, followed by a newline character and no `PAYLOAD` part (or 0-length `PAYLOAD` if you like).

## Examples

`tools/migration` folder contains two files: `sender-jid-from-mam-message.example.eterm` and `sender-jid-from-mam-message.example.xml`.
They are input samples for the script and may be used as a reference for the script usage.

You can test them by running:

* `tools/migration/sender-jid-from-mam-message.escript eterm < sender-jid-from-mam-message.example.eterm > out`
* `tools/migration/sender-jid-from-mam-message.escript xml < sender-jid-from-mam-message.example.xml > out`

In both cases the `out` file should have the following content:

```
37
gżegżółka@brzęczyszczykiewicz.pl
```

## Debug

If an environment variable `DEBUG` is set to `1`, the script will store error messages in a `/tmp/script-debug` file.

