# XEP-tool usage

The XEP-tool is the answer for developers who wonder how to maintain an actual list of supported XEPs.
It's a fast and easy way to automatically produce documentation from raw, beam files.
This is a quick guide on how to enjoy the usage of the XEP-tool.

##  Sign your module file first

The architecture of MongooseIM determines that almost every XEP or feature implementation resides in its own file.
It is not strictly enforced but usually the file is named with a `mod_` prefix.
For example `mod_privacy` file implements [XEP-0016: Privacy Lists](http://xmpp.org/extensions/xep-0016.html).

### Mandatory `xep` and `version`

In order to let the XEP-tool know about your module, we add a special attribute `xep` at the beginning of the `mod_privacy` module:

```erlang
-xep([{xep, 16}, {version, "1.6"}]).
```

Now we know that this module implements to [XEP-0016: Privacy Lists](http://xmpp.org/extensions/xep-0016.html) with version 1.6.

It gives the tool enough information to generate a URL to the XEP homepage.
If the module implements a ProtoXEP, the `xep` property should be an atom:

```erlang
-xep([{xep, 'muc-light'}, {version, "0.0.1"}]).
```

You ought to remember to specify `xep` and `version` properties every time.
You can also put several `xep` attributes in one module.
For example `mod_roster` implements attributes of [XEP-0083: Nested Roster Groups](https://xmpp.org/extensions/xep-0083.html), [XEP-0093: Roster Item Exchange](https://xmpp.org/extensions/xep-0093.html) and [XEP-0237: Roster Versioning](https://xmpp.org/extensions/xep-0237.html).
Just list them one after another:

```erlang
-xep([{xep, 237}, {version, "1.3"}]).
-xep([{xep, 83}, {version, "1.0"}]).
-xep([{xep, 93}, {version, "1.2"}]).
```

### Status

By default the status is `complete`. You can also mark the implementation status as `partial`:

```erlang
-xep([{xep, 79}, {version, "1.2"}, {status, partial}]).
```

### Legacy versions

Sometimes the implementation is backwards-compatible, supporting legacy namespaces defined in a particular historical version. You can list such versions as below:

```erlang
-xep([{xep, 313}, {version, "0.6"}, {legacy_versions, ["0.5"]}]).
```

!!! Warning
    Watch out for conflicts! Put the `xep` attribute in the main module implementing the extension to avoid conflicting declarations.
    If you need to specify the same XEP multiple times (e.g. because the implementation is split into two parts),
    make sure that the `version` properties are the same for all attributes - otherwise the XEP tool will report an error.
    The resulting status for a XEP is `complete` unless all implementing modules have the `partial` status.

## Compile and run

After annotating all modules with the `xep` attributes, you need to run `make` in order to generate the .beam files.
Next, you can run the XEP tool. It has a mandatory argument, which specifies the output format:

* `markdown` - a Markdown table,
* `list` - an Erlang list,
* `doap` - [Description Of A Project](https://xmpp.org/extensions/xep-0453.html).

For example, to print out the DOAP, you can run the following from the MongooseIM project directory:

```bash
tools/xep_tool/xep_tool.escript doap
```

To save the output to a file, you can just provide the file name as the second argument.

```bash
tools/xep_tool/xep_tool.escript doap doc/mongooseim.doap
```

The last command has a shortcut in the Makefile:

```bash
make xeplist
```

## Generated markdown file example

|||||
| ------------- | ------------- | ------------- |------------- |
|[XEP-0012: Last Activity](http://www.xmpp.org/extensions/xep-0012.html) | [XEP-0016: Privacy Lists](http://www.xmpp.org/extensions/xep-0016.html) | [XEP-0018: Invisible Presence](http://www.xmpp.org/extensions/xep-0018.html) | [XEP-0022: Message Events](http://www.xmpp.org/extensions/xep-0022.html) |
[XEP-0023: Message Expiration](http://www.xmpp.org/extensions/xep-0023.html) | [XEP-0030: Service Discovery](http://www.xmpp.org/extensions/xep-0030.html) | [XEP-0045: Multi-User Chat](http://www.xmpp.org/extensions/xep-0045.html) | [XEP-0049: Private XML Storage](http://www.xmpp.org/extensions/xep-0049.html) |
[XEP-0050: Ad-Hoc Commands](http://www.xmpp.org/extensions/xep-0050.html) | [XEP-0054: vcard-temp](http://www.xmpp.org/extensions/xep-0054.html) | [XEP-0055: Jabber Search](http://www.xmpp.org/extensions/xep-0055.html) | [XEP-0059: Result Set Management](http://www.xmpp.org/extensions/xep-0059.html) |
[XEP-0068: Field Standardization for Data Forms](http://www.xmpp.org/extensions/xep-0068.html) | [XEP-0077: In-Band Registration](http://www.xmpp.org/extensions/xep-0077.html) | [XEP-0078: Non-SASL Authentication](http://www.xmpp.org/extensions/xep-0078.html) | [XEP-0079: Advanced Message Processing](http://www.xmpp.org/extensions/xep-0079.html) |
[XEP-0082: XMPP Date and Time Profiles](http://www.xmpp.org/extensions/xep-0082.html) | [XEP-0083: Nested Roster Groups](http://www.xmpp.org/extensions/xep-0083.html) | [XEP-0085: Chat State Notifications](http://www.xmpp.org/extensions/xep-0085.html) | [XEP-0086: Error Condition Mappings](http://www.xmpp.org/extensions/xep-0086.html) |
[XEP-0093: Roster Item Exchange](http://www.xmpp.org/extensions/xep-0093.html) | [XEP-0114: Jabber Component Protocol](http://www.xmpp.org/extensions/xep-0114.html) | [XEP-0124: Bidirectional-streams Over Synchronous HTTP (BOSH)](http://www.xmpp.org/extensions/xep-0124.html) | [XEP-0126: Invisibility](http://www.xmpp.org/extensions/xep-0126.html) |
[XEP-0138: Stream Compression](http://www.xmpp.org/extensions/xep-0138.html) | [XEP-0157: Contact Addresses for XMPP Services](http://www.xmpp.org/extensions/xep-0157.html) | [XEP-0160: Best Practices for Handling Offline Messages](http://www.xmpp.org/extensions/xep-0160.html) | [XEP-0170: Recommended Order of Stream Feature Negotiation](http://www.xmpp.org/extensions/xep-0170.html) |
[XEP-0175: Best Practices for Use of SASL ANONYMOUS](http://www.xmpp.org/extensions/xep-0175.html) | [XEP-0198: Stream Management](http://www.xmpp.org/extensions/xep-0198.html) | [XEP-0199: XMPP Ping](http://www.xmpp.org/extensions/xep-0199.html) | [XEP-0202: Entity Time](http://www.xmpp.org/extensions/xep-0202.html) |
[XEP-0206: XMPP Over BOSH](http://www.xmpp.org/extensions/xep-0206.html) | [XEP-0212: XMPP Basic Server 2008](http://www.xmpp.org/extensions/xep-0212.html) | [XEP-0237: Roster Versioning](http://www.xmpp.org/extensions/xep-0237.html) | [XEP-0279: Server IP Check](http://www.xmpp.org/extensions/xep-0279.html) |
[XEP-0280: Message Carbons](http://www.xmpp.org/extensions/xep-0280.html) | [XEP-0313: Message Archive Management](http://xmpp.org/extensions/xep-0313.html) |
