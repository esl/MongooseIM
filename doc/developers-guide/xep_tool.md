# XEP-tool usage

The XEP-tool is the answer for developers who wonder how to maintain an actual list of supported XEPs.
It's a fast and easy way to automatically produce documentation from raw, beam files.
This is a quick guide on how to enjoy the usage of the XEP-tool.

###  Sign your module file first

The architecture of MongooseIM determines that almost every XEP or feature implementation resides in its own file. 
It is not strictly enforced but usually the file is named with a `mod_` prefix. 
For example `mod_privacy` file implements [XEP-0016: Privacy Lists](http://xmpp.org/extensions/xep-0016.html).

#### Mandatory `xep` and `version`

In order to let the XEP-tool know about your module, we add a special attribute `xep` at the begining of the `mod_privacy` module:

```
-xep([{xep, 16}, {version, "1.6"}]).
```

Now we know that this module implements to [XEP-0016: Privacy Lists](http://xmpp.org/extensions/xep-0016.html) with version 1.6.

It gives the tool enough information to generate a URL to the XEP homepage.
There are also some variations of the `xep` attribute like:

You ought to remember to specify `xep` and `version` attributes every time.
You can also put several `xep` attributes in one module.
For example `mod_mam_muc` implements attributes of [XEP-0313: Message Archive Management](http://xmpp.org/extensions/xep-0313.html) and also [XEP-0045: Multi-User Chat](http://xmpp.org/extensions/xep-0045.html).

Just list them one after another:

```erlang
-xep([{xep, 45}, {version, "1.25"}]).
-xep([{xep, 313}, {version, "0.5.1"}]).
```

#### Specific URL

```
-xep([{xep, 16}, {version, "1.6"}, {url, "http://xmpp.org/extensions/xep-0016.html"}]).
```

#### Comment

```
-xep([{xep, 16}, {version, "1.6"}, {comment, "Example comment: Partial Implemented"}]).
```

And the XEP-tool will do the work!

### Compile and run

You've just finished marking your modules. 
The only thing left is to `make compile` MongooseIM in order to generate the .beam files.
To run the XEP tool, you must issue an additional subcommand. 
There are two choices:

* `markdown`: to produce a markdown list of supported XEPs. 
 This option also needs an output file as an argument.
* `list`: to print out supported XEPs to the console.

For example, to run our XEP-tool with a `markdown` command, type:

`make xeplist`

Or do it manually:

`$MONGOOSEIM_ROOT/tools/xep_tool/xep_tool.escript markdown <PATH_TO_EBIN> <OPTIONAL_OUTPUT_FILE>`

In our case, from MongooseIM root directory:

`./tools/xep_tool/xep_tool.escript markdown ebin list.md`

The Markdown list with unique XEP names and URLs is saved to file `list.md`
You can copy-paste the content of this file to your main README file.

### Generated file example

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
