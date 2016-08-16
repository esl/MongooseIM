### Module Description
This module implements [XEP-0060 (Publish-Subscribe)](http://www.xmpp.org/extensions/xep-0060.html).
It’s a fascinating design pattern which mostly promote loose coupling. In the pubsub world we have publishers who fire events and subscribers who are users who wish to be notified when the publisher sent data.
There might be several subscribers, several publishers and even several channels (topics) where the data is sent.
The functionality of the module might be extended by enabling plugins. The momdule implements also [XEP-0060 (Personal Eventing Protocol)](http://xmpp.org/extensions/xep-0163.html)


### Options

* `iqdisc`

### Example Configuration

```
  {mod_pubsub, []},
```
