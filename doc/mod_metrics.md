### Module Description
Adding this module will enable gathering various XMPP-related statistics. The complete list can be fetched with HTTP GET request to metrics URL (see [`listen` section of `ejabberd.cfg`](wiki/Listener-modules)). They are also listed in `[MongooseIM root]/apps/ejabberd/src/mod_metrics.erl`, line 50.

Please also check [metrics wiki page](REST-interface-to-folsom-metrics) for more information.

### Options

none

### Example configuration

` {mod_metrics, []} `