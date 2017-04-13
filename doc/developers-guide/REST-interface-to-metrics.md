## Introduction

To expose MongooseIM metrics, an adequate endpoint must be included in the Cowboy HTTP listener section.

Here's how it can look:
```
...
{ {5288, "127.0.0.1"}, ejabberd_cowboy, [
    ...
    {modules, [
        {"localhost", "/api", [{handlers, [mongoose_api_metrics]}]}
    ]}
]}
...
```

### Security notice

An auth mechanism for the HTTP API has not yet been made available.
That's why we recommend to expose this API only using a private interface or a port hidden behind a firewall to limit the access to the API.
The above configuration starts the API only on a loopback interface.

## Response format

Currently the web-service responses are composed in a JSON format with a root element containing one or more attributes as response elements.

Example response:

    {
        "hosts": [
            "localhost"
        ],
        "metrics": [
            "xmppErrorIq",
            "xmppPresenceReceived",
            "xmppMessageBounced",
            "xmppPresenceSent",
            "modRegisterCount",
            "xmppIqReceived",
            "modPrivacyStanzaBlocked",
            "modPrivacySetsDefault",
            "xmppStanzaCount",
            "xmppStanzaSent",
            "xmppStanzaDropped",
            "xmppStanzaReceived",
            "xmppMessageSent",
            "xmppIqSent",
            "xmppMessageReceived",
            "xmppErrorBadRequest",
            "sessionCount",
            "modUnregisterCount",
            "xmppIqTimeouts",
            "sessionAuthAnonymous",
            "modPresenceSubscriptions",
            "modRosterGets",
            "sessionSuccessfulLogins",
            "modPresenceUnsubscriptions",
            "sessionLogouts",
            "modPrivacyGets",
            "modRosterSets",
            "modPrivacySets",
            "sessionAuthFails",
            "modPrivacyStanzaAll",
            "xmppErrorTotal",
            "xmppErrorMessage",
            "xmppErrorPresence",
            "modPrivacySetsActive",
            "modPrivacyPush",
            "modRosterPush"
        ],
        "global": [
            "nodeSessionCount",
            "totalSessionCount",
            "uniqueSessionCount"
        ]
    }

## Services

### GET /api/metrics

Returns ```200 OK``` and two elements:

* "hosts" containing a list of XMPP host names available on the server,
* "metrics" containing a list of metrics available on the server.

### GET /api/metrics/all

Returns ```200 OK``` and an element:

* "metrics" containing a list of summed metrics for all server hosts.

### GET /api/metrics/all/:metric

On success returns ```200 OK``` and an element:

* "metric" containing a metric :metric that summs up values for all server hosts.

Returns ```404 Not Found``` when metric :metric couldn't been found.

### GET /api/metrics/host/:host

On success returns ```200 OK``` and an element:

* "metrics" containing a list of metric values for host :host.

Returns ```404 Not Found``` when host :host couldn't been found.

### GET /api/metrics/host/:host/:metric

On success returns ```200 OK``` and an element:

* "metric" containing a metric :metric value for host :host.

Returns ```404 Not Found``` when the pair (host :host, metric :metric) couldn't been found.

### GET /api/metrics/global

On success returns ```200 OK``` and an element:

* "metrics" containing all global metrics.

### GET /api/metrics/global/:metric

On success returns ```200 OK``` and an element:

* "metric" containing a global metric :metric.

Returns ```404 Not Found``` when metric :metric couldn't been found.


## collectd integration

The interface is compatible with the collectd curl_json plugin.
Data fetched by collectd may be later visualized by tools like Graphite.

Here's an example of a collectd configuration entry that will fetch all available metrics for a given host:
```
LoadPlugin curl_json
...
<Plugin curl_json>
    <URL "http://<MONGOOSEIM HOST>:<MONGOOSEIM HTTP LISTENER PORT>/api/metrics/host/<XMPP HOST>">
        Instance "mongooseim"
        <Key "metrics/sessionCount/value">
            Type "absolute"
        </Key>
        <Key "metrics/*/count">
            Type "absolute"
        </Key>
        <Key "metrics/*/one">
            Type "absolute"
        </Key>
    </URL>
</Plugin>
```
