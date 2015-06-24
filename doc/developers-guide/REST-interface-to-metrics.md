## Introduction


To expose MongooseIM metrics, an adequate endpoint must be included in the cowboy HTTP listener section.

It may look as follows:
```
...
{ 5280, ejabberd_cowboy, [
    ...
    {modules, [
        %% Modules used here should also be listed in the MODULES section.
        ...
        {"localhost", "/api", [{handlers, [mongoose_api_metrics]}]}
    ]}
]}
...
```

## Response format

Currently web-services responses are composed in JSON format with root element containing one or more attributes as response elements.

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

* "hosts" containing list of XMPP available on the server
* "metrics" containing list of metrics available on the server.

### GET /api/metrics/all

Returns ```200 OK``` and an element:

* "metrics" containing list of metrics where values are summed for all server hosts

### GET /api/metrics/all/:metric

Returns ```200 OK``` and an element on success:

* "metric" containing metric :metric value that is summed for all server hosts.

Returns ```404 Not Found``` when metric :metric couldn't been found.

### GET /api/metrics/host/:host

Returns ```200 OK``` and an element on success:

* "metrics" containing list of metrics values for host :host.

Returns ```404 Not Found``` when host :host couldn't been found.

### GET /api/metrics/host/:host/:metric

Returns ```200 OK``` and an element on success:

* "metric" containing metric :metric value for host :host.

Returns ```404 Not Found``` when pair (host :host, metric :metric) couldn't been found.

### GET /api/metrics/global

Returns ```200 OK``` and an element on success:

* "metrics" containing all global metrics.

### GET /api/metrics/global/:metric

Returns ```200 OK``` and an element on success:

* "metric" containing global metric :metric.

Returns ```404 Not Found``` when metric :metric couldn't been found.


## collectd integration
The interface is compatible with collectd curl_json plugin.
Data fetched by collectd may be later visualized by tools like Graphite.

An example collectd configuration entry may be as follows. It will fetch all available metrics for a given host.
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
