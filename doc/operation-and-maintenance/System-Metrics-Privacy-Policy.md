# Introduction
MongooseIM system metrics are gathered to analyse the trends and needs of our users, improve MongooseIM, and let us know where to focus our efforts.
This section is devoted to explaining how to customise, read, enable and disable collecting of the system metrics.

# Consent
To ensure transparency, a log message is generated on every MongooseIM node start (unless the metrics service is configured with the `report` option) to show that the functionality is enabled.
The user is being notified that the metrics are gathered and has the right to withdraw the consent at any time without limiting the functionality of the product.
For more information on how to disable this feature, please see the [Services][service_mongoose_system_metrics] section.

# What information is being gathered?
When introducing this feature, it is crucial for us to be fully transparent as to what information is being gathered.
In general, we capture information on how MongooseIM is being used, its version and the chosen feature set.
We only report the names of known modules and APIs that are part of the opensource product. All additional customisations are simply counted without disclosing any specific details.
The user can view all the information that is shared in two different ways. The log file `system_metrics_report.json` contains the most recent report that was sent.
Additionally, the user can configure the Tracking ID to use their own Google Analytics account and have a view of their MongooseIM status in that dashboard.
For more information on how to set up the Tracking ID, please see [How to configure additional and private Tracking ID in Google Analytics][how-to-configure-tracking-id].

The full list of information that is being gathered can be seen below:

* MongooseIM node uptime.
* MongooseIM version.
* Number of nodes that are part of the MongooseIM cluster.
* Generic modules that are part of the opensource project and are in use. Some modules report what database they use as a backend, e.g. [Sample report](#how-a-report-looks-like).
* Number of custom modules - without disclosing any details, we are just curious to see if there are any.
* Number of connected external XMPP components.
* List of configured REST APIs that are part of the opensource project.
* XMPP transport mechanisms like, TCP/TLS, WebSockets or BOSH.
* Geographical Data - Google Analytics is providing several geographical dimensions, such as City, Country, Continent.
These values are derived from the IP address the data was sent from.
See [About Geographical Data](https://support.google.com/analytics/answer/6160484?hl=en) for more details.

# How the information is being used?
The information collected is automatically anonymised before it is being processed any further.
Each MongooseIM is randomly generating a Client ID that is being attached to the reports.
The collected data has only statistical relevance and aims to help us understand the needs of our users.
Knowing how our product is used will allow us to identify the core value it brings to the users. It will point out the direction in which to expand it and show us how to target our further efforts developing it.

# How a report looks like?
A sample report showing metrics for the mod_vcard backends from Google Analytics can be found below.
![System metrics sample report][system_metrics_report]

Based on such report we can see the frequency of different backends being used with mod_vcard.

# How often the metrics are reported?
Metrics are reported first shortly after the system startup and later at regular intervals.
These timers are configurable using the `initial_report` and `periodic_report` parameters.
The default values are 5 minutes for the initial report and 3 hours for the periodic one.
These reporting intervals can be changed depending on the configuration parameters.

# How to configure this service?
This functionality is provided as a "service".
For more details regarding service configuration, please see [Services](../advanced-configuration/Services.md) section.

# How to configure additional and private Tracking ID in Google Analytics?
The data is gathered and forwarded to Google Analytics.
The user can add custom Google Analytics Tracking ID in the MongooseIM configuration and see all incoming events that are related to their own system metrics.
For more details on how to create or sign in to the Google Analytics account, please see [Get Started with Analytics.](https://support.google.com/analytics/answer/1008015?hl=en&ref_topic=3544906)

Tracking ID is a property identification code that all collected data is associated with.
It is determining the destination where the collected data is sent.
To create a new Tracking ID, please follow the steps below:

* Go to the `Admin` tab of your user dashboard.
* Create a new account with `+ Create Account`.
* Add new property with `+ Create Property`.
    * Within the new property go to `Tracking Info > Tracking Code`.
    * Tracking ID can be found in the top left corner of the section and has following format UA-XXXX-Y.

## Example configuration
New Tracking ID can be added to the list of options
```
{service_mongoose_system_metrics, [
                                   report,
                                   {intial_report, 300000},
                                   {periodic_report, 108000000},
                                   {tracking_id, UA-XXXX-Y}
                                  ]
}
```

For more details regarding service configuration, please see [Services](../advanced-configuration/Services.md) section.

# Data Sharing Policy
For more information on how Google Analytics collects and processes data, please see [Google Privacy & Terms](https://policies.google.com/technologies/partner-sites).
Google Analytics is being used due to the ease of host and display reporting information.
We will not share any user specific information with further third parties not mentioned in this document.
Insight of statistical significance regarding our findings from bulk data collected will be shared as a blog post on our website [Erlang-Solutions](https://www.erlang-solutions.com/blog.html).

[system_metrics_report]: system_metrics_report.png
[how-to-configure-tracking-id]: #how-to-configure-additional-and-private-tracking-id-in-google-analytics
[service_mongoose_system_metrics]:../advanced-configuration/Services.md#service_mongoose_system_metrics
