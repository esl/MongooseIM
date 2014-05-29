# MongooseIM 1.3.1

- port XEP-0114 (ejabberd_service)
- port LDAP VCard support
- port LDAP Shared Roster support
- finalize project rename to MongooseIM by changing scripts names

# MongooseIM 1.3.0

- added XEP-0313: Message Archive Management support (`mod_mam`)
- port LDAP authentication from ejabberd
- added PostgreSQL support
- disable SSL 2.0 support
- disable old unsafe ciphers
- make the repo includable as a rebar dependency
- use `#xmlel{}` instead of `{xmlel, ...}` in the whole codebase


# MongooseIM 1.2.0

- BOSH support (XMPP over HTTP) 
- WSS (WebSocekt Secure) 
- various XMPP related metrics exposed via REST API
  (https://github.com/esl/ejabberd/wiki/REST-interface-to-folsom-metrics) 
- alarm handler for better monitoring and bottleneck finding 

We've also assured compatibility with the latest ejabberd Community Edition by
ProcessOne, so that backporting ejabberd modules to MongooseIM requires
less effort.
