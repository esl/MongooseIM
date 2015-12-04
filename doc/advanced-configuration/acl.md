In Mongoose the access control is performed via Access Control Lists (ACL). Initially,
this functionality was supposed to answer the following question: "Is given user
allowed to access a particular resource?". The answer might have been either allow or
deny, but this functionality was extended to return any value. For instance we
can ask how many concurrent session can given user open, and the answer may vary
depending on user affiliation - is he an admin user and a normal user.

The functionality is defined via two top-level options

* `{acl, Name, Pattern}` - contrary to its name, it defines a user or the whole group.
For instance we can define admin users or banned users.
* `{access, Name, List}` - actual ACL that has a name and list of rules and
values returned by them in case of success match.

# ACL tuple

 `{acl, Name, Pattern}` The name could be any atom. This name will be used in the
 access definitions. Note that there might be many acl entries with the same name,
 in that case the result will be a union of all patterns.

### Patterns

Patterns can be defined in one of the following formats:

#### **all**
All users/jids match.

#### **{user, username()}**

All users with a given user name, example: <br/>
`{user, "admin"}` includes `admin@localhost`, `admin@xmpp.org` etc.

#### **{user, username(), server()}**
In this case username and domain have to match example:<br/>
`{user, "admin", "localhost"}` - `admin@localhost` matches,
where`admin@xmpp.org` doesn't.

#### **{server, server()}**
All users from a given domain, example:<br/>
`{server, "localhost"}` `admin@localhost`, `pawel@localhost` match. `pawel@xmpp.org`
doesn't.

#### **{resource, resource()}**
All users with matching resource, example:<br/>
`{resource, "res1"}` - `admin@localhost/res1` matches, `admin@localhost/res2` doesn't.

#### **{user_regexp, regexp()}**
Similar to user, but match is done against regular expression, example:<br/>
`{user_regexp, "^ad.*"}` - `admin@localhost`, `ads@localhost2` match,`pad@localhost` doesn't.

#### **{user_regexp, regexp(), server()}** -
Similar to user, username is matched against regex, server part has to be exactly the same, example:<br/>
`{user_regexp, "^ad.*", "localhost"}` - `admin@localhost` matches, `admin@xmpp.org`
doesn't.

#### **{server_regexp, regexp()}**
Analogous to `user_regexp`, but regex matches on the server part of the jid, example:<br/>
`{server_regexp, "^local.*"}` - `admin@localhost` matches, `admin@relocal` doesn't.

#### **{resource_regexp, regexp()}**
The same story as above, but for the resource part, example:<br/>
`{resource_regexp, "res.*"}` - `admin@localhost/resource` matches, `admin@localhost/ios` doesn't.

#### **{node_regexp, regexp(), regexp()}**
Regexp matching on both username and domain, example:<br/>
`{node_regexp, "ad.*", "loc.*"}` - `admin@localhost` matches, `pawel@xmpp.org` doesn't.

#### **{user_glob, glob()}**
Match on username part using glob style patterns, example:<br/>
`{user_glob, "paw*"}` - `pawel@localhost` matches, `admin@localhost` doesn't.

#### **{user_glob, glob(), server()}**
Match on the username part using glob patterns and on server using exact match, example:<br/>
`{user_glob, "paw*", "localhost"}` - `pawel@localhost` matches, `pawel@xmpp.org` doesn't.

#### **{server_glob, glob()}**
Match on the server part using glob patterns, example:<br/>
`{server_glob, "local*"}` - `pawel@localhost` matches, `pawel@xmpp.org` doesn't.

#### **{resource_glob, glob()}**
Match on the resource part using glob patterns, example:<br/>
`{resource_glob, "r*"}` - `pawel@localhost/res` matches, `pawel@xmpp.org` doesn't.

#### **{node_glob, glob(), glob()}**
Match on the username and server part using glob patterns, example:<br/>
`{node_glob, "paw*", "loc*"}` - `pawel@localhost/res` matches, `pawel@xmpp.org` doesn't.

# Access tuple

Once we have out group of interests grouped in acl tuple, we can use them in actual
access control lists. To do so, we need to specify the following tuple:

`{access, name_of_access_rule, [{value(), acl_name()}]}`

As an example we can discuss 2 rules which are present in the default config file:

* {access, register, [{allow, all}]}.<br/>
This rule is used, while registering a new user, the example above has no restrictions, but
we might want to block certain jids, for instance admin jid. To do so we need to set:<br/>
`{acl, admin, {user, "admin"}}.`, then `{access, register[{admin, deny}, {allow, all}]}.`
* {access, max_user_offline_messages, [{5000, admin}, {100, all}]}.<br/>
This rule is used in mod_offline,it determines mod_offline's buffer size.
For users defined in the admin acl(for example. `{acl, admin, {user, "pawel", "localhost"}}`)
the size is 5000, while normal users size is 10.


# Priority - global vs host access lists

By default, both acl and access elements are "global", they apply to all domains
available on the server. However using the host_config option, we are able to override
rules for a particular domain.
```
%%  Define specific Access Rules in a virtual host.
{host_config, "localhost",
 [
  {access, c2s, [{allow, admin}, {deny, all}]},
  {access, register, [{deny, all}]}
 ]
}.
```

Regards priority, at the moment it works in the following way- the global rule has the highest
priority, however if the global rule ends with `{allow, all}` the host specific rule
is taken into account.

# For developers

To access the ACL functionality one has to use `acl:match_rule/3` function.

Given following ACL:

`{access, register, [{deny, all}]}`

One can call:

`acl:match_rule(<<"localhost">>, register, jlib:make_jid(<<"p">>, <<"localhost">>, <<>>)).`<br/>

Which in our case will return deny.
If the rule is not host specific one can use `global` instead of `<<"localhost">>`.

