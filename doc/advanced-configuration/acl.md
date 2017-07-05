In MongooseIM access control is performed via Access Control Lists (ACL).
Initially, this functionality was supposed to answer the following question: "Is a given user allowed to access a particular resource?".
The answer was either `allow` or `deny`, but this functionality was extended to return any value.
For instance, we can ask how many concurrent session a given user can open, where the answer may vary depending on user affiliation - are they an admin or a normal user.

The functionality is defined via two top-level options:

* `{acl, Name, Pattern}`: contrary to its name, it defines a user or the whole group.
 For instance we can define admin users or banned users.
* `{access, Name, List}`: an ACL that has a name and list of rules and values returned by them in the case of a successfull match.

# ACL tuple

 `{acl, Name, Pattern}`: the name could be any atom.
 This name will be used in the access definitions.
 Note that there might be many ACL entries with the same name, in this case the result will be the union of all patterns.

### Patterns

This sections describes all possible ACL patterns.
Some of them use `re` syntax for regular expressions and some accept `glob` syntax.

* [`re` definitions](http://erlang.org/doc/man/re.html#id253353)
* [`glob` definitions](https://en.wikipedia.org/wiki/Glob_(programming))

Patterns can be defined in one of the following formats:

#### **all**
All users/JIDs match.

#### **{user, username()}**

All users with a given user name: <br/>
`{user, "admin"}`: includes `admin@localhost`, `admin@xmpp.org`, etc.

#### **{user, username(), server()}**
In this case the username and the domain have to match:<br/>
`{user, "admin", "localhost"}`: `admin@localhost` matches, but`admin@xmpp.org` doesn't.

#### **{server, server()}**
All users from a given domain:<br/>
`{server, "localhost"}`: `admin@localhost` and `pawel@localhost` match, but `pawel@xmpp.org` doesn't.

#### **{resource, resource()}**
All users with a matching resource:<br/>
`{resource, "res1"}`: `admin@localhost/res1` matches, but `admin@localhost/res2` doesn't.

#### **{user_regexp, username_regexp()}**
Similar to user, but the match is done against a regular expression:<br/>
`{user_regexp, "^ad.*"}`: `admin@localhost` and `ads@localhost2` match, but `pad@localhost` doesn't.

#### **{user_regexp, username_regexp(), server()}**
Similar to user, the username is matched against regex, the server part has to be exactly the same:<br/>
`{user_regexp, "^ad.*", "localhost"}`: `admin@localhost` matches, but `admin@xmpp.org` doesn't.

#### **{server_regexp, server_regexp()}**
Analogous to `user_regexp`, but regex matches on the server part of the JID:<br/>
`{server_regexp, "^local.*"}`: `admin@localhost` matches, but `admin@relocal` doesn't.

#### **{resource_regexp, resource_regexp()}**
The same story as above, but for the resource part:<br/>
`{resource_regexp, "res.*"}`: `admin@localhost/resource` matches, but `admin@localhost/ios` doesn't.

#### **{node_regexp, username_regexp(), server_regexp()}**
Regexp matching on both the username and the domain:<br/>
`{node_regexp, "ad.*", "loc.*"}`: `admin@localhost` matches, but `pawel@xmpp.org` doesn't.

#### **{user_glob, username_glob()}**
Match on the username part using glob style patterns:<br/>
`{user_glob, "paw*"}`: `pawel@localhost` matches, but `admin@localhost` doesn't.

#### **{user_glob, username_glob(), server()}**
Match on the username part using glob patterns and on the server using exact match:<br/>
`{user_glob, "paw*", "localhost"}`: `pawel@localhost` matches, but `pawel@xmpp.org` doesn't.

#### **{server_glob, server_glob()}**
Match on the server part using glob patterns:<br/>
`{server_glob, "local*"}`: `pawel@localhost` matches, but `pawel@xmpp.org` doesn't.

#### **{resource_glob, resource_glob()}**
Match on the resource part using glob patterns:<br/>
`{resource_glob, "r*"}`: `pawel@localhost/res` matches, but `pawel@xmpp.org` doesn't.

#### **{node_glob, username_glob(), server_glob()}**
Match on the username and the server part using glob patterns:<br/>
`{node_glob, "paw*", "loc*"}`: `pawel@localhost/res` matches, but `pawel@xmpp.org` doesn't.

# Access tuple

Once we have our group of interest gathered in an ACL tuple, we can use them in an access control list.
To do so, we need to specify a tuple like the following:

`{access, name_of_access_rule, [{value(), acl_name()}]}`

As an example we can discuss 2 rules which are present in the default config file:

* `{access, register, [{allow, all}]}`.<br/>
This rule is used while registering a new user, the example above has no restrictions, but we might want to block certain JIDs, `admin` JID for instance.
To do so we need to set:<br/>
`{acl, admin, {user, "admin"}}.`, then `{access, register, [{deny, admin}, {allow, all}]}.`
* `{access, max_user_offline_messages, [{5000, admin}, {100, all}]}`.<br/>
This rule is used in `mod_offline`, it determines `mod_offline`'s storage limit.
For users defined in the admin ACL (for example `{acl, admin, {user, "pawel", "localhost"}}`) the size is 5000 messages, while the size for normal user is 10.


# Priority: global vs host access lists

By default, both ACL and access elements are "global", so they apply to all domains available on the server.
However using the `host_config` option, we are able to override the rules for a particular domain.

```
%%  Define specific Access Rules in a virtual host.
{host_config, "localhost",
 [
  {access, c2s, [{allow, admin}, {deny, all}]},
  {access, register, [{deny, all}]}
 ]
}.
```

The global rule has the highest priority, however if the global rule ends with `{allow, all}` the host specific rule is taken into account.

# For developers

To access the ACL functionality, one has to use the `acl:match_rule/3` function.

Given the following ACL:

`{access, register, [{deny, all}]}`

One can call:

`acl:match_rule(<<"localhost">>, register, jid:make(<<"p">>, <<"localhost">>, <<>>)).`

Which in our case will return deny.
If the rule is not host specific, one can use `global` instead of `<<"localhost">>`.

