### Module Description
This module is a generic interface for flexible packet filtering by server policy.


### Options

none

### Example configuration

```
  {mod_filter, []},
```

The configuration of rules is done using mim's ACL and ACCESS, so you should also study the corresponding section on ejabberd guide.

default ACCESS configuration:
```
{access, mod_filter, [{allow, all}]}.
{access, mod_filter_presence, [{allow, all}]}.
{access, mod_filter_message, [{allow, all}]}.
{access, mod_filter_iq, [{allow, all}]}.
```

For more example refer to mod_filter_SUITE in big_tests or to origin [code].

[code]: https://github.com/knobo/mod_filter