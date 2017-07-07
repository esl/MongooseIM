## Overview

TODO

## Configuration options

* **ldap_base:**
    * **Description:**  LDAP base directory which stores users accounts.
    * **Values:** String
    * **Default:** This option is required

* **ldap_uids:**
    * **Description:**  LDAP attribute which holds a list of attributes to use as alternatives for getting the JID.
   The attributes are of the form: `[{ldap_uidattr}]` or `[{ldap_uidattr, ldap_uidattr_format}]`. You can use as many comma separated attributes as needed.
    * **Values** `[ ldap_uidattr | {ldap_uidattr: ldap_uidattr_format} ]`
    The values for `ldap_uidattr` and `ldap_uidattr_format` are described as follow:
         * **ldap_uidattr:** LDAP attribute which holds the user’s part of a JID. The default value is `uid`
         * **ldap_uidattr_format:**  Format of the `ldap_uidattr` variable. The format must contain one and only one pattern variable `%u` which will be replaced by the user’s part of a JID. For example, `%u@example.org`. The default value is `%u`.
    * **Default**  `[{uid, %u}]`

* **ldap_filter:**
    * **Description:** LDAP filter. Please, do not forget to close brackets and do not use superfluous whitespaces.
    Also you must not use `ldap_uidattr` attribute in filter because this attribute will be substituted in LDAP filter automatically.
    * **Values:** String. For example:

                                      (&(objectClass=shadowAccount)(memberOf=Jabber Users))

    * **Default:** `undefined`

* **ldap_dn_filter:**
    * **Description:**  This filter is applied on the results returned by the main filter.
    This filter performs additional LDAP lookup to make the complete result. This is useful when you are unable to define all filter rules in ldap_filter.
    You can define `%u`, `%d`, `%s` and `%D` pattern variables in the filter: `%u` is replaced by a user’s part of a JID, `%d` is replaced by the corresponding domain (virtual host), all `%s` variables are consecutively replaced by values of FilterAttrs attributes and `%D` is replaced by Distinguished Name.
    Since this filter makes additional LDAP lookups, use it only as the last resort: try to define all filter rules in ldap_filter if possible.
    * **Values:** `{Filter, [FilterAttributes]}`. For example:

                              (&(name=%s)(owner=%D)(user=%u@%d))": ["sn"]

    * **Default:** `undefined`

* **ldap_local_filter:**
    * **Description:** If you can’t use ldap_filter due to performance reasons (the LDAP server has many users registered), you can use this local filter.
    The local filter checks an attribute in MongooseIM, not in LDAP, so this limits the load on the LDAP directory.
    * **Values:** `Filter`. Example values:

                              {ldap_local_filter, {notequal, {"accountStatus",["disabled"]}}}.
                              {ldap_local_filter, {equal, {"accountStatus",["enabled"]}}}.
                              {ldap_local_filter, undefined}.
    * **Default:** `undefined`
