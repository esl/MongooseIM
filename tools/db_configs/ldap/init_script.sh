#!/usr/bin/env bash

set -e

# LDAP does not like when its entrypoint is overwritten
# (it has a lot of init magic, so we need to do few fixes)

# Fixes blocking to start by policy-rc.d
printf '#!/bin/sh\nexit 0' > /usr/sbin/policy-rc.d

# Fixes failed to find runlevel
printf '#!/bin/sh\necho 3' > /sbin/runlevel
chmod 755 /sbin/runlevel

# Fixes "*** /container/run/startup/slapd failed with status 1" after "Start OpenLDAP..."
sed -i "s/log-helper level ge debug/log-helper level ge info/g" /container/service/slapd/startup.sh
