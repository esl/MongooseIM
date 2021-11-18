#!/usr/bin/env bash

set -e
printf '#!/bin/sh\nexit 0' > /usr/sbin/policy-rc.d
printf '#!/bin/sh\necho 3' > /sbin/runlevel
chmod 755 /sbin/runlevel
