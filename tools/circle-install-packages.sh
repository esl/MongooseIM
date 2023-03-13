#!/usr/bin/env bash

set -e

sudo pkill -9 apt-get || true
echo "Acquire::ForceIPv4 'true';" | sudo tee -a /etc/apt/apt.conf.d/99force-ipv4
sudo ./tools/retry.sh apt-get update
sudo ./tools/retry.sh apt-get install $1 -y
