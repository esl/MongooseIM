#!/usr/bin/env bash

if ls dev/mongooseim_*/etc/ejabberd.cfg.bak 1> /dev/null 2>&1; then
    echo "Unrestored config exists. Your actions:"
    echo "1. Stop nodes"
    echo "2. Run \"make devrel\" again to restore original configs"
    echo "3. Start nodes"
    echo "4. Run the test again"
    exit 1
fi
