#!/usr/bin/env bash

# Simple hash for our certs
md5sum tools/ssl/mongooseim/key.pem | cut -d " " -f1
