#!/usr/bin/env bash

week=$(date "+%V")
((num=$week - $week % 2))
year=$(date "+%Y")
ssl_sum=$(cat tools/ssl/Makefile tools/ssl/*.cnf | sha1sum | awk '{print $1}')

# Change it once incompatible changes are made in tools/ssl/ directory
CERT_KEY_VERSION=1

# CI_CERT_KEY_VERSION could be set in
# https://app.circleci.com/settings/project/github/esl/MongooseIM/environment-variables
# It takes integer values 1, 2, 3...
# Change it to invalidate cache without making code changes
# There is no way to see the current value in the Circle CI settings though.
# To get the current value, run a CI job and check the output of the "Prepare cache key" task.
echo "${year}-week${num}-${ssl_sum}-${CERT_KEY_VERSION}-${CI_CERT_KEY_VERSION}"
