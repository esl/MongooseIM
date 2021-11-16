#!/usr/bin/env bash

set -e

OUT_FILE="$1"
CFG="$(cat .circleci/template.yml)"

MYSQL_CNF=$(cat tools/db_configs/mysql/mysql.cnf | base64)
MYSQL_SQL=$(cat priv/mysql.sql | base64)
MYSQL_SETUP=$(cat tools/docker-setup-mysql.sh | base64)
MIM_CERT=$(cat tools/ssl/mongooseim/cert.pem | base64)
MIM_KEY=$(cat tools/ssl/mongooseim/key.pem | base64)
INJECT_FILES=$(cat tools/inject-files.sh | base64)

CFG=${CFG//__MYSQL_CNF__/$MYSQL_CNF}
CFG=${CFG//__MYSQL_SQL__/$MYSQL_SQL}
CFG=${CFG//__MYSQL_SETUP__/$MYSQL_SETUP}
CFG=${CFG//__MIM_CERT__/$MIM_CERT}
CFG=${CFG//__MIM_KEY__/$MIM_KEY}
CFG=${CFG//__INJECT_FILES__/$INJECT_FILES}

echo "$CFG" > "$OUT_FILE"
