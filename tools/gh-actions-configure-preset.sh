#!/bin/bash
##########################################################################################################
## for testing purposes run:
##    GITHUB_ENV=/dev/tty tools/gh-actions-configure-preset.sh internal_mnesia TESTSPEC=default.spec
##    GITHUB_ENV=/dev/tty tools/gh-actions-configure-preset.sh mysql_redis TESTSPEC=default.spec
##
## this script can be also sourced with parameters to set env variables in the current shell:
##    GITHUB_ENV=/dev/null . tools/gh-actions-configure-preset.sh internal_mnesia TESTSPEC=default.spec
##    GITHUB_ENV=/dev/tty source tools/gh-actions-configure-preset.sh mysql_redis TESTSPEC=default.spec
##########################################################################################################

export PRESET="$1"
shift 1

function export_env(){
  [ -z "$GITHUB_ENV" ] && { return; }
  # echo "exporting: $@"
  local env_var
  for env_var in "$@"; do
     export "$env_var"
     echo "$env_var" >> $GITHUB_ENV
  done
}

export_env "$@"

DB_ARRAY=( $(./tools/test_runner/presets_to_dbs.sh "$PRESET" ) )
[ "${#DB_ARRAY[@]}" -gt 0 ] && export DB="${DB_ARRAY[@]}"

case "$PRESET" in
  internal_mnesia)
    export TLS_DIST=true ;;
  *)
    ;;
esac

if [ ! -z "$GITHUB_ENV" ]; then
  ## $PRESET is required for gh-upload-to-s3.sh script
  env | grep -E "^(DB|REL_CONFIG|TLS_DIST|PRESET)=" >> $GITHUB_ENV
fi
