# Include the file to add helpers for parallel tasks.
# Example:
# source tools/common-vars.sh
# source tools/parallel.sh

PARALLEL_ENABLED=${PARALLEL_ENABLED-true}

# Add prefix to all lines of the output
function exec_with_prefix
{
  set -eo pipefail
  NAME="$1"
  shift
  if [ "$NAME" = "false" ]; then
    # no prefix
    $@
  else
    # Apply a separate sed filter on stderr
    $@ \
     2> >($SED -e "s/^/$NAME stderr:\t/;" >&2) \
      | $SED -e "s/^/$NAME:\t/;"
  fi
  echo "DONE $NAME"
}

function cleanup_parallel
{
  trap "" INT TERM ERR
  echo "Cleanup parallel tasks $1"
  kill 0
}

function init_parallel
{
  if [ "$PARALLEL_ENABLED" = "true" ]; then
    X=$$
    # Kill background jobs if the user clicks CTRL-C
    trap "cleanup_parallel $1" INT TERM ERR
  fi
}

PARALLEL_PIDS=""

function parallel
{
  if [ "$PARALLEL_ENABLED" = "true" ]; then
    exec_with_prefix $@ &
    PARALLEL_PIDS="$PARALLEL_PIDS $!"
  else
    shift # ignore an argument with a prefix
    $@
  fi
}

function wait_for_parallel
{
  if [ "$PARALLEL_ENABLED" = "true" ]; then
    # https://stackoverflow.com/questions/49513335/bash-wait-exit-on-error-code
    set -e
    werr=0
    err=0
    for pid in $PARALLEL_PIDS; do
      wait $pid || werr=$?
      ! [ $werr = 127 ] || break
      err=$werr
      ## To handle *as soon as* first failure happens uncomment this:
      [ $err = 0 ] || break
    done
    ## If you want to still wait for children to finish before exiting
    ## parent (even if you handle the failed child early) uncomment this:
    #trap 'wait || :' EXIT
    if [ $err = 0 ]; then
      echo "Done $1"
    else
      echo "Failed $1"
      exit $err
    fi
  fi
}
