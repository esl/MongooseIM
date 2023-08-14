# Include the file to add helpers for parallel tasks.
# Example:
# source tools/common-vars.sh
# source tools/parallel.sh

PARALLEL_ENABLED=${PARALLEL_ENABLED-true}

# Add prefix to all lines of the output
function exec_with_prefix
{
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

function parallel
{
  if [ "$PARALLEL_ENABLED" = "true" ]; then
    exec_with_prefix $@ &
  else
    shift # ignore an argument with a prefix
    $@
  fi
}

function wait_for_parallel
{
  if [ "$PARALLEL_ENABLED" = "true" ]; then
    wait
    echo "Done $1"
  fi
}
