# To enable autocompletion:
# source tools/run-all-tests-complete.sh
if [ ! -z "$ZSH_NAME" ]; then
    # zsh sets $ZSH_NAME variable so it can be used to detect zsh
    # following enables using bash-completion under zsh
    autoload bashcompinit
    bashcompinit
fi

_run_all_tests() {
  local cur pos opt
  # Pointer to current completion word.
  # By convention, it's named "cur" but this isn't strictly necessary.

  COMPREPLY=()   # Array variable storing the possible completions.

  pos=$COMP_CWORD

  until [[ "$pos" == "0" ]] || [[ "${COMP_WORDS[$pos]}" == -* ]]; do
      pos=$(($pos-1))
  done

  cur=${COMP_WORDS[$COMP_CWORD]}
  opt=${COMP_WORDS[$pos]}

  # If current is option - we don't care about option expantion
  case "$cur" in
    --*)
      opt=""
  esac

  case "$opt" in
    --db*)
    # Suggest a list of supported databases
    # -- is to start typing another parameter
    # Read command output into array
    # Each element is a line
    ARRAY=( $(./tools/run-all-tests.sh --list-dbs) )
    # Concat array using whitespaces
    COMPREPLY=( $( compgen -W "${ARRAY[*]} --" -- $cur ) );;

    --preset*)
    ARRAY=( $(./tools/run-all-tests.sh --list-presets) )
    COMPREPLY=( $( compgen -W "${ARRAY[*]} --" -- $cur ) );;

    --dev-nodes*)
    ARRAY=( $(./tools/run-all-tests.sh --list-dev-nodes) )
    COMPREPLY=( $( compgen -W "${ARRAY[*]} --" -- $cur ) );;

    --test-hosts*)
    ARRAY=( $(./tools/run-all-tests.sh --list-test-hosts) )
    COMPREPLY=( $( compgen -W "${ARRAY[*]} --" -- $cur ) );;

    *)

    LIST_SUITES_ARGS=""
    if [[ " ${COMP_WORDS[@]} " =~ " --no-small-tests " ]]; then
         # if --no-small-tests is a previous option, do not show small-tests
         LIST_SUITES_ARGS+=" --no-small-tests "
    fi
    if [[ " ${COMP_WORDS[@]} " =~ " --no-big-tests " ]]; then
         # if --no-big-tests is a previous option, do not show big-tests
         LIST_SUITES_ARGS+=" --no-big-tests "
    fi

    SUGGESTIONS=$(./tools/test_runner/complete-test-name.sh "$cur")
    SUITES=$(./tools/test_runner/list_suites.sh $LIST_SUITES_ARGS)

    COMPREPLY=( $( compgen -W '--db --preset --dev-nodes --test-hosts \
                          --no-big-tests \
                          --no-small-tests \
                          --no-build-tests \
                          --no-services \
                          --no-cover \
                          --tls-dist \
                          --verbose \
                          --help \
                          --examples \
                          --examples-complete \
                          '"$SUGGESTIONS"' \
                          '"$SUITES"' \
                           --' -- $cur ) );;
#   Generate the completion matches and load them into $COMPREPLY array.
#   xx) May add more cases here.
#   yy)
#   zz)
  esac

  return 0
}

# you can use complete builtin in both bash and zsh now
alias run-all-tests.sh="$(pwd)/tools/run-all-tests.sh"
complete -F _run_all_tests run-all-tests.sh

# Say to the main script, that completion was enabled
export RUN_ALL_TESTS_COMPLETE=true
