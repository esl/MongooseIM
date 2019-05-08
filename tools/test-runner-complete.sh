# To enable autocompletion:
# source tools/test-runner-complete.sh
if [ ! -z "$ZSH_NAME" ]; then
    # zsh sets $ZSH_NAME variable so it can be used to detect zsh
    # following enables using bash-completion under zsh
    echo "Loading zsh completion system"
    autoload compinit
    compinit
    echo "Loading bash complete for zsh"
    autoload bashcompinit
    bashcompinit
fi


function_exists() {
    declare -f -F $1 > /dev/null
    return $?
}


# If brew command exists
# Load bash completion for _get_comp_words_by_ref on Mac
try_to_load_bash_completion_using_brew()
{
if ! function_exists _get_comp_words_by_ref && [ -x "$(command -v brew)" ] && [ -z "$ZSH_NAME" ]; then
    echo "Load bash_completion using brew"
    INCLUDE_BASH_COMPLETION="$(brew --prefix)/etc/bash_completion"
    if ! [ -f "$INCLUDE_BASH_COMPLETION" ]; then
      echo "Install bash-completion packet"
      echo 'Running "brew install bash-completion"'
      brew install bash-completion
    fi
    . "$INCLUDE_BASH_COMPLETION"
fi
}

try_to_load_bash_completion_using_brew

_run_all_tests() {
  printf "%s\n" "${COMP_WORDS[@]}" > /tmp/test-runner-last-competion
  # Make COMP_WORDS, without using colon as a breaker
  # To see all breakers, check COMP_WORDBREAKS variable
  # It's needed for bash only, not zsh
  if [ -z "$ZSH_NAME" ]; then
    _get_comp_words_by_ref -n : cur
  fi
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

  # If current is an option - we don't care about option expansion
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
    ARRAY=( $(./tools/test-runner.sh --list-dbs) )
    # Concat array using whitespaces
    COMPREPLY=( $( compgen -W "${ARRAY[*]} --" -- $cur ) );;

    --preset*)
    ARRAY=( $(./tools/test-runner.sh --list-presets) )
    COMPREPLY=( $( compgen -W "${ARRAY[*]} --" -- $cur ) );;

    --dev-nodes*)
    ARRAY=( $(./tools/test-runner.sh --list-dev-nodes) )
    COMPREPLY=( $( compgen -W "${ARRAY[*]} --" -- $cur ) );;

    --test-hosts*)
    ARRAY=( $(./tools/test-runner.sh --list-test-hosts) )
    COMPREPLY=( $( compgen -W "${ARRAY[*]} --" -- $cur ) );;

    *)

    LIST_SUITES_ARGS=""
    if [[ " ${COMP_WORDS[@]} " =~ " --skip-small-tests " ]]; then
         # if --skip-small-tests is a previous option, do not show small-tests
         LIST_SUITES_ARGS+=" --skip-small-tests "
    fi
    if [[ " ${COMP_WORDS[@]} " =~ " --skip-big-tests " ]]; then
         # if --skip-big-tests is a previous option, do not show big-tests
         LIST_SUITES_ARGS+=" --skip-big-tests "
    fi

    SUGGESTIONS=$(./tools/test_runner/complete-test-name.sh "$cur")
    SUITES=$(./tools/test_runner/list_suites.sh $LIST_SUITES_ARGS)

    COMPREPLY=( $( compgen -W '--db --preset \
                          --dev-nodes \
                          --test-hosts \
                          --one-node \
                          --pause \
                          --skip-big-tests \
                          --skip-small-tests \
                          --skip-build-tests \
                          --skip-build-mim \
                          --skip-preset \
                          --skip-cover \
                          --skip-start-nodes \
                          --skip-stop-nodes \
                          --skip-setup-db \
                          --tls-dist \
                          --verbose \
                          --help \
                          --examples \
                          --examples-complete \
                          --show-small-reports \
                          --show-big-reports \
                          --rerun-big-tests \
                          --colors \
                          '"$SUGGESTIONS"' \
                          '"$SUITES"' \
                           --' -- $cur ) );;
#   Generate the completion matches and load them into $COMPREPLY array.
#   xx) May add more cases here.
#   yy)
#   zz)
  esac 

  # When completing CT group, remove "SUITE:" prefix from  suggestions
  # This call modifies COMPREPLY
  # The function is not declared or supported in zsh
  if function_exists __ltrim_colon_completions && [ -z "$ZSH_NAME" ]; then
    __ltrim_colon_completions "$cur"
  fi
  return 0
}

# you can use complete builtin in both bash and zsh now
alias test-runner.sh="$(pwd)/tools/test-runner.sh"
complete -F _run_all_tests test-runner.sh

# Say to the main script, that completion was enabled
export RUN_ALL_TESTS_COMPLETE=true

echo "Completion has been enabled for the current shell"

# Show advice
EXAMPLES_ADVICE=$(cat <<-END
-----------------------------------
Run with --examples-complete argument to show the completion examples:
  ./tools/test-runner.sh --examples-complete
-----------------------------------

END
)

function run_all_tests_complete() {
  # Copy arguments into COMP_WORDS
  COMP_WORDS=(test-runner.sh "$@")
  # Count array elements, minus one
  COMP_CWORD=$(expr ${#COMP_WORDS[@]} - 1)
  _run_all_tests
  # print array one element per line
  printf '%s\n' "${COMPREPLY[@]}"
}
echo "$EXAMPLES_ADVICE"
