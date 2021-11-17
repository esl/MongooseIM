# For all variables that start with ENV_FILE_
# example:
# export ENV_FILE_CFG_PATH=/tmp/test
# export ENV_FILE_CFG_DATA=meow
set -e

export

echo "Injecting files"
for var in "${!ENV_FILE_@}"; do
    # If ends with _PATH
    if [ -z ${var##*_PATH} ]; then
        # remove suffix
        name=${var%_PATH}
        data_var="${name}_DATA"
        path="${!var}"
        data="${!data_var}"
        echo "Write $name into $path"
        dir=$(dirname "$path")
        mkdir -p "$dir"
        echo "$data" | base32 --decode > "$path"
    fi
done
ls -lah /
echo "Start old entrypoint $OLD_ENTRYPOINT"
eval "$OLD_ENTRYPOINT"
