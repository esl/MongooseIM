#!/usr/bin/env bash

# Use bash "strict mode"
# Based on http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -eu pipefail
IFS=$'\n\t'

echo "Check that print_install_dir works"
MIM_DIR=$(mongooseimctl print_install_dir)
test -d "$MIM_DIR"

echo "Executing init scripts via 'mongooseimctl bootstrap'"
# Fails, if the exit code is wrong
mongooseimctl bootstrap

echo "Check that bootstrap01-hello.sh script is executed"
BOOTSTRAP_RESULT=$(mongooseimctl bootstrap)
echo "$BOOTSTRAP_RESULT" | grep "Hello from"

# Script should be accessable by the "mongooseim" user
mv smoke_templates.escript "$MIM_DIR/"

echo "Check, that templates are correctly processed"
echo "Override default demo_session_lifetime=600 with 700"
# We check escaping with MIM_unused_var
MIM_unused_var="'\n\t\t\"" MIM_demo_session_lifetime=700 mongooseimctl bootstrap
mongooseimctl escript "$MIM_DIR/smoke_templates.escript"

# Uppercase variables also work
MIM_DEMO_SESSION_LIFETIME=700 mongooseimctl bootstrap
mongooseimctl escript "$MIM_DIR/smoke_templates.escript"

echo "Check, that bootstrap fails, if permissions are wrong"
GOOD_SCRIPT="$MIM_DIR/scripts/bootstrap01-hello.sh"
chmod "644" "$GOOD_SCRIPT"

BAD_PERM_BOOTSTRAP_RESULT=$(mongooseimctl bootstrap || echo "It should fail")
echo "$BAD_PERM_BOOTSTRAP_RESULT" | grep "It should fail"


echo "Check, that bootstrap works without any scripts"
rm "$MIM_DIR/scripts/"*
mongooseimctl bootstrap


echo "Check, that bootstrap fails, if any of bootstrap scripts fail"
BAD_SCRIPT="$MIM_DIR/scripts/bootstrap02-fails.sh"
cat << EOF > "$BAD_SCRIPT"
#!/usr/bin/env bash

cat this_file_is_missing_you
EOF

chmod 755 "$BAD_SCRIPT"

BAD_BOOTSTRAP_RESULT=$(mongooseimctl bootstrap || echo "It should fail")
echo "$BAD_BOOTSTRAP_RESULT" | grep "It should fail"


echo "Starting mongooseim via 'mongooseimctl start'"
mongooseimctl start

echo "Waiting for the port 5222 to accept TCP connections"
./wait-for-it.sh -h localhost -p 5222 -t 60

echo "Checking status via 'mongooseimctl status'"
mongooseimctl status

echo "Trying to register a user with 'mongooseimctl register localhost a_password'"
mongooseimctl register localhost a_password

echo "Trying to register a user with 'mongooseimctl register_identified user localhost a_password_2'"
mongooseimctl register_identified user localhost a_password_2

echo "Checking if 2 users are registered on host 'localhost'"
expected=2
registered=$(mongooseimctl registered_users localhost | wc -l)
if [ ${registered} -ne ${expected} ]; then
    echo "registered value is ${registered} but expected ${expected}"
    exit 1
fi

echo "Stopping mongooseim via 'mongooseimctl stop'"
mongooseimctl stop

