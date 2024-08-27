#!/bin/bash

source tools/circleci-helpers.sh

set -euo pipefail
IFS=$'\n\t'

# Relative directory name
CT_REPORTS=$(ct_reports_dir)
mkdir -p ${CT_REPORTS}/small
mkdir -p ${CT_REPORTS}/big

if [ -d _build/test/logs ]; then
	cp -Rp _build/test/logs/* ${CT_REPORTS}/small
fi

CT_REPORT=big_tests/ct_report

if [ -d ${CT_REPORT} ] && [ "$(ls -A ${CT_REPORT})" ];  then
	cp -Rp ${CT_REPORT}/* ${CT_REPORTS}/big
fi

cat > ${CT_REPORTS}/index.html << EOL
<html>
  <head></head>
  <body>
    <p><a href="small/index.html">Small tests (test/)</a></p>
    <p><a href="big/index.html">Big tests (big_tests/)</a></p>
  </body>
</html>
EOL

CT_REPORTS_FULL=$(cd "$CT_REPORTS" && pwd)

now=`date +'%Y-%m-%d_%H.%M.%S'`
# Replace all occurrences of / with _
PREFIX="${CT_REPORTS//\//_}"

# Optimize naming, so it is easy to extract on MacOS just by clicking it
# and with reasonable directory names
LOG_DIR_ROOT=${CT_REPORTS}/logs/${PREFIX}_${now}
LOG_ZIP=${CT_REPORTS_FULL}/logs_${PREFIX}_${now}.zip
for dev_node_logs_path in `find _build -name log -type d`; do
	dev_node=$(basename $(dirname $(dirname $(dirname ${dev_node_logs_path}))))
        LOG_DIR=${LOG_DIR_ROOT}/${dev_node}/
	mkdir -p ${LOG_DIR}
	cp ${dev_node_logs_path}/* ${LOG_DIR}
done

cp *.log ${LOG_DIR_ROOT}
cp big_tests/*.log ${LOG_DIR_ROOT} || true

OLD_DIR=$(pwd)

# cd so we don't include nested dirs in the archive (for example, PR/4366/236412)
cd "$LOG_DIR_ROOT/.."

# Zip to safe space
zip -9 -r "$LOG_ZIP" "$(basename "$LOG_DIR_ROOT")"

cd "$OLD_DIR"

# Slightly faster than removing
mv "$LOG_DIR_ROOT" /tmp/
