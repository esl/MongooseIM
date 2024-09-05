#!/bin/bash

if [[ "$NO_CIRCLECI" == "1" ]]; then
source tools/helpers.sh
else
source tools/circleci-helpers.sh
fi

REPO_DIR=$(pwd)

set -euo pipefail
IFS=$'\n\t'

# Relative directory name
CT_REPORTS=$(ct_reports_dir)
mkdir -p "$CT_REPORTS"
CT_REPORTS_FULL=$(cd "$CT_REPORTS" && pwd)

now=`date +'%Y-%m-%d_%H.%M.%S'`
# Replace all occurrences of / with _
PREFIX="${CT_REPORTS//\//_}"

# Optimize naming, so it is easy to extract on MacOS just by clicking it
# and with reasonable directory names
LOG_DIR_ROOT=${CT_REPORTS}/logs/${PREFIX}_${now}
LOG_ZIP=${CT_REPORTS_FULL}/logs_${PREFIX}_${now}.tar.gz
for dev_node_logs_path in `find _build -name log -type d`; do
	dev_node=$(basename $(dirname $(dirname $(dirname ${dev_node_logs_path}))))
        LOG_DIR=${LOG_DIR_ROOT}/${dev_node}/
	mkdir -p ${LOG_DIR}
	mv ${dev_node_logs_path}/* ${LOG_DIR}
done

mv *.log ${LOG_DIR_ROOT}
mv big_tests/*.log ${LOG_DIR_ROOT} || true

# cd so we don't include nested dirs in the archive (for example, PR/4366/236412)
cd "$LOG_DIR_ROOT/.."

# Zip to safe space
tar -czf "$LOG_ZIP" "$(basename "$LOG_DIR_ROOT")"

cd "$REPO_DIR"

# Slightly faster than removing
mv "$LOG_DIR_ROOT" /tmp/

# Compress big ct_reports
BIG_REPORTS_DIR="$(pwd)/big_tests/ct_report"
SMALL_REPORTS_DIR="$(pwd)/_build/test/logs"

if [ -f ${BIG_REPORTS_DIR}/index.html ]; then
  cd ${BIG_REPORTS_DIR}
  # Ignore GDPR extracted logs
  # They are primarily empty files
  tar \
    --exclude='./ct_run*/*.logs/last_link.html' \
    --exclude='./ct_run*/*.logs/last_name' \
    --exclude='./ct_run*/*.unzipped' \
    -czf "${CT_REPORTS_FULL}/big.tar.gz" .
fi

if [ -f ${SMALL_REPORTS_DIR}/index.html ]; then
  cd ${SMALL_REPORTS_DIR}
  tar \
    --exclude='./ct_run*/*.logs/last_link.html' \
    --exclude='./ct_run*/*.logs/last_name' \
    -czf "${CT_REPORTS_FULL}/small.tar.gz" .
fi
