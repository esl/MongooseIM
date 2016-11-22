#!/bin/bash

source tools/travis-helpers.sh

set -euo pipefail
IFS=$'\n\t'

CT_REPORTS=$(ct_reports_dir)

echo "Uploading test results to s3"
echo $(s3_url ${CT_REPORTS})

mkdir -p ${CT_REPORTS}/small
mkdir -p ${CT_REPORTS}/big

if [ -d _build/test/logs ]; then
	cp -Rp _build/test/logs/* ${CT_REPORTS}/small
fi

CT_REPORT=test.disabled/ejabberd_tests/ct_report

if [ -d ${CT_REPORT} ] && [ "$(ls -A ${CT_REPORT})" ];  then
	cp -Rp ${CT_REPORT}/* ${CT_REPORTS}/big
fi

cat > ${CT_REPORTS}/index.html << EOL
<html>
  <head></head>
  <body>
    <p><a href="small/index.html">Small tests (apps/ejabberd/test)</a></p>
    <p><a href="big/index.html">Big tests (test.disabled/ejabberd_tests)</a></p>
  </body>
</html>
EOL

now=`date +'%Y-%m-%d_%H.%M.%S'`
LOG_DIR_ROOT=${CT_REPORTS}/logs/${now}
for dev_node_path in `find _build -name mongooseim -type d`; do
	dev_node=$(basename $(dirname $(dirname ${dev_node_path})))
	LOG_DIR=${LOG_DIR_ROOT}/${dev_node}/log
	mkdir -p ${LOG_DIR}
	cp ${dev_node_path}/log/* ${LOG_DIR}
done

cp *.log ${LOG_DIR_ROOT}
cp test.disabled/ejabberd_tests/*.log ${LOG_DIR_ROOT}

aws s3 sync --quiet ${CT_REPORTS} s3://mongooseim-ct-results/${CT_REPORTS}

