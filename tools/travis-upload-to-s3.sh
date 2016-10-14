#!/bin/bash

source tools/travis-helpers.sh

set -euo pipefail
IFS=$'\n\t'

CT_REPORTS=$(ct_reports_dir)

echo "Uploading test results to s3"
echo $(s3_url ${CT_REPORTS})

mkdir -p ${CT_REPORTS}/small
mkdir -p ${CT_REPORTS}/big

if [ -d apps/ejabberd/logs ]; then
	cp -Rp apps/ejabberd/logs/* ${CT_REPORTS}/small
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

for dev_node_path in dev/mongooseim_*; do
	dev_node=$(basename ${dev_node_path})
	now=`date +'%Y-%m-%d_%H.%M.%S'`
	LOG_DIR=${CT_REPORTS}/big/${dev_node}/${now}/log
	mkdir -p ${LOG_DIR}
	cp ${dev_node_path}/log/* ${LOG_DIR}
done

aws s3 sync --quiet ${CT_REPORTS} s3://mongooseim-ct-results/${CT_REPORTS}

