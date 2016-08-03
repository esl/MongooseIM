#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

CT_REPORTS=${TRAVIS_JOB_NUMBER:-ct_reports}
# Replace . with / to create better dir structure
CT_REPORTS=${CT_REPORTS/\./\/}
BRANCH=${TRAVIS_BRANCH:-master}
PR=${TRAVIS_PULL_REQUEST:-false}

if [ ${PR} == false ]; then
	CT_REPORTS=branch/${BRANCH}/${CT_REPORTS}
else
	CT_REPORTS=PR/${PR}/${CT_REPORTS}
fi


mkdir -p ${CT_REPORTS}/small
mkdir -p ${CT_REPORTS}/big

if [ -d apps/ejabberd/logs ]; then
	cp -Rp apps/ejabberd/logs/* ${CT_REPORTS}/small
fi

CT_REPORT=test/ejabberd_tests/ct_report

if [ -d ${CT_REPORT} ] && [ "$(ls -A ${CT_REPORT})" ];  then
	cp -Rp ${CT_REPORT}/* ${CT_REPORTS}/big
fi

cat > ${CT_REPORTS}/index.html << EOL
<html>
  <head></head>
  <body>
    <p><a href="small/index.html">Small tests (apps/ejabberd/test)</a></p>
    <p><a href="big/index.html">Big tests (test/ejabberd_tests)</a></p>
  </body>
</html>
EOL

for dev_node_path in dev/mongooseim_*; do
	dev_node=$(basename ${dev_node_path})
	LOG_DIR=${CT_REPORTS}/big/${dev_node}/log
	mkdir -p ${LOG_DIR}
	cp ${dev_node_path}/log/* ${LOG_DIR}
done

echo "Uploading test results to s3"
echo "http://mongooseim-ct-results.s3-website-eu-west-1.amazonaws.com/${CT_REPORTS}/index.html"

aws s3 sync --quiet ${CT_REPORTS} s3://mongooseim-ct-results/${CT_REPORTS}

