#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

# Upload ct_report directory on Github pages for trusted builds

CT_REPORTS=${TRAVIS_JOB_NUMBER:-ct_reports}

mkdir -p ${CT_REPORTS}/small
mkdir -p ${CT_REPORTS}/big

if [ -d apps/ejabberd/logs ]; then
	cp -Rp apps/ejabberd/logs/* ${CT_REPORTS}/small
fi

if [ -d test/ejabberd_tests/ct_report ]; then
	cp -Rp test/ejabberd_tests/ct_report/* ${CT_REPORTS}/big
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

echo "Uploading test results to s3"
echo "http://mongooseim-ct-results.s3-website-eu-west-1.amazonaws.com/${CT_REPORTS}/index.html"

aws s3 sync --quiet ${CT_REPORTS} s3://mongooseim-ct-results/${CT_REPORTS}

