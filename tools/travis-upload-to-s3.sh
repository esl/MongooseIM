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

aws s3 cp --recursive --quiet ${CT_REPORTS} s3://mongooseim-ct-results/${CT_REPORTS}

