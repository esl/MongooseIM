ct_reports_dir() {
  local BUILD_NO=${CIRCLE_BUILD_NUM:-ct_reports}
  local PRESET_NAME=${PRESET:-default}
  OTP_VERSION=`cat otp_version`
  local ERLANG=${OTP_VERSION:-default}
  local CT_REPORTS="${BUILD_NO}/${PRESET_NAME}.${ERLANG}"
  local BRANCH=${CIRCLE_BRANCH:-master}
  PR_NUM=`basename $CI_PULL_REQUEST`
  local PR=${PR_NUM:-false}


  if [ ${PR} == false ]; then
  	echo "branch/${BRANCH}/${CT_REPORTS}"
  else
  	echo "PR/${PR}/${CT_REPORTS}"
  fi

}

# Works for directories only
# Allows to list directories content

s3_url() {
  local CT_REPORTS=${1:-}
  echo "http://esl.github.io/circleci-mim-results/s3_reports.html?prefix=${CT_REPORTS}"
}

direct_s3_url() {
  local CT_REPORTS=${1:-}
  echo "https://circleci-mim-results.s3.eu-central-1.amazonaws.com/${CT_REPORTS}"
}

