ct_reports_dir() {
  if [[ "$CIRCLECI" == true ]]; then
    ct_reports_dir_circleci
  else
    ct_reports_dir_github
  fi
}

ct_reports_dir_circleci() {
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

ct_reports_dir_github() {
  # Note, that tools/gh-upload-to-s3.sh uploads to a different random prefix
  local BUILD_NO=${GITHUB_RUN_NUMBER:-ct_reports}
  local PRESET_NAME=${PRESET:-default}
  # @TODO CI:
  # CI_OTP_RELEASE: ${{ matrix.otp }}
  local ERLANG=${CI_OTP_RELEASE:-default}
  local CT_REPORTS="${BUILD_NO}/${PRESET_NAME}.${ERLANG}"
  # @TODO CI:
  # CI_BRANCH: $(echo ${GITHUB_REF##*/})
  local BRANCH=${CI_BRANCH:-master}
  # @TODO CI:
  # CI_PULL_REQUEST: ${{ github.event.pull_request.number }}
  local PR=${CI_PULL_REQUEST:-false}

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

archive_reader_url() {
  local TEST_TYPE=$1
  local CT_REPORTS=${2:-}
  echo "https://esl.github.io/html-zip-reader/${CT_REPORTS}/${TEST_TYPE}.tar.gz/"
}
