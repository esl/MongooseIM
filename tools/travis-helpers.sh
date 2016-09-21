ct_reports_dir() {
  local BUILD_NO=${TRAVIS_BUILD_NUMBER:-ct_reports}
  local PRESET_NAME=${PRESET:-default}
  local ERLANG=${TRAVIS_OTP_RELEASE:-default}
  local CT_REPORTS="${BUILD_NO}/${PRESET_NAME}.${ERLANG}"
  local BRANCH=${TRAVIS_BRANCH:-master}
  local PR=${TRAVIS_PULL_REQUEST:-false}

  if [ ${PR} == false ]; then
  	echo "branch/${BRANCH}/${CT_REPORTS}"
  else
  	echo "PR/${PR}/${CT_REPORTS}"
  fi

}

s3_url() {
  local CT_REPORTS=${1:-}
  echo "http://mongooseim-ct-results.s3-website-eu-west-1.amazonaws.com/${CT_REPORTS}/index.html"
}

