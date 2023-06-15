api_secret="$GA4_API_SECRET"
measurement_id="$GA4_MEASUREMENT_API"

ct_summary_file="/tmp/ct_summary"

echo "gh-report-failing-testcases-to-ga4.sh"

[ -f "$ct_summary_file" ] || { echo "ct_summary file is missing"; exit 1; }
[ -z "$api_secret" ] && { echo "invalid \$GA4_API_SECRET value"; exit 1; }
[ -z "$measurement_id" ] && { echo "invalid \$GA4_MEASUREMENT_API value"; exit 1; }

url="https://www.google-analytics.com/mp/collect"
url+="?measurement_id=${measurement_id}"
url+="&api_secret=${api_secret}"

function report_tc_to_ga4() {
    local test_case="$1"
    local client_id="$RANDOM"
    local payload='{"client_id": "'"$client_id"'",
                    "events": [{
                      "name": "failed_test_case",
                      "params": {
                        "test_case": "'"$test_case"'"}}]}'

    echo "reporting failed test case '${test_case}'"
    curl -s -X POST "$url" -d "$payload"
}

while read tc; do
    report_tc_to_ga4 "$tc"
done < "$ct_summary_file"
