#!/bin/sh
# Public-read bucket, since mod_http_upload_SUITE downloads via unsigned URLs.
# POSIX sh: setup-rustfs.sh runs it in the busybox-based curl image.
# Usage: prepare-rustfs-bucket.sh ENDPOINT_URL

set -e

ENDPOINT="$1"
ACCESS_KEY="AKIAIAOAONIULXQGMOUA"
SECRET_KEY="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"
BUCKET="mybucket"

s3() {
    curl --fail-with-body -sS --retry 10 --retry-delay 1 --aws-sigv4 "aws:amz:us-east-1:s3" \
        --user "$ACCESS_KEY:$SECRET_KEY" "$@"
}

s3 -X PUT "$ENDPOINT/$BUCKET"
s3 -X PUT "$ENDPOINT/$BUCKET?policy" -H "Content-Type: application/json" --data-binary @- <<EOF
{"Version": "2012-10-17",
 "Statement": [{"Effect": "Allow",
                "Principal": {"AWS": ["*"]},
                "Action": ["s3:GetObject"],
                "Resource": ["arn:aws:s3:::$BUCKET/*"]}]}
EOF
echo "Bucket $BUCKET is ready"
