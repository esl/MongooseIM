#!/usr/bin/env bash

set -e

cd tools/pkg/packages
PACKAGE_NAME=$(ls)
PKG_PROFILE=packages
prefix=${pkg_PLATFORM/_//}

if which aws ; then
    echo "aws tool ready"
else
    curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
    unzip -q awscliv2.zip
    sudo ./aws/install
fi

aws configure set aws_access_key_id "$PKG_ACCESS_KEY_ID" --profile "$PKG_PROFILE"
aws configure set aws_secret_access_key "$PKG_SECRET_ACCESS_KEY" --profile "$PKG_PROFILE"

aws s3 cp "$PACKAGE_NAME" "s3://arn:aws:s3:$PKG_AWS_REGION:$PKG_USER_ID:accesspoint/mim-packages/$prefix/$PACKAGE_NAME" --acl public-read --profile "$PKG_PROFILE" --region "$PKG_AWS_REGION"
