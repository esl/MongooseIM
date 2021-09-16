#!/bin/bash

content_type="$1"

# create some temporary text file
filename="$(mktemp)"
text_data="qwerty"
echo "$text_data" > "$filename"

# get file size
filesize="$(wc -c "$filename" | awk '{print $1}')"

# generate upload/download urls
urls="$(./mongooseimctl http_upload localhost tmp.txt "$filesize" "$content_type" 600)"
if [ "$?" -ne 0 ]; then
    rm "$filename"
    echo "'mongooseimctl http_upload' failed  - '$urls'"
    exit 1
fi

# extract upload/download urls
put_url="$(echo "$urls" | awk '/PutURL:/ {print $2}')"
get_url="$(echo "$urls" | awk '/GetURL:/ {print $2}')"

# prepare HTTP headers for PUT request
headers=()

# if content type is set, add Content-Type header
if [ "$content_type" != "" ]; then
    headers+=(-H "Content-Type: $content_type")
fi

# if add_acl is enabled, add x-amz-acl header
if echo $put_url | grep -q "x-amz-acl"; then
    headers+=(-H "x-amz-acl: public-read")
fi

# try to upload a file (for interactive execution you may want to use -v or -i flags)
curl --fail -T "$filename" "${headers[@]}" "$put_url" || { rm "$filename"; exit 1; }

# remove temporary file
rm "$filename"

# try to download a file
text="$(curl --fail "$get_url")" || exit 1

if [ "$text" != "$text_data" ]; then
    echo "invalid text - '$text'"
    exit 1
fi
