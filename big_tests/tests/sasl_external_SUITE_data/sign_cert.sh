#!/bin/bash

function invalid_usage()
{
  echo    "invalid input parameters, correct usage:"
  echo -n "    sign_cert.sh --req <path_to_csr_file> --out <path_to_signed_cert>"
  exit 1
}

req_file=
out_file=

## process input parameters
[ $# -eq 0 ] && invalid_usage
while true; do
  [ "$1" == "--req" ] && [ $# -gt 1 ] && { req_file="$2";  shift 2; continue; }
  [ "$1" == "--out" ] && [ $# -gt 1 ] && { out_file="$2";  shift 2; continue; }
  break
done

openssl ca -config openssl-ca-clients.cnf \
              -policy signing_policy \
	      -extensions signing_req \
              -out $out_file \
              -in $req_file <<EOF
y
y
EOF

