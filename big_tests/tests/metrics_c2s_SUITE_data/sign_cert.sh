#!/usr/bin/env bash

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

args=()
case "$(basename "$req_file")" in
  not_yet_valid_cert.csr)
    # Valid from 01.01.3000
    args+=(-startdate 30000101000000Z);;
  expired_cert.csr)
    # Valid from 01.01.2024 until 31.12.2024
    args+=(-startdate 20240101000000Z -enddate 20241231000000Z);;
esac

openssl ca -config openssl-ca-clients.cnf \
           -policy signing_policy \
           -extensions signing_req \
           -out $out_file \
           -batch \
           -in $req_file \
           "${args[@]}" <<EOF
EOF

