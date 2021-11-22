#!/usr/bin/env bash

week=$(date "+%V")
((num=$week - $week % 2))
year=$(date "+%Y")
makefile_sum=$(sha1sum tools/ssl/Makefile | cut -d " " -f1)

echo "${year}-week${num}-${makefile_sum}"
