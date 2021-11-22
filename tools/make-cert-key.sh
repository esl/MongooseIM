#!/usr/bin/env bash

week=$(date "+%V")
((num=$week - $week % 2))
year_month=$(date "+%Y_%m")

echo "${year_month}-week${num}"
