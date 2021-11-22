#!/usr/bin/env bash

week=$(date "+%V")
((num=$week - $week % 2))
year=$(date "+%Y")

echo "${year}-week${num}"
