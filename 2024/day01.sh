#!/bin/bash

### Day 1: Historian Hysteria ###

INPUTFILE=$1

paste -d'-' \
  <(cat $INPUTFILE | tr -s ' ' | cut -d' ' -f1 | sort -n) \
  <(cat $INPUTFILE | tr -s ' ' | cut -d' ' -f2 | sort -n) \
  | bc \         # calculator
  | tr -d '-' \  # absolute value by deleting minus signs
  | awk '{ sum += $1 } END { print sum }'

while read ct n; do
  declare "count_$n=$ct"
  var="count_$n"
done < <(cat $INPUTFILE | tr -s ' ' | cut -d' ' -f2 | sort | uniq -c)

paste -sd'+' <(
while read n; do
  var="count_$n"
  if [ ! -z ${!var} ]; then
    echo "${!var}*$n"
  fi
done < <(cat $INPUTFILE | tr -s ' ' | cut -d' ' -f1)
) | bc
