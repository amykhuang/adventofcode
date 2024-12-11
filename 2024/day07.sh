#!/bin/bash

### Day 7: Bridge Repair ###

INPUTFILE=inputs/day07.txt

match_first_two="([0-9]+)\,([0-9]+)\,(.*)"

answer=0
while read -r line; do
  target=$(echo $line | cut -d ":" -f 1)
  # change to comma separated so we can use space-separated array
  equation=$(echo $line | cut -d ":" -f 2 | xargs | sed 's/ /,/g')","
  echo ">>" $target":" $equation

  # filter out obviously too low and too high answers
  IFS=","
  min_sum=0
  max_product=1
  max_concat=`echo $equation | tr -d ', '`
  for x in $equation; do
    (( min_sum += x ))
    (( max_product *= x ))
  done
#  if [[ $min_sum -gt $target ]]; then
#    continue
#  fi
  if [[ $min_sum -eq $target || $max_product -eq $target || $max_concat -eq $target ]]; then
    (( answer += target ))
    continue
  fi

  IFS=" "
  queue=($equation)
  while (( ${#queue[@]} > 0 )); do
    last=$(( ${#queue[@]} - 1 ))
    ns=${queue[$last]}
    unset 'queue[$last]'
    # echo "  equation:" $ns

    if [[ $ns =~ $match_first_two ]]; then
      a=${BASH_REMATCH[1]}
      b=${BASH_REMATCH[2]}
      rest=${BASH_REMATCH[3]}
      c1=$(( a + b ))
      c2=$(( a * b ))
      c3="$a$b" # remove c3 for part 1

      if [[ $rest == "" ]]; then
        if [[ $c1 == $target || $c2 == $target || $c3 == $target ]]; then
          echo "  found"
          (( answer += target ))

