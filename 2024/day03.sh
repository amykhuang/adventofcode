#!/bin/bash

### Day 3: Mull It Over ###

INPUTFILE=$1

mulregex="mul\(([0-9]+),([0-9]+)\)"

sum=0
sum2=0
enabled=1

for s in $(cat $INPUTFILE | sed 's/mul/\nmul/g' | sed 's/do/\ndo/g'); do
  if [[ $s == "do()"* ]]; then
    enabled=1
  elif [[ $s == "don't()"* ]]; then
    enabled=0
  elif [[ $s =~ $mulregex ]]; then
    a=${BASH_REMATCH[1]}
    b=${BASH_REMATCH[2]}
    (( sum += a * b ))
    if [[ $enabled == 1 ]]; then
      (( sum2 += a * b ))
    fi
  fi
done

echo $sum
echo $sum2

