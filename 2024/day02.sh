#!/bin/bash

### Day 2: Red-Nosed Reports  ###

INPUTFILE=$1

awk '
function valid(x,y,inc) {
  return inc*x > inc*y && inc*x <= inc*y + 3
}

function remove_index(ls2, ls1, k) {
  delete ls2
  i=1
  for(j=1;j<=length(ls1);j++) {
    if(j==k) continue
    ls2[i]=ls1[j]
    i++
  }
}

function is_safe(ls, inc) {
  for(i=2;i<=length(ls);i++) {
    if(!valid(ls[i], ls[i-1], inc))
      return 0
  }
  return 1
}

BEGIN{
safe=0
safe_with_remove=0
}

{
  split($0,ls," ")
  safe+=is_safe(ls, -1) + is_safe(ls, 1)

  for(k=0;k<=length(ls);k++) {
    remove_index(newls, ls, k)
    if(is_safe(newls, -1) + is_safe(newls, 1) > 0) {
      safe_with_remove+=1
      break
    }
  }
}

END{
print safe
print safe_with_remove
}' $INPUTFILE

