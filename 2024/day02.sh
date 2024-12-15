#!/bin/bash

### Day 2: Red-Nosed Reports  ###

INPUTFILE=$1

awk '
function remove_index(ls2, ls1, k) {
  delete ls2
  i=1
  for(j=1;j<=length(ls1);j++) {
    if(j==k) continue
    ls2[i]=ls1[j]
    i++
  }
}

function is_safe(ls) {
  for(i=2;i<=length(ls);i++) {
    if(ls[i] >= ls[i-1] || ls[i] < ls[i-1] - 3)
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
  safe+=is_safe(ls)

  for(k=0;k<=length(ls);k++) {
    remove_index(newls, ls, k)
    if(is_safe(newls)) {
      safe_with_remove+=1
      break
    }
  }
}

END{
print safe
print safe_with_remove
}' <(cat $INPUTFILE \
     <(cat $INPUTFILE | awk '{ for(i=NF;i>=1;i--) printf "%s ",$i;print "" }'))
