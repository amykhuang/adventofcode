#!/bin/bash

### Day 5: Print Queue ###

INPUTFILE=inputs/day05.txt

awk '
function valid(ls,order){
  for(i=1;i<length(ls);i++)
    if(!((ls[i],ls[i+1]) in order))
      return 0
  return 1
}

# changes value of ls
function reorder(ls,order){
  while(!valid(ls,order)){
    for(i=1;i<length(ls);i++){
      if(!((ls[i],ls[i+1]) in order)){
        tmp=ls[i]
        ls[i]=ls[i+1]
        ls[i+1]=tmp
      }
    }
  }
}

BEGIN{
total=0
total_fixed=0
}
{
  if($0 ~ /\|/){
    split($0,ns,"|")
    ordering[ns[1],ns[2]]=1
  }

  if($0 ~ /,/){
    split($0,ls,",")
    if(valid(ls,ordering)==1){
      total+=ls[(length(ls)+1)/2]
    }else{
      reorder(ls,ordering)
      total_fixed+=ls[(length(ls)+1)/2]
    }
  }
}
END{
print total
print total_fixed
}' $INPUTFILE

