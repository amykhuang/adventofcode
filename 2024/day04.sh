#!/bin/bash

### Day 4: Ceres Search ###

INPUTFILE=inputs/day04.txt

awk '
function count_xmas(arr, i, j) {
  if(arr[i,j]!="X") return 0
  n=0
  if(arr[i,j+1]=="M" && grid[i,j+2]=="A" && grid[i,j+3]=="S")
    n+=1
  if(arr[i,j-1]=="M" && grid[i,j-2]=="A" && grid[i,j-3]=="S")
    n+=1
  if(arr[i+1,j]=="M" && grid[i+2,j]=="A" && grid[i+3,j]=="S")
    n+=1
  if(arr[i-1,j]=="M" && grid[i-2,j]=="A" && grid[i-3,j]=="S")
    n+=1
  if(arr[i+1,j+1]=="M" && grid[i+2,j+2]=="A" && grid[i+3,j+3]=="S")
    n+=1
  if(arr[i-1,j-1]=="M" && grid[i-2,j-2]=="A" && grid[i-3,j-3]=="S")
    n+=1
  if(arr[i-1,j+1]=="M" && grid[i-2,j+2]=="A" && grid[i-3,j+3]=="S")
    n+=1
  if(arr[i+1,j-1]=="M" && grid[i+2,j-2]=="A" && grid[i+3,j-3]=="S")
    n+=1
  return n
}

function count_x_mas(arr, i, j) {
  if(arr[i,j]!="A") return 0
  n=0
  a=arr[i+1,j+1]
  b=arr[i-1,j-1]
  c=arr[i+1,j-1]
  d=arr[i-1,j+1]
  if(a=="M" && b=="S" && c=="S" && d=="M")
    n+=1
  if(a=="S" && b=="M" && c=="M" && d=="S")
    n+=1
  if(a=="M" && b=="S" && c=="M" && d=="S")
    n+=1
  if(a=="S" && b=="M" && c=="S" && d=="M")
    n+=1
  return n
}
BEGIN{
rows=0
count1=0
count2=0
}
{
  cols=length($0)
  split($0,ls,"")
  rows++
  for(c=1;c<=cols;c++){
    grid[rows,c]=ls[c]
  }
}
END{
for(i=1;i<=rows;i++) {
  for(j=1;j<=cols;j++) {
    count1+=count_xmas(grid,i,j)
    count2+=count_x_mas(grid,i,j)
  }
}

print count1
print count2
}' $INPUTFILE

