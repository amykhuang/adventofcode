#!/bin/bash

### Day 6: Guard Gallivant ###

INPUTFILE=inputs/day06.txt

awk '
function get_next(x,y,d,grid){
x2=x
y2=y
if(d==0) x2=x-1
if(d==1) y2=y+1
if(d==2) x2=x+1
if(d==3) y2=y-1

if(! ((x2,y2) in original_grid))
  return "done"

if(grid[x2,y2]=="#")
  return x "," y "," (d+1)%4

return x2 "," y2 "," d
}

function run(x,y,dir,b1,b2){
# populate 
for(m=1;m<=rows;m++){
  for(n=1;n<=cols;n++){
    for(p=0;p<=3;p++){
      map[m,n]=original_grid[m,n]
      visited[m,n]=0
      visited_loop[m,n,p]=0
    }
  }
}
map[b1,b2]="#"

while(1){
  visited[x,y]=1
  visited_loop[x,y,dir]=1
  result=get_next(x,y,dir,map)
  if(result=="done"){
    for(m in visited)sum+=visited[m]
    return sum
  }
  split(result, nextpos, ",")
  x=nextpos[1]
  y=nextpos[2]
  dir=nextpos[3]
  if(visited_loop[x,y,dir]==1){
    return "loop"
  }
}
}

BEGIN{
rows=0
start_dir=0  # 0: north, 1: east, 2: south, 3: west 
}
{
  cols=length($0)
  split($0,ls,"")
  rows++
  for(c=1;c<=cols;c++){
    if(ls[c]=="^"){
      start_x=rows
      start_y=c
    }
    original_grid[rows,c]=ls[c]
  }
}

END{
print run(start_x,start_y,start_dir,-1,-1)

# Save initial path by copying to new array
for(v in visited)if(visited[v]==1)check_path[v]=1

num_loops=0
for(x in check_path){
  split(x, pos, SUBSEP);
  if(pos[1]==start_x && pos[2]==start_y)continue
  if(run(start_x,start_y,start_dir,pos[1],pos[2])=="loop"){
    num_loops+=1
  }
}
print num_loops
}' $INPUTFILE
                                                                              88,1          Bot

