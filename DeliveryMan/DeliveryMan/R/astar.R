# calculates the h-value using manhattan distance
#
#
manhattan<-function(x,y,Goal_x,Goal_y){
  h_value = abs(Goal_x-x)+abs(Goal_y-y)
  return(h_value)
}


# Find closest target and returns a list of the integer coordinates as (x,y)
#
# 
findDestination<-function(car, packages){
  
  if(car$load!=0){
    Gx=packages[car$load,3]
    Gy=packages[car$load,4]
  }else{
    MaxValue=100
    for(i in 1:5){
      if(packages[i,5]==0){ 
        candidate = manhattan(car$x,car$y,packages[i,1],packages[i,2])
        if(candidate<=MaxValue){
          Gx=packages[i,1]
          Gy=packages[i,2]
          MaxValue=candidate
        }
      }
    }
  }
  return(list(Gx,Gy))
}

# creates a node
# x = x coordinate
# y = y coordinate
# h = calculated heuristic value
# g = traffic situation (turn cost)
#
getNode<-function(x,y,h,g,firstMove){
  return(node=list("x"=x,"y"=y,"h"=h,"g"=g,"firstMove"=firstMove))
}

# Find car's nextMove. (ax,ay) - current node's x and y,  (ax,ay) - neighbour's node's x and y, returns a direction eg. - 4(left)
# ax - current node's x
# ay - current node's x
# bx - neighbour node's x
# by - neighbour node's x
#
direction = function(ax,ay,bx,by){
  if(ax == bx && ay < by){
    direction = 8
  } else if(ax == bx && ay > by){
    direction = 2
  } else if(ax < bx && ay == by){
    direction = 6
  } else if(ax > bx && ay == by){
    direction = 4
  } else {
    print("Error")
  }
  return (direction)
}

# Find neighbours - returns a list of the closest nodes given a currentNode = curNode
# curNode = node which neighbours are to be expanded
# Gx = destination x-coordinate
# Gy = destination y-coordinate
#
findNeighbours=function(roads, packages, curNode, Gx, Gy){ 
  x = curNode$x
  y = curNode$y
  parent_g = curNode$g
  firstMove = curNode$firstMove
  
  if(x==1){
    ways = list(getNode(x+1,y,manhattan(x+1,y,Gx,Gy),roads$hroads[y,x]+parent_g, firstMove))
  }else{
    if(x==10){
      ways = list(getNode(x-1,y,manhattan(x-1,y,Gx,Gy),roads$hroads[y,x-1]+parent_g,firstMove))
    }else{
      ways = list(getNode(x-1,y,manhattan(x-1,y,Gx,Gy),roads$hroads[y,x-1]+parent_g,firstMove),getNode(x+1,y,manhattan(x+1,y,Gx,Gy),roads$hroads[y,x]+parent_g,firstMove))
    }
  }
  if(y==1){
    ways = c(ways,list(getNode(x,y+1,manhattan(x,y+1,Gx,Gy),roads$vroads[y,x]+parent_g,firstMove)))
  }else{
    if(y==10){
      ways = c(ways,list(getNode(x,y-1,manhattan(x,y-1,Gx,Gy),roads$vroads[y-1,x]+parent_g,firstMove)))
    }else{
      ways = c(ways,list(getNode(x,y-1,manhattan(x,y-1,Gx,Gy),roads$vroads[y-1,x]+parent_g,firstMove),getNode(x,y+1,manhattan(x,y+1,Gx,Gy),roads$vroads[y,x]+parent_g,firstMove)))
    }
  }
  return(ways)
}

# Returns the minimum node, aka. next node to be expanded based on G and H
#
#
minimum = function (frontier){
  min = 100
  for(i in 1:length(frontier)){
    if ((frontier[[i]]$g+frontier[[i]]$h) < min){
      min = frontier[[i]]$g
      result_index=i
    }
  }
  return (result_index)
}

# Add the expanded nodes to the frontier if they are 'shorter' than the current occurance OR if it doesnt exist
# NodeList = frontier
# newNode = newly expanded nodes
#
addUniqueList<-function(NodeList, newNodes){
  for(i in 1:length(newNodes)){
    NodeList = addUnique(NodeList,newNodes[i])
  }
  return(NodeList)
}

# Subfunction to addUniqueList
#
#
addUnique<-function(NodeList, newNode){
  newNode=data.frame(newNode)
  for(i in 1:length(NodeList)){
    curNode=data.frame(NodeList[i])
    if((newNode$x==curNode$x) && (newNode$y==curNode$y) && (newNode$g>curNode$g)){
      return(NodeList)
    }
  }
  return(c(NodeList,list(newNode)))  
}

# Running the DM 
#
#
astarDM=function(roads,car,packages){ 
  target_list = findDestination(car, packages)
  Gx = target_list[[1]]
  Gy = target_list[[2]]
    
  node = getNode(car$x,car$y,manhattan(car$x,car$y,Gx,Gy), 0, 0)
  
  frontier = findNeighbours(roads, packages, node, Gx, Gy)
  for( i in 1 : length(frontier)){ #retrieve initial 'nextMove'
    target = frontier[[i]]
  
    frontier[[i]]$firstMove = direction(car$x,car$y,target$x,target$y)
  
  }
  repeat{
      index = minimum(frontier)
      current = frontier[[index]]
      frontier = c(frontier, findNeighbours(roads, packages, current, Gx, Gy)) #eventuella dubbetter TODO
      #frontier = addUniqueList(frontier, findNeighbours(roads, packages, current, Gx, Gy))
      frontier[index] <- NULL
      if(current$x == Gx && current$y == Gy){
        break
      }
  }
    car$nextMove = current$firstMove
    frontier = NULL
    return(car)
}  

runMe=function(){
  for(i in 1:50){
    runDeliveryMan(astarDM,10,2000,F,0,5)
  }
}

runMe()