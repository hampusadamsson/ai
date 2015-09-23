
  
  #' Run Delivery Man
  #' 
  #' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
  #' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
  #' Your score is the time it takes for you to complete this task. To play manually pass manualDM
  #' as the carReady function and enter the number pad direction numbers to make moves.
  #' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the 
  #' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
  #' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic 
  #' conditional on the vertical roads. (2) a list providing information about your car. This
  #' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car 
  #' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
  #' 'mem' that you can use to store information you want to remember from turn to turn, and
  #' a field called nextMove where you will write what you want the car to do. Moves are 
  #' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
  #' matrix containing information about the packages. This contains five columns and a row for each
  #' package. The first two columns give x and y coordinates about where the package should be picked
  #' up from. The next two columns give x and y coordinates about where the package should be 
  #' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not delivered, 2 is delivered).
  #' Your function should return the car object with the nextMove specified.
  #' @param dim The dimension of the board. You will be scored on a board of dimension 10.
  #' @param turns The number of turns the game should go for if deliveries are not made. Ignore this 
  #' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
  #' you fail.
  #' @param pause The pause period between moves. Ignore this.
  #' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
  #' @return A string describing the outcome of the game.
  #' @export
  #' 
  
  hValue<-function(x,y,Gx,Gy){ #get h-value (Gy/Gx = goal)
  hx=x-Gx
  if(hx<0){hx=hx*-1}
  hy=y-Gy
  if(hy<0){hy=hy*-1}
    #return(hx+hy)
    return((hx+hy)*2)
  }
  
  getNode<-function(x,y,h,g,Nm,Pn){
    return(node=list("x"=x,"y"=y,"h"=h,"g"=g,"Nm"=Nm,"Pn"=Pn))
  }
  
  getNext<-function(x,y,Gx,Gy,roads,Pn){ #get neighburs
    if(x==1){
      ways = list(getNode(x+1,y,hValue(x+1,y,Gx,Gy),roads$hroads[y,x],6,Pn))
    }else{
      if(x==10){
        ways = list(getNode(x-1,y,hValue(x-1,y,Gx,Gy),roads$hroads[y,x-1],4,Pn))
      }else{
        ways = list(getNode(x-1,y,hValue(x-1,y,Gx,Gy),roads$hroads[y,x-1],4,Pn),getNode(x+1,y,hValue(x+1,y,Gx,Gy),roads$hroads[y,x],6,Pn))
      }
    }
    if(y==1){
      ways = c(ways,list(getNode(x,y+1,hValue(x,y+1,Gx,Gy),roads$vroads[y,x],8,Pn)))
    }else{
      if(y==10){
        ways = c(ways,list(getNode(x,y-1,hValue(x,y-1,Gx,Gy),roads$vroads[y-1,x],2,Pn)))
      }else{
        ways = c(ways,list(getNode(x,y-1,hValue(x,y-1,Gx,Gy),roads$vroads[y-1,x],2,Pn),getNode(x,y+1,hValue(x,y+1,Gx,Gy),roads$vroads[y,x],8,Pn)))
      }
    }
    return(ways)
  }
  
  
  predCost<-function(Node){
    Node = data.frame(Node)
    i=0
    result=0
    while(length(Node)>((i*5)+3)){
      result=result+Node[(4+(5*i))]
      i=i+1
    }
    return(result)
  }

  calculateNext<-function(car, packages, roads){
    print("Choosing target...")
    Gx=0
    Gy=0
    if (car$load>0) {
      Gx=packages[car$load,3]
      Gy=packages[car$load,4]
    }else{
      MaxTurns=100
      for(i in 1:5){
        if(packages[i,5]==0){
          Gx_tmp=packages[i,1]
          Gy_tmp=packages[i,2]
          status = estimateTurns(getNext(car$x,car$y,Gx_tmp,Gy_tmp,roads,0),roads, Gx_tmp, Gy_tmp)
          if(status$turns<MaxTurns){
            MaxTurns=status$turns
            Gx=Gx_tmp
            Gy=Gy_tmp
          }
        }
      }
    }
    return(list(Gx,Gy))
  }
  
  estimateTurns<-function(NodeList, roads, Gx, Gy){
    status = 0
    repeat{
      nextLoc=0
      highV=99
      locMax=100
      Index=1
      winner=FALSE
      
      if(length(NodeList)!=0){
        for(i in 1:length(NodeList)){
          highV=predCost(NodeList[i])+data.frame(NodeList[i])$h
          if(highV<=locMax){
            nextLoc=data.frame(NodeList[i])
            locMax=highV
            Index=i
          }
        }
        status$turns=highV-data.frame(NodeList[Index])$h
        #print(paste(data.frame(NodeList[Index])$x,",",data.frame(NodeList[Index])$y))
        NodeList[Index]<-NULL
      }

      if(Gx==nextLoc$x){
        if(Gy==nextLoc$y){
          winner=nextLoc
        }
      }
      
      NodeList=c(NodeList,rekurs(nextLoc,roads,Gx,Gy))
      
      if(winner!=FALSE){
        status$nextMove=getInit(winner)
        break
      }
    }
    return(status)
  }
  
  astarDM=function(roads,car,packages){
    
    Target=calculateNext(car, packages, roads)
    Gx = as.numeric(Target[1])
    Gy = as.numeric(Target[2])

    print(paste("Target choosen: ",Gx,",",Gy))

    Neighburs <- getNext(car$x,car$y,Gx,Gy,roads,0)  
    car$nextMove = estimateTurns(Neighburs, roads, Gx, Gy)$nextMove
    
    print("Path choosen")
    
    return(car)
  }

  getInit<-function(Node){
    Node = data.frame(Node)
    nextMove=(Node[length(Node)-1])
    return(nextMove)
  }
  
  rekurs<-function(Node,roads,Gx,Gy){
    return(getNext(Node$x,Node$y,Gx,Gy,roads,Node))
  }
  
  runMe=function(){
    for(i in 1:50){
      runDeliveryMan(astarDM,10,2000,T,0,5)
    }
  }
  
  runMe()