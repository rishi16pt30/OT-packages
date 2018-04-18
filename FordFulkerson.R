library('igraph')
library('igraphdata')

bfs <- function(G,s,t){
  
  n = nrow(G)
  parent = integer(n)
  visited = integer(n)
  visited[s] = 1
  queue = c(s)
  
  #Loop while queue exists
  while (length(queue)){
    #print (queue)
    u = queue[1]
    queue = queue[-1]
    for (i in 1:n){
      if (visited[i] == 0 && G[u,i] > 0){
        queue = c(queue,i)
        visited[i] = 1
        parent[i] = u
      }
    }
  }

  return(c(visited[t] , parent))

}

FordFulkerson <- function(G,source,sink,sinkweight = Inf){

  n = nrow(G)
  m = ncol(G)
  
  MAXFLOW = 0
  PATHS = list()
  
  bf = bfs(G,source,sink)
  pathexist = bf[1]
  parent = bf[-1]
  
  while (pathexist){

    start = sink
    flow = Inf
    gnodes = c(source)
    
    while (start != source){
      
      #print ('Hi')
      gnodes = c(gnodes,start)
      flow = min(flow,G[parent[start],start],sinkweight)
      start = parent[start]
      
    }
    
    MAXFLOW = MAXFLOW + flow
    gnodes = c(gnodes , flow)
    PATHS = append(PATHS,list(gnodes))

    start = sink
    while (start != source){

      temp = parent[start]
      G[temp,start] = G[temp,start] - flow
     # G[start,temp] = G[start,temp] + flow
      start = parent[start]
    
    }
    
    if (flow == sinkweight) {
      break
    }
    else{
      sinkweight = sinkweight - flow
    }
    
    bf = bfs(G,source,sink)
    pathexist = bf[1]
    parent = bf[-1]
    
  }

  return (list(MAXFLOW,G,PATHS))

}

MaxFlow <- function (G,sources,sinks,sinkweights = NULL){
  
  GRAPH = G
  GRAPH = graph.adjacency(GRAPH,weighted = TRUE)
  
  nsources = length(sources)
  nsinks = length(sinks)
  
  if (!(is.null(sinkweights))){

    nweights = length(sinkweights)
    if (nsinks != nweights){
      print ("Number of sinks and sinkweights does not match ...")
      return (NULL)
    }

  }
  
  if (is.null(sinkweights)){
      
    MAXFLOWS = matrix(0,nsources,nsinks)
    GRAPHPATHS = list()
    
    #for (i in 1:nsources){
    #  GRAPHPATHS = append(GRAPHPATHS , list(0))
    #}
    
    for(i in 1:nsources){

      tempPath = list()
      for (j in 1:nsinks){

        ReturnValues = FordFulkerson(G,sources[i],sinks[j])
        MAXFLOWS[i,j] = ReturnValues[[1]]
        G = ReturnValues[[2]]
        tempPath = append(tempPath , ReturnValues[[3]])

      }
      GRAPHPATHS = append(GRAPHPATHS , tempPath)

    }

    #Last value in GRAPHPATHS is maxflow across the path 
    FLOWNODES = c()
    for (i in GRAPHPATHS){
      len = length(i)
      FLOWNODES = unique(c(FLOWNODES , i[-len]))
    }
    
    cat("\nMAXIMUM FLOW \n")
    
    ssources = c()
    ssinks = c()
    flows = c()
    
    for (i in 1:nsources){
      for(j in 1:nsinks){
        ssources = c(ssources , sources[i])
        ssinks = c(ssinks , sinks[j])
        flows = c(flows , MAXFLOWS[i,j])
      }
    }
    
    MAXFLOWS=data.frame(SOURCES = ssources , SINKS = ssinks , MAXFLOW = flows)
    print (MAXFLOWS)
    cat ("\n")
    
    plot.igraph(GRAPH,mark.groups = FLOWNODES)
    return (MAXFLOWS)

  }
  
  else{

    MAXFLOWS = matrix(0,nsources,nsinks)
    GRAPHPATHS = list()
    flag = TRUE
    checkflows = matrix(0,nsources,nsinks)
    
    while (TRUE){

      flag = TRUE
      for(j in 1:nsinks){

        tempPath = c()
        for(i in 1:nsources){
          
          ReturnValues = FordFulkerson(G,sources[i],sinks[j],sinkweights[j])
          MAXFLOWS[i,j] = MAXFLOWS[i,j] + ReturnValues[[1]]
          G = ReturnValues[[2]]
          
          if (sum(MAXFLOWS[,j]) >= sinkweights[j]){
            sinkweights[j] = Inf
            flag = FALSE
            break
          }
          
          if (ReturnValues[[1]] != 0){
            flag = FALSE
          } 

          checkflows[i,j] = ReturnValues[[1]]
          
          tempPath = append(tempPath , ReturnValues[[3]])

        }

        GRAPHPATHS = append(GRAPHPATHS , tempPath)
      
      }

      if (flag) break
      
    }

    cat("\nMAXIMUM FLOW \n")
    
    ssources = c()
    ssinks = c()
    flows = c()

    for (i in 1:nsources){
      for(j in 1:nsinks){
        ssources = c(ssources , sources[i])
        ssinks = c(ssinks , sinks[j])
        flows = c(flows , MAXFLOWS[i,j])
      }
    }

    MAXFLOWS=data.frame(SOURCES = ssources , SINKS = ssinks , MAXFLOW = flows)
    print (MAXFLOWS)
    cat ("\n")
    
    FLOWNODES = c()
    for (i in GRAPHPATHS){
      len = length(i)
      FLOWNODES = unique(c(FLOWNODES , i[-len]))
    }

    plot.igraph(GRAPH,mark.groups = FLOWNODES)
    return (MAXFLOWS)

  }
}

#G = matrix(c(0,16,13,0,0,0,0,0,10,12,0,0,0,4,0,0,14,0,0,0,9,0,0,20,0,0,0,7,0,4,0,0,0,0,0,0),6,byrow=TRUE)

data("rfid")
G = rfid[]
a = MaxFlow(G,c(1),c(67,75),c(0,100))
