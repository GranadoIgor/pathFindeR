#' @title Constructs a static weighted graph
#'
#' @details The \code{makeGraph} function can be used to construct a weighted graph.
#'
#' @param from Integer or character vector containing sources nodes IDs.
#' @param to Integer or character vector containing target nodes IDs.
#' @param weight A non-negative integer or numeric vector containing the edges weights.
#' @param directed logical. If FALSE, then all edges are duplicated by inverting 'from' and 'to' nodes (default TRUE).
#' @param coords A data.frame or matrix containing all nodes coordinates. Columns order should be 'id', 'x', 'y'. (Optional)
#' @examples
#'  from = c(1, 2, 3)
#'  to = c(2, 3, 1)
#'  weight = runif(3)
#'  coords = data.frame("id"=1:3, "x"=10:12, "y"=c(2, 3, 5))
#'  makeGraph(from=from, to=to, weight = weight, coords = coords)
#'
makeGraph = function(from, to, weight=NULL, directed=TRUE, coords=NULL){

  id = ref = NULL # due to NSE notes in R CMD check

  if(is.null(weight)) weight = rep(1, length(to))
  if (length(from) != length(to) || length(to) != length(weight))
    stop("'from', 'to' and weight' arguments should have the same length.")
  if (any(weight<0)) stop("Negative costs are not allowed")
  graph = data.table("from" = from, "to" = to, "weight" = weight)
  if (anyNA(graph))
    stop("NAs are not allowed in the graph")

  graph    = graph[from != to]
  nodeList = unique(c(graph[, from], graph[, to]))
  nodeList = nodeList[order(nodeList)]

  if (directed==FALSE)
    graph = rbind(graph, graph[, c("to", "from", "weight")], use.names=FALSE)

  if (!is.null(coords)){

    if (ncol(coords)!=3)
      stop("Coords should have 3 columns")

    setDT(coords)
    setnames(coords, names(coords), c("id", "x", "y"))

    if (anyNA(coords))
      stop("NAs are not allowed in coordinates")
    if (sum(duplicated(coords[, id]))>0)
      stop("Nodes should be unique in the coordinates data frame")
    if (sum(nodeList %in% coords[, id]) < length(nodeList))
      stop("Some nodes are missing in coordinates data")
    nodeList = coords[coords[, id] %in% nodeList,]

  } else {nodeList = data.table("id"=nodeList)}

  nodeList[, ref := seq_len(nrow(nodeList))-1]

  graph[, ":=" (from = nodeList$ref[match(from, nodeList$id)],
                to   = nodeList$ref[match(to, nodeList$id)])]

  object <- new("graph",
                graph    = graph,
                nodeList = nodeList
  )
  return(object)
}


setClass(Class = "graph",
         slots = c(graph    = "data.frame",
                   nodeList = "data.frame"),
         package = "pathFindeR"
)




#' The \code{makeTDGraph} function constructs a discrete time-dependent graph
#'
#' The \code{makeTDGraph} function can be used to construct a weighted time-dependent graph.
#'
#' @title Create a discrete time-dependent graph
#' @param from Integer or character vector containing sources nodes IDs.
#' @param to Integer or character vector containing target nodes IDs.
#' @param weight A non-negative data.frame or matrix containing the edges time-dependent weights. See details
#' @param crossTimes A non-negative data.frame containing the edges time-dependent travel times. See details
#' @param startTime a POSIXct object with the start time information.
#' @param timeStep Number that represents the delta time in hours (time step) between weights.
#' @param coords A data.frame or matrix containing all nodes coordinates. Columns order should be 'node_ID', 'X', 'Y'. (Optional)
#' @param directed logical. If FALSE, then all edges are duplicated by inverting \code{from} and \code{to} nodes (default TRUE).
#' @details
#' 'weight' is a non-negative data.frame or matrix describing the cost (e.g time, distance) between each 'edge (\code{from} and \code{to} nodes).
#' The values should match the edge represented by the '\code{from} and \code{to} arguments.
#'
#' 'crossTimes' is a non-negative data.frame or matrix describing the travel time between each edge (\code{from} and \code{to} nodes). It is only use when the
#' weights do not represent the travel time. The values should match the edge represented by the '\code{from} and \code{to} arguments.
#'
#' @examples
#' from = c(1, 2, 3)
#' to = c(2, 3, 1)
#' weight = data.frame("w1"=runif(3), "w2"=runif(3), "w3"=runif(3))
#' startTime = as.POSIXct("2021-08-06 12:47:00")
#' timeStep = 24 # hours
#' coords = data.frame("id"=1:3, "x"=10:12, "y"=c(2, 3, 5))
#' makeTDGraph(from=from, to=to, weight = weight, coords = coords,
#' startTime = startTime, timeStep=timeStep)
#'
makeTDGraph = function(from, to, weight, crossTimes=NULL, startTime, timeStep, coords=NULL, directed=TRUE){

  id = idx = ref = NULL # due to NSE notes in R CMD check

  if (length(from) != length(to) || length(to) != nrow(weight))
    stop("'from', 'to' and weight' arguments should have the same length.")
  if (any(weight<0))
    stop("Negative edge costs are not allowed")
  graph = data.table("from" = from, "to" = to,  weight) # las columnas weigth no tengo muy claro como llamarlas...
  if (anyNA(graph))
    stop("NAs are not allowed in the graph")

  graph = graph[from != to]  # Remove edges that star and finish in the same node
  nodeList = unique(c(graph[, from], graph[, to]))
  nodeList = nodeList[order(nodeList)]

  if (directed==FALSE)
    idx = c(2,1,3:ncol(graph))
    graph = rbind(graph, graph[, ..idx], use.names=FALSE)


  if (!is.null(coords)){
    if (ncol(coords)!=3)
      stop("Coords should have 3 columns")

    setDT(coords)
    setnames(coords, names(coords), c("id", "x", "y"))

    if (anyNA(coords))
      stop("NAs are not allowed in coordinates")
    if (sum(duplicated(coords[, id]))>0)
      stop("Nodes should be unique in the coordinates data frame")
    if (sum(nodeList %in% coords[, id]) < length(nodeList))
      stop("Some nodes are missing in coordinates data")
    nodeList = coords[coords[, id] %in% nodeList,]
  } else {nodeList = data.table("id"=nodeList)}

  nodeList[, ref := seq_len(nrow(nodeList))-1]

  graph[, ":=" (from = nodeList$ref[match(from, nodeList$id)],
                to   = nodeList$ref[match(to, nodeList$id)])]

  t0 = hour(startTime)+minute(startTime)/60+second(startTime)/3600 # The hours from start date

  object <- new("td.graph",
                tdGraph   = as.matrix(graph),
                nodeList  = nodeList,
                graphInfo = list("starTime" = startTime, "timeStep" = timeStep,
                                 "nTimes" = ncol(weight), "t0" = t0),
                crossTime = if(is.null(crossTimes)) matrix() else  as.matrix(crossTimes)
  )
  return(object)
}



setClass(Class = "td.graph",
         slots = c(tdGraph = "matrix",
                   nodeList = "data.frame",
                   graphInfo = "list",
                   crossTime = "matrix"),
         package = "pathFindeR"
)



#' @title Create a random static graph
#'
#' @description \code{randomGraph} function constructs a static random graph using the
#' Erdos-Renyi model.
#'
#' @param n Integer containing the number of nodes.
#' @param m_p Numeric (p) or integer (M) containing the number of edges (M) for the 'gnm' model,or the probability (p) for 'gnp' model.
#' @param type A character containing the model type. There are two options: G(n,p) model "gnp" or G(n,M) model 'gnm'.
#' Sea details for further information. (Default "gnp")
#' @param directed logical. If FALSE, then all edges are duplicated by inverting 'from' and 'to' nodes (default TRUE).
#' @param loops logical. If TRUE loops are allowed in the graph.
#'
#' @details Erdos Renyi Random Graph generator.
#'
#'The Erdos-Renyi (Erdos and Renyi, 1959) is the first ever proposed algorithm for the formation of random graphs.
#'The original two definitions are:
#'
#' - In the G(n,M) model, a graph is chosen uniformly at random from the collection of all graphs which have
#'n nodes and M edges. The nodes are considered to be labeled, meaning that graphs obtained from each other by permuting
#'the vertices are considered to be distinct.
#'
#' - In the G(n,p) model, a graph is constructed by connecting labeled nodes randomly. Each edge is included in
#'the graph with probability p , independently from every other edge.
#'
#' @note Currently, only the G(n,p) model is implemented in this function. For G(n,M) model we transform M to p by p = 2M / (n(n-1)).
#' Therefore, the 'gnm' model only generates approximately Erdos-Renyi Random Graphs, and the number of desired edges will not match.
#' However, if the size of the graph is large enough the function will generate graphs more similar to the expected Erdos-Renyi Model
#' and with the desired number of edges.
#'
#' @examples
#' graph = randomGraph(n=25, .1)
#'
randomGraph =  function(n, m_p, type = c("gnp", "gnm"), directed = TRUE, loops = FALSE){

  from = to = NULL # due to NSE notes in R CMD check

  type = match.arg(type)

  # Check input parameters
  stopifnot("Error: n must be positive"=n>0)

  if (type == "gnp") {
    stopifnot("p must be between 0 and 1."=!(m_p > 1 || m_p < 0))
    if (m_p == 1) { # Create a full graph
      graph = data.table("from" = rep(0:(n-1), each=n),
                         "to"   = rep(0:(n-1), n))
    } else {
      graph = setDT(ErdosRenyiPmodel(n, m_p))
    }
  }

  if (type == "gnm") {
    stopifnot("Error: M must be positive"=m_p>0)
    stopifnot("Invalid number of edges: M <= n"=n>m_p)

    if (m_p == n) { # Create a full graph
      graph = data.table("from" = rep(0:(n-1), each=n),
                         "to"   = rep(0:(n-1), n))
    } else {
      m_p = 2*m_p / (n * (n-1))
      graph = setDT(ErdosRenyiPmodel(n, m_p))
    }
  }

  if (!directed) {
    graph = rbind(graph, graph[, c("to", "from")], use.names=FALSE)
    setorder(graph, from, to)
    graph = unique(graph)
  }

  if(!loops)
    graph = graph[from != to]

  object <- new("graph",
                graph  = graph[, "weight" := 1],
                nodeList = data.table("id"  = seq_len(n),
                                      "ref" = seq_len(n)-1)
  )
  return(object)
}



#' @title Subset all connected components of a graph from node u
#'
#' @description \code{graphComponents} gives a subgraph in which any vertices can be
#' reached departing from node v by at least one path.
#'
#' @param graph A graph object.
#' @param v Node id.
#'
#' @examples
#' graph = randomGraph(n=10, .15)
#' subsetGraph = graphComponents(graph, 4)
#'
graphComponents =  function(graph, v){

  stopifnot("v is not a valid node id."= v %in% graph@nodeList$id)

  nodeRef = DFS(graph, v)

  subGraph = graph@graph[graph@graph$from %in% nodeRef, ]
  nodeList = graph@nodeList[graph@nodeList$ref %in% nodeRef, ]

  object <- new("graph",
                graph    = subGraph,
                nodeList = nodeList)

  return(object)
}
