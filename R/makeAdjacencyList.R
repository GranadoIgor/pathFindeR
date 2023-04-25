#' @title Create a static graph Adjacency list
#'
#' @description \code{makeAdjcList} function constructs an adjacency list
#'
#' @param min.lon A value indicating the min longitude of the graph (numeric).
#' @param max.lon A value indicating the max longitude of the graph  (numeric)
#' @param min.lat A value indicating the min latitude of the graph  (numeric)
#' @param max.lat A value indicating the max latitude of the graph  (numeric)
#' @param dx Graph resolution in the x direction
#' @param dy Graph resolution in the y direction
#' @param n number of adjacent cells (a cuales puede ir, 1 solo a las contiguas, 2 a las contijuas de las contiguas...)
#' @param land Layer from which the geometries or attributes are queried. This arguments allows
#' 'SpatialPolygons' or 'SpatialPolygonsDataFrame' class objects.
#' @return This function return node and edge adjacency-list
#' @examples
#' \dontrun{
#' # Graph id scheme
#'  #          -------------
#'  #          | 4 | 5 | 6 |
#'  #          -------------
#'  #          | 1 | 2 | 3 |
#'  #          -------------
#'  }

makeAdjcList = function(min.lon, max.lon, min.lat, max.lat, dx, dy=NULL, n=1, land=NULL){

  # due to NSE notes in R CMD check
  id = edges = N = `.` = destination = to = heading = distance = NULL
  midPoint_lon = midPoint_lat = NULL

  if (is.null(dy))
    dy=dx
  x = (max.lon - min.lon)/dx
  y = (max.lat - min.lat)/dy
  # Node list
  grid = expand.grid(x=seq(from = min.lon, to = max.lon, by = dx) + dx/2, y=seq(from = min.lat, to = max.lat, by = dy) + dy/2)
  setDT(grid)
  if (!is.null(land)) {
    node.list = grid[, "land" := pointsOnLand(x, y, land)]
    node.list = node.list[land == "Ocean"]
    node.list[, land:=NULL]
  } else {
    node.list = grid
  }
  node.list[, id := seq_len(.N)]
  node.list[, edges := list()]
  for (i in seq_len(nrow(node.list))) set(x = node.list,
                                          i = i,
                                          j = "edges",
                                          value = node.list[x %between% c(x[i]-(n*dx), x[i]+(n*dx)) & y %between% c(y[i]-(n*dy), y[i]+(n*dy)), .(id)][id != i]) # [-i] to remove their own id value
  node.list[, N := sum(unlist(edges)>0), by = id]

  ## Edge list
  edge.list = data.table("from" = rep(node.list$id, node.list$N),
                         "lon0" = rep(node.list$x, node.list$N),
                         "lat0" = rep(node.list$y, node.list$N))
  edge.list[ , to := rbindlist(lapply(node.list$edges, as.data.table))]

  edge.list[, ":=" (lon1 = node.list$x[match(edge.list$to, node.list$id)],
                    lat1 = node.list$y[match(edge.list$to, node.list$id)])]
  # calculate the heading
  p1 = matrix(c(edge.list$lon0, edge.list$lat0), ncol = 2)
  p2 = matrix(c(edge.list$lon1, edge.list$lat1), ncol = 2)
  edge.list[, heading := geosphere::bearing(p1, p2)]
  # Calculate the distance in Km
  edge.list[, distance := distGeo(p1, p2)/1000]
  # get middle point of the edge
  mid = midPoint(p1, p2)
  edge.list[, ":=" (midPoint_lon = mid[, "lon"],
                    midPoint_lat = mid[, "lat"])]

  object <- new("adjacency.list",
                node.list = node.list,
                edge.list = edge.list
  )
  return(object)

}


setClass("adjacency.list",
         slots = c(node.list = "data.frame",
                   edge.list = "data.frame"),
         package = "pathFindeR")
