# Nedded data to create a graph
from = c(0,0,0, 1,1,1,1,1, 2,2,2, 3,3,3,3,3, 4,4,4,4,4,4,4,4, 5,5,5,5,5, 6,6,6, 7,7,7,7,7, 8,8,8)
to   = c(1,3,4, 0,3,4,5,2, 1,4,5, 0,1,4,7,6, 0,1,2,3,5,6,7,8, 1,2,4,7,8, 3,4,7, 6,3,4,5,8, 4,5,7)
weight = runif(length(from))
coords = data.frame("id"=c(0:8), "x"=runif(9), "y"=runif(9))



test_that("makeGraph from, to, and weight parameters lengths", {

  expect_s4_class(makeGraph(from=from, to=to, weight = weight), "graph")

  from = 1:22
  to = 1:5
  weight = runif(4)

  expect_error(makeGraph(from=from, to=to, weight = weight))

})


test_that("makeGraph check NA values on from, to, weight, and coords parameters", {

  expect_s4_class(makeGraph(from=from, to=to, weight = weight, coords = coords), "graph")

  from = c(1, 2, 3)
  to = c(2, 3, NA)
  weight = runif(3)
  coords = data.frame("id"=1:10, "x"=45:54, "y"=1:10)

  expect_error(makeGraph(from=from, to=to, weight = weight))

  to = c(2, 3, 1)
  coords = data.frame("id"=1:3, "x"=10:12, "y"=c(2, NA, 5))

  expect_error(makeGraph(from=from, to=to, weight = weight, coords = coords))

})


test_that("makeGraph check coords parameter", {

  expect_s4_class(makeGraph(from=from, to=to, weight = weight, coords = coords), "graph")

  coords = data.frame("id"=1:10, "x"=45:54, "y"=1:10, "Z"=1:10)

  expect_error(makeGraph(from=from, to=to, weight = weight, coords = coords))

})


test_that("makeGraph check directed parameter", {

  expect_s4_class(makeGraph(from=from, to=to, weight = weight, coords = coords, directed = F), "graph")
  expect_s4_class(makeGraph(from=from, to=to, weight = weight, coords = coords, directed = T), "graph")

})


test_that("makeGraph check node order (ref)", {

  graph = makeGraph(from=from, to=to, weight = weight, coords = coords, directed = T)

  expect_equal(coords$id, graph@nodeList$ref)

  from = from+5
  to   = to+5
  coords$id = coords$id+5
  graph = makeGraph(from=from, to=to, weight = weight, coords = coords, directed = T)

  expect_equal(graph@nodeList$ref, 0:(nrow(coords)-1))

})


test_that("makeGraph check node as character ", {

  from = letters[from+1]
  to   = letters[to+1]
  coords$id = letters[coords$id+1]

  expect_s4_class(makeGraph(from=from, to=to, weight = weight, coords = coords, directed = T), "graph")

})



###  Time dependent graph
weight = data.frame("w1"=runif(length(from)), "w2"=runif(length(from)), "w3"=runif(length(from)))
startTime=as.POSIXct("2021-08-06 12:47:00")
timeStep = 24 # hours



test_that("makeTDGraph from, to, and weight parameters lengths", {

  expect_s4_class(makeTDGraph(from = from, to = to, weight = weight,
                              coords = coords, startTime =  startTime,
                              timeStep = timeStep)
                  , "td.graph")

  weight = runif(4)
  expect_error(makeTDGraph(from=from, to=to, weight = weight))

})


test_that("makeTDGraph check NA values on from, to, weight, and coords parameters", {


  expect_s4_class(makeTDGraph(from = from, to = to, weight = weight,
                              coords = coords, startTime =  startTime,
                              timeStep = timeStep), "td.graph")

  from[4] = NA
  expect_error(makeTDGraph(from = from, to = to, weight = weight,
                                   startTime =  startTime, timeStep = timeStep))
})


test_that("makeTDGraph check crossTime parameter", {

  crossTime = weight

  expect_s4_class(makeTDGraph(from = from, to = to, weight = weight, crossTimes = crossTime,
                              coords = coords, startTime =  startTime,
                              timeStep = timeStep), "td.graph")

})

