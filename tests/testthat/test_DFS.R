

test_that("apply the DFS funciotn to a random graph", {

  graph = randomGraph(100, 1)

  expect_no_error(DFS(graph, 50))

})

