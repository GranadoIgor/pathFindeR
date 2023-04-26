# pathFindeR
A collection of algorithms for the static and time-dependent shortest path problem using R and C++


**Installation**

```R
devtools::install_github("GranadoIgor/pathFindeR")
library(pathFindeR)
```

## List of functions

- `makeGraph()`, creates a static weighted graph.
- `makeTDGraph()`, creates a time-dependent weighted graph.
- `randomGraph()`, creates a randomly generated static graph using the Erdos-Renyi model, where a graph is generated by connecting nodes with a fixed probability, independently of any existing connections
- `graphComponents()`, gives a subgraph in which any vertex can be reached from node v by at least one path.
- `makeAdjcList()`, constructs an adjacency list.


## List of algorithms

- `DFS()`, the Depth First Search algorithm is a graph traversal algorithm that explores as far as possible along each branch before backtracking. 
- `BFS()`, the Breadth-first search algorithm is a graph traversal algorithm that visits all the nodes of a graph in breadth-first order, visiting all the neighbors of a node before visiting any of their neighbors.
- `Dijkstra()`, The Dijkstra algorithm is a shortest-path algorithm that finds the shortest path between a starting node and all other nodes in a weighted graph, by iteratively selecting the node with the lowest distance and updating the distances of its neighbors.
- `Astar()`, the A* algorithm is a heuristic search algorithm that finds the shortest path between a starting node and a goal node in a weighted graph. 
