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
- `randomGraph()`, creates a randomly generated static graph using the Erdos-Renyi model.
- `graphComponents()`, gives a subgraph in which any vertex can be reached from node v by at least one path.
- `makeAdjcList()`, constructs an adjacency list.

