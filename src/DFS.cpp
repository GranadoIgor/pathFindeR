#include <Rcpp.h>
#include <stack>

using namespace Rcpp;

// A non-recursive implementation of Depth-first search (DFS) algorithm

// [[Rcpp::export]]
IntegerVector DFS(const RObject& graph, int v) {

  DataFrame     Graph    = wrap(graph.slot("graph"));
  DataFrame     nodeList = wrap(graph.slot("nodeList"));
  IntegerVector from     = Graph["from"];
  IntegerVector to       = Graph["to"];
  int           n        = nodeList.nrows();
  LogicalVector visited(n);
  IntegerVector nodeId   = nodeList["id"];
  IntegerVector nodeRef  = nodeList["ref"]; // C++ node index starting from 0

  // Match node id with node ref
  LogicalVector idx = nodeId == v;
  v = as<int>(nodeRef[idx]);

  // Adjacency list
  std::vector<std::vector<int> >  adj(n);
  for (int i = 0; i < from.size(); ++i) {
    adj[from[i]].push_back(to[i]);
  }

  std::stack<int> S;
  S.push(v);

  while (!S.empty()) {

    v = S.top();
    S.pop();
    visited[v] = true;

    IntegerVector  adjLoop = wrap(adj[v]);
    for(IntegerVector::iterator i = adjLoop.begin(); i != adjLoop.end(); i++) {
      if (!visited[*i])
        S.push(*i);
    }
  }

  return nodeRef[visited];
}

