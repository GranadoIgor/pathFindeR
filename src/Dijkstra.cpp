#include <queue>
#include <vector>
#include <Rcpp.h>

using namespace Rcpp;

//' Dijkstra algorithm to calculate the shortest path on a static graph object

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
Rcpp::List Dijkstra(const RObject& graph, IntegerVector source, IntegerVector goal) {

  // Set up variables
  DataFrame       Graph    = wrap(graph.slot("graph"));
  DataFrame       nodeList = wrap(graph.slot("nodeList"));
  IntegerVector   from     = Graph["from"];
  IntegerVector   to       = Graph["to"];
  NumericVector   weigth   = Graph["weight"];
  IntegerVector   nodeId   = nodeList["id"];
  IntegerVector   nodeRef  = nodeList["ref"]; // node index from starting from 0 (c++ index)
  int             nNodes   = nodeId.size();
  std::vector<IntegerVector> path(source.size());
  std::vector<NumericVector> cost(source.size());

  // c++ index shift from R index using nodeRef
  IntegerVector   idx = Rcpp::match(source, nodeId);
  IntegerVector   sourceCpp = nodeRef[idx-1];
                  idx = Rcpp::match(goal, nodeId);
  IntegerVector   goalCpp = nodeRef[idx-1];

  // PriorityQueue Element: node weight and id
  typedef std::pair<double, int> Elt;

  //Graph
  std::vector<std::vector<std::pair<int, double> > > G(nNodes);
  for (int i = 0; i < from.size(); ++i) {
    G[from[i]].push_back(std::make_pair(to[i], weigth[i]));
  }

  // Initialization
  NumericVector   dist(nNodes, R_PosInf);
  IntegerVector   prev(nNodes, -1);

  // loop over the sources and goals nodes
  for (int j=0; j!=sourceCpp.size();j++){
    if (j % 256){
      Rcpp::checkUserInterrupt ();
    }

    IntegerVector path2;
    NumericVector cost2;
    int           StartNode = sourceCpp[j];
    int           endNode   = goalCpp[j];

    dist[StartNode] = 0.0;

    std::priority_queue< Elt, std::vector<Elt>, std::greater<Elt> > Q;
    Q.push(Elt(0.0, StartNode));

    while (!Q.empty()) {
      int u = Q.top().second;
      Q.pop();

        // loop over the neighbors
        for (unsigned int i=0; i< G[u].size(); i++) {
          std::pair<int,double> j = G[u][i];
          int    v  = j.first;
          double w = j.second;

          if (dist[u] + w < dist[v]) {
            dist[v] = dist[u] + w;
            prev[v] = u;
            Q.push(std::make_pair(dist[v], v));
          }
        }
      if (u==goalCpp[j])  break;
    }

    for (auto p = prev[endNode]; p != -1; p = prev[p]){
      path2.push_front(nodeId[p]);
      cost2.push_front(dist[p]);
    }

    if (path2.size()>0){
      path2.push_back(nodeId[endNode]);
      cost2.push_back(dist[endNode]);
    }

    path[j] = path2;
    cost[j] = cost2;

    //Reinitialize
    std::fill(dist.begin(),dist.end(), R_PosInf);
    std::fill(prev.begin(),prev.end(),-1);
  }

  return  List::create(Rcpp::Named("Path") = path,
                       Rcpp::Named("Cost") = cost);
}

