#include <queue>
#include <vector>
#include <Rcpp.h>

using namespace Rcpp;

// Time-dependent Dijkstra algorithm to calculate the shortest path on a time-dependent graph object

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
Rcpp::List tdDijkstra(const RObject& graph, IntegerVector sources, IntegerVector goals) {

  // Set up variables
  NumericMatrix   Graph    = graph.slot("tdGraph");
  DataFrame       nodeList = wrap(graph.slot("nodeList"));
  NumericVector   from     = Graph(_, 0);
  NumericVector   to       = Graph(_, 1);
  int             m        = Graph.ncol() - 1;
  NumericMatrix   weight   = Graph(_, Range(2, m));
  IntegerVector   nodeId   = nodeList["id"];
  IntegerVector   nodeRef  = nodeList["ref"]; // node index from strating from 0 (c++ index)
  int             nNodes  = nodeId.size();
  List            graphInfo  = wrap(graph.slot("graphInfo"));
  const double    t0       = graphInfo["t0"];
  double          timeStep  = graphInfo["timeStep"];
  double          nTimes    = graphInfo["nTimes"];
  IntegerVector   status(nNodes, 0); // 0 no visited, 1 visited, 2 finished
  std::vector<IntegerVector> path(sources.size());
  std::vector<NumericVector> time(sources.size());

  // Conver source/gols to c++ index using nodeRef
  IntegerVector   idx = Rcpp::match(sources, nodeId);
  IntegerVector   sourceCpp = nodeRef[idx-1];
  idx = Rcpp::match(goals, nodeId);
  IntegerVector   goalsCpp = nodeRef[idx-1];

  typedef std::pair<double, int> Elt; // PriorityQueue Element: node id and weight

  struct Edge {
    int           to;
    NumericVector weight;
  } edge;

  // Create graph
  std::vector< std::vector<Edge> > G(from.size());

  for (int i = 0; i < from.size(); ++i ) {
    edge.to        = to[i];
    edge.weight    = weight(i,_);
    G[from[i]].push_back( edge );
  }

  // Initialization
  NumericVector   dist(nNodes, R_PosInf);
  IntegerVector   prev(nNodes, -1);

  for (int j=0; j!=sourceCpp.size();j++){

    IntegerVector path2;
    NumericVector time2;
    int           StartNode = sourceCpp[j];
    int           endNode   = goalsCpp[j];

    // Initialization source node
    dist[StartNode]   = t0;
    status[StartNode] = 1; // esto no se si hay que meterlo!!

    std::priority_queue < Elt, std::vector<Elt>, std::greater<Elt> > Q;
    Q.push(Elt(dist[StartNode], StartNode));

    while (!Q.empty()) {

      int       u = Q.top().second;
      Q.pop();

      if (u==endNode)  break; // esto lo acabo de poner aqui!!

      // Select weight
      double box = floor(dist[u] / timeStep);
      if (nTimes <= box) {box = nTimes-1;}

      // Loop over the neighbors
      for (unsigned int i=0; i< G[u].size(); i++) {
        Edge          j = G[u][i];
        int           v = j.to;
        NumericVector w2 = j.weight;

        if (status[v]==0) {
          dist[v] = dist[u] + w2[box];
          status[v] = 1;
          prev[v] = u;
          Q.push(std::make_pair(dist[v], v));
        } else if (status[v]==1 && dist[u] + w2[box] < dist[v]) {
          dist[v] = dist[u] + w2[box];
          prev[v] = u;
          Q.push(std::make_pair(dist[v], v));
        }
      }

      status[u] = 2;

    }

    for (auto p = prev[endNode]; p != -1; p = prev[p]){
      path2.push_front(nodeId[p]);
      time2.push_front(dist[p]);
    }

    if (path2.size()>0){
      path2.push_back(nodeId[endNode]);
      time2.push_back(dist[endNode]);
    }

    path[j] = path2;
    time[j] = time2;

    //Reinitialize
    std::fill(dist.begin(),dist.end(), R_PosInf); // std::numeric_limits<double>::max()
    std::fill(prev.begin(),prev.end(),-1);
    std::fill(status.begin(), status.end(), 0);

  } // for end

  return  List::create(Rcpp::Named("Path") = path,
                       Rcpp::Named("Time") = time);
}
