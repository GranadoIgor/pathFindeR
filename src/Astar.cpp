#include <queue>
#include <vector>
#include <Rcpp.h>
#include <string>
#include "heuristics.h"

using namespace Rcpp;

//' A star algorithm to calculate the shortest path on a graph object

// [[Rcpp::export]]
Rcpp::List Astar(const RObject& graph, IntegerVector source, IntegerVector goal, String hFnc = "euclidean", double hCoef = 1) {

  // Set up variables
  DataFrame       Graph    = wrap(graph.slot("graph"));
  DataFrame       nodeList = wrap(graph.slot("nodeList"));
  IntegerVector   from     = Graph["from"];
  IntegerVector   to       = Graph["to"];
  NumericVector   weigth   = Graph["weight"];
  IntegerVector   nodeId   = nodeList["id"];
  IntegerVector   nodeRef  = nodeList["ref"]; // C++ node index starting from 0
  int             nNodes   = nodeId.size();
  NumericVector   nodeLon  = nodeList["x"];
  NumericVector   nodeLat  = nodeList["y"];
  std::vector<IntegerVector> path(source.size());
  std::vector<NumericVector> cost(source.size());

  // c++ index shift from R index using nodeRef
  IntegerVector   idx = Rcpp::match(source, nodeId);
  IntegerVector   sourceCpp = nodeRef[idx-1];
  idx = Rcpp::match(goal, nodeId);
  IntegerVector   goalCpp = nodeRef[idx-1];

  typedef std::pair<double, int> Elt; // PriorityQueue Element: node id and weight

  //Graph
  std::vector<std::vector<std::pair<int, double> > > G(nNodes);
  for (int i = 0; i < from.size(); ++i) {
    G[from[i]].push_back(std::make_pair(to[i], weigth[i]));
  }

  // Initialization
  NumericVector   gScore(nNodes, R_PosInf);
  NumericVector   fScore(nNodes, R_PosInf);
  IntegerVector   prev(nNodes, -1);

  for (int j=0; j!=sourceCpp.size();j++){

    IntegerVector path2;
    NumericVector cost2;
    int           StartNode = sourceCpp[j];
    int           endNode   = goalCpp[j];

    gScore[StartNode] = 0.0;

    if(hFnc=="haversine") {
      fScore[StartNode] = haversine(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode])/hCoef;
    } else if (hFnc=="manhattan") {
      fScore[StartNode] = manhattan(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode])/hCoef;
    } else if (hFnc=="canberra") {
      fScore[StartNode] = canberra(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode])/hCoef;
    } else {
      fScore[StartNode] = euclidean(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode])/hCoef;
    }

    std::priority_queue< Elt, std::vector<Elt>, std::greater<Elt> > Q;
    Q.push(Elt(0.0, StartNode));

    while (!Q.empty()) {
      int u = Q.top().second;
      Q.pop();

      // loop over the neighbors
      for (unsigned int i=0; i< G[u].size(); i++) {
        std::pair<int,double> j = G[u][i];
        int    v = j.first;
        double w = j.second;

        if (gScore[u] + w < gScore[v]) {
          gScore[v] = gScore[u] + w;
          if(hFnc=="haversine") {
            fScore[v] = gScore[v] + haversine(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode])/hCoef;
          } else if (hFnc=="manhattan") {
            fScore[v] = gScore[v] + manhattan(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode])/hCoef;
          } else if (hFnc=="canberra") {
            fScore[v] = gScore[v] + canberra(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode])/hCoef;
          } else {
            fScore[v] = gScore[v] + euclidean(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode])/hCoef;
          }
          prev[v]   = u;
          Q.push(std::make_pair(fScore[v], v));
        }
      }
      if (u==goalCpp[j])  break;
    }

    for (auto p = prev[endNode]; p != -1; p = prev[p]){
      path2.push_front(nodeId[p]);
      cost2.push_front(gScore[p]);
    }

    if (path2.size()>0){
      path2.push_back(nodeId[endNode]);
      cost2.push_back(gScore[endNode]);
    }

    path[j] = path2;
    cost[j] = cost2;

    //Reinitialize
    std::fill(gScore.begin(),gScore.end(), R_PosInf);
    std::fill(fScore.begin(),fScore.end(), R_PosInf);
    std::fill(prev.begin(),prev.end(),-1);
  }
  return  List::create(Rcpp::Named("Path") = path,
                       Rcpp::Named("Cost") = cost);
}



//' A star algorithm that takes a heuristinc funciton as argument (it is slower)

// [[Rcpp::export]]
Rcpp::List Astarhfnc(const RObject& graph, IntegerVector source, IntegerVector goal, Function hFnc, double hCoef = 1) {

  // Set up variables
  DataFrame       Graph    = wrap(graph.slot("graph"));
  DataFrame       nodeList = wrap(graph.slot("nodeList"));
  IntegerVector   from     = Graph["from"];
  IntegerVector   to       = Graph["to"];
  NumericVector   weigth   = Graph["weight"];
  IntegerVector   nodeId   = nodeList["id"];
  IntegerVector   nodeRef  = nodeList["ref"]; // C++ node index starting from 0
  int             nNodes   = nodeId.size();
  NumericVector   nodeLon  = nodeList["x"];
  NumericVector   nodeLat  = nodeList["y"];
  std::vector<IntegerVector> path(source.size());
  std::vector<NumericVector> cost(source.size());

  // c++ index shift from R index using nodeRef
  IntegerVector   idx = Rcpp::match(source, nodeId);
  IntegerVector   sourceCpp = nodeRef[idx-1];
  idx = Rcpp::match(goal, nodeId);
  IntegerVector   goalCpp = nodeRef[idx-1];

  typedef std::pair<double, int> Elt; // PriorityQueue Element: node id and weight

  //Graph
  std::vector<std::vector<std::pair<int, double> > > G(nNodes);
  for (int i = 0; i < from.size(); ++i) {
    G[from[i]].push_back(std::make_pair(to[i], weigth[i]));
  }

  // Initialization
  NumericVector   gScore(nNodes, R_PosInf);
  NumericVector   fScore(nNodes, R_PosInf);
  IntegerVector   prev(nNodes, -1);

  for (int j=0; j!=sourceCpp.size();j++){
    if (j % 256){
      Rcpp::checkUserInterrupt();
    }

    IntegerVector path2;
    NumericVector cost2;
    int           StartNode = sourceCpp[j];
    int           endNode   = goalCpp[j];

    gScore[StartNode] = 0.0;
    fScore[StartNode] = as<double>(hFnc(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode]))/hCoef; // /hCoef Esto lo he quitado, ya que igual lo meto dentro de la funcion heuristica

    std::priority_queue< Elt, std::vector<Elt>, std::greater<Elt> > Q;
    Q.push(Elt(0.0, StartNode));

    while (!Q.empty()) {
      int u = Q.top().second;
      Q.pop();

      // loop over the neighbors
      for (unsigned int i=0; i< G[u].size(); i++) {
        std::pair<int,double> j = G[u][i];
        int    v = j.first;
        double w = j.second; //gScore[u]

        if (gScore[u] + w < gScore[v]) {
          gScore[v] = gScore[u] + w;
          fScore[v] = gScore[v] + as<double>(hFnc(nodeLon[StartNode], nodeLat[StartNode], nodeLon[endNode], nodeLat[endNode]))/hCoef; // antes tenia esto, pero creo que estaba mal
          prev[v]   = u;
          Q.push(std::make_pair(fScore[v], v));
        }
      }
      if (u==goalCpp[j])  break;
    }

    for (auto p = prev[endNode]; p != -1; p = prev[p]){
      path2.push_front(nodeId[p]);
      cost2.push_front(gScore[p]);
    }

    if (path2.size()>0){
      path2.push_back(nodeId[endNode]); // path2.insert(path2.end(),nodeId[endNode]);
      cost2.push_back(gScore[endNode]);
    }

    path[j] = path2;
    cost[j] = cost2;

    //Reinitialize
    std::fill(gScore.begin(),gScore.end(), R_PosInf); // std::numeric_limits<double>::max()
    std::fill(fScore.begin(),fScore.end(), R_PosInf); // std::numeric_limits<double>::max()
    std::fill(prev.begin(),prev.end(),-1);
  }
  return  List::create(Rcpp::Named("Path") = path,
                       Rcpp::Named("Cost") = cost);
}

