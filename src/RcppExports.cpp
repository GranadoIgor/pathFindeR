// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Astar
Rcpp::List Astar(const RObject& graph, IntegerVector source, IntegerVector goal, String hFnc, double hCoef);
RcppExport SEXP _pathFindeR_Astar(SEXP graphSEXP, SEXP sourceSEXP, SEXP goalSEXP, SEXP hFncSEXP, SEXP hCoefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const RObject& >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type goal(goalSEXP);
    Rcpp::traits::input_parameter< String >::type hFnc(hFncSEXP);
    Rcpp::traits::input_parameter< double >::type hCoef(hCoefSEXP);
    rcpp_result_gen = Rcpp::wrap(Astar(graph, source, goal, hFnc, hCoef));
    return rcpp_result_gen;
END_RCPP
}
// Astarhfnc
Rcpp::List Astarhfnc(const RObject& graph, IntegerVector source, IntegerVector goal, Function hFnc, double hCoef);
RcppExport SEXP _pathFindeR_Astarhfnc(SEXP graphSEXP, SEXP sourceSEXP, SEXP goalSEXP, SEXP hFncSEXP, SEXP hCoefSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const RObject& >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type goal(goalSEXP);
    Rcpp::traits::input_parameter< Function >::type hFnc(hFncSEXP);
    Rcpp::traits::input_parameter< double >::type hCoef(hCoefSEXP);
    rcpp_result_gen = Rcpp::wrap(Astarhfnc(graph, source, goal, hFnc, hCoef));
    return rcpp_result_gen;
END_RCPP
}
// BFS
IntegerVector BFS(const RObject& graph, int v);
RcppExport SEXP _pathFindeR_BFS(SEXP graphSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const RObject& >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< int >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(BFS(graph, v));
    return rcpp_result_gen;
END_RCPP
}
// DFS
IntegerVector DFS(const RObject& graph, int v);
RcppExport SEXP _pathFindeR_DFS(SEXP graphSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const RObject& >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< int >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(DFS(graph, v));
    return rcpp_result_gen;
END_RCPP
}
// Dijkstra
Rcpp::List Dijkstra(const RObject& graph, IntegerVector source, IntegerVector goal);
RcppExport SEXP _pathFindeR_Dijkstra(SEXP graphSEXP, SEXP sourceSEXP, SEXP goalSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const RObject& >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type goal(goalSEXP);
    rcpp_result_gen = Rcpp::wrap(Dijkstra(graph, source, goal));
    return rcpp_result_gen;
END_RCPP
}
// ErdosRenyiPmodel
Rcpp::DataFrame ErdosRenyiPmodel(int n, double p);
RcppExport SEXP _pathFindeR_ErdosRenyiPmodel(SEXP nSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(ErdosRenyiPmodel(n, p));
    return rcpp_result_gen;
END_RCPP
}
// haversine
double haversine(double lon1, double lat1, double lon2, double lat2);
RcppExport SEXP _pathFindeR_haversine(SEXP lon1SEXP, SEXP lat1SEXP, SEXP lon2SEXP, SEXP lat2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type lon1(lon1SEXP);
    Rcpp::traits::input_parameter< double >::type lat1(lat1SEXP);
    Rcpp::traits::input_parameter< double >::type lon2(lon2SEXP);
    Rcpp::traits::input_parameter< double >::type lat2(lat2SEXP);
    rcpp_result_gen = Rcpp::wrap(haversine(lon1, lat1, lon2, lat2));
    return rcpp_result_gen;
END_RCPP
}
// euclidean
double euclidean(double lon1, double lat1, double lon2, double lat2);
RcppExport SEXP _pathFindeR_euclidean(SEXP lon1SEXP, SEXP lat1SEXP, SEXP lon2SEXP, SEXP lat2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type lon1(lon1SEXP);
    Rcpp::traits::input_parameter< double >::type lat1(lat1SEXP);
    Rcpp::traits::input_parameter< double >::type lon2(lon2SEXP);
    Rcpp::traits::input_parameter< double >::type lat2(lat2SEXP);
    rcpp_result_gen = Rcpp::wrap(euclidean(lon1, lat1, lon2, lat2));
    return rcpp_result_gen;
END_RCPP
}
// manhattan
double manhattan(double lon1, double lat1, double lon2, double lat2);
RcppExport SEXP _pathFindeR_manhattan(SEXP lon1SEXP, SEXP lat1SEXP, SEXP lon2SEXP, SEXP lat2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type lon1(lon1SEXP);
    Rcpp::traits::input_parameter< double >::type lat1(lat1SEXP);
    Rcpp::traits::input_parameter< double >::type lon2(lon2SEXP);
    Rcpp::traits::input_parameter< double >::type lat2(lat2SEXP);
    rcpp_result_gen = Rcpp::wrap(manhattan(lon1, lat1, lon2, lat2));
    return rcpp_result_gen;
END_RCPP
}
// canberra
double canberra(double lon1, double lat1, double lon2, double lat2);
RcppExport SEXP _pathFindeR_canberra(SEXP lon1SEXP, SEXP lat1SEXP, SEXP lon2SEXP, SEXP lat2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type lon1(lon1SEXP);
    Rcpp::traits::input_parameter< double >::type lat1(lat1SEXP);
    Rcpp::traits::input_parameter< double >::type lon2(lon2SEXP);
    Rcpp::traits::input_parameter< double >::type lat2(lat2SEXP);
    rcpp_result_gen = Rcpp::wrap(canberra(lon1, lat1, lon2, lat2));
    return rcpp_result_gen;
END_RCPP
}
// tdDijkstra
Rcpp::List tdDijkstra(const RObject& graph, IntegerVector sources, IntegerVector goals);
RcppExport SEXP _pathFindeR_tdDijkstra(SEXP graphSEXP, SEXP sourcesSEXP, SEXP goalsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const RObject& >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type sources(sourcesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type goals(goalsSEXP);
    rcpp_result_gen = Rcpp::wrap(tdDijkstra(graph, sources, goals));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_pathFindeR_Astar", (DL_FUNC) &_pathFindeR_Astar, 5},
    {"_pathFindeR_Astarhfnc", (DL_FUNC) &_pathFindeR_Astarhfnc, 5},
    {"_pathFindeR_BFS", (DL_FUNC) &_pathFindeR_BFS, 2},
    {"_pathFindeR_DFS", (DL_FUNC) &_pathFindeR_DFS, 2},
    {"_pathFindeR_Dijkstra", (DL_FUNC) &_pathFindeR_Dijkstra, 3},
    {"_pathFindeR_ErdosRenyiPmodel", (DL_FUNC) &_pathFindeR_ErdosRenyiPmodel, 2},
    {"_pathFindeR_haversine", (DL_FUNC) &_pathFindeR_haversine, 4},
    {"_pathFindeR_euclidean", (DL_FUNC) &_pathFindeR_euclidean, 4},
    {"_pathFindeR_manhattan", (DL_FUNC) &_pathFindeR_manhattan, 4},
    {"_pathFindeR_canberra", (DL_FUNC) &_pathFindeR_canberra, 4},
    {"_pathFindeR_tdDijkstra", (DL_FUNC) &_pathFindeR_tdDijkstra, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_pathFindeR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
