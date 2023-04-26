#ifndef HAVERSINE_H
#define HAVERSINE_H

#include <Rcpp.h>
double haversine(double lon1, double lat1, double lon2, double lat2);

double euclidean(double lon1, double lat1, double lon2, double lat2);

double manhattan(double lon1, double lat1, double lon2, double lat2);

double canberra(double lon1, double lat1, double lon2, double lat2);

#endif
