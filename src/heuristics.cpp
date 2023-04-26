#include <Rcpp.h>
#include <cmath>
#include <bits/stdc++.h>
using namespace Rcpp;


// Heuristic functions to compute A* algorithm

// Utility function for converting degrees to radians
double deg2rad(const double degree)
{
  double one_deg = (M_PI) / 180;
  return (one_deg * degree);
}


// Haversine distance
// Code borrowed from: https://www.geeksforgeeks.org/program-distance-two-points-earth/

// [[Rcpp::export]]
double haversine(double lon1, double lat1, double lon2, double lat2)
{
  // Convert the latitudes  and longitudes from degree to radians.
  lat1 = deg2rad(lat1);
  lon1 = deg2rad(lon1);
  lat2 = deg2rad(lat2);
  lon2 = deg2rad(lon2);

  // Haversine Formula
  long double dlon = lon2 - lon1;
  long double dlat = lat2 - lat1;

  long double ans = pow(sin(dlat / 2.0), 2) +  cos(lat1) * cos(lat2) *  pow(sin(dlon / 2.0), 2);

  ans = 2.0 * asin(sqrt(ans));
  long double R = 6371.0; // Radius of Earth in  Kilometers, R = 6371;  Use R = 3956 for miles

  return ans * R;
}


// Euclidean distance

// [[Rcpp::export]]
double euclidean(double lon1, double lat1, double lon2, double lat2)
{
  return( sqrt(pow(lat1-lat2,2) + pow(lon1-lon2,2)) );
}


// Manhattan distance

// [[Rcpp::export]]
double manhattan(double lon1, double lat1, double lon2, double lat2)
{
  return( fabs(lat1-lat2) + fabs(lon1-lon2) );
}


// Canberra distance

// [[Rcpp::export]]
double canberra(double lon1, double lat1, double lon2, double lat2)
{
 return( fabs(lon1-lat1) / (fabs(lon1) + fabs(lat1)) ) + ( fabs(lon2-lat2) / (fabs(lon2) + fabs(lat2)) );
}

