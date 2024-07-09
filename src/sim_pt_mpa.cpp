#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List sim_pt_mpa(double r, double k,double init_b_inside, double init_b_outside, double m, double u, double p_mpa,
                      int local_dd,int years, double phi, bool pt,
                      double plim) {
  
  
  int n;
  
  double mu = 0;
  
  NumericVector yield (n);
  
  NumericVector inside_b (n);
  
  double last_inside_b = 0.0;
  
  NumericVector outside_b (n);
  
  double last_outside_b = 0.0;
  
  double growth_mult = 1.0;
  
  NumericVector tmp (n);
  
  n = years;
  
  last_inside_b = inside_b[0] =  init_b_inside;
  
  last_outside_b = outside_b[0] = init_b_outside;
  
  yield[0] = u * outside_b[0];
  
  mu = m * (1 - p_mpa); 
  
  for(int t = 1; t < n; ++t) {    
    // Pella-Tomlinsson Toggle
    if (pt == 0){
      
      if (local_dd == 1){
        
        inside_b[t] =
          last_inside_b +  r * last_inside_b * (1 - last_inside_b / (k * p_mpa)) - mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
        
        outside_b[t] =
          (1 - u) * last_outside_b +  r * last_outside_b * (1 - last_outside_b / (k * (1 - p_mpa))) + mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
        
        last_inside_b = inside_b[t];
        
        last_outside_b = outside_b[t];
        
      } else {
        
        
        inside_b[t] =
          last_inside_b +  p_mpa * r * (last_inside_b + last_outside_b)  * (1 - (last_inside_b + last_outside_b) / (k)) - mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
        
        outside_b[t] =
          (1 - u) * last_outside_b +  (1 - p_mpa) * r * (last_inside_b + last_outside_b) * (1 - (last_inside_b + last_outside_b) / (k)) + mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
        
        last_inside_b = inside_b[t];
        
        last_outside_b = outside_b[t];
        
        
      } // close local dd ifelse
      
    } else {
      
      
      if (local_dd == 1){
        
        // growth_mult =  std::min(1.0,last_inside_b / (plim * k * p_mpa));
        
        inside_b[t] =
          last_inside_b +  ((phi + 1) / phi) * r * last_inside_b * (1 - pow(last_inside_b / (k * p_mpa),phi)) - mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
        
        inside_b[inside_b < 0] = 1e-6;
        
        // growth_mult =  std::min(1.0,last_outside_b / (plim * k * (1  - p_mpa)));
    
        outside_b[t] =
          (1 - u) * last_outside_b + ((phi + 1) / phi) * r * last_outside_b * (1 - pow(last_outside_b / (k * (1 - p_mpa)),phi)) + mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
        
        outside_b[outside_b < 0] = 1e-6;
        
        last_inside_b = inside_b[t];
        
        last_outside_b = outside_b[t];
        
      } else {
        
        // growth_mult = std::min(1.0,(last_inside_b + last_outside_b) / (plim * k));
      
        inside_b[t] =
          last_inside_b + ((phi + 1) / phi) * p_mpa * r * (last_inside_b + last_outside_b)  * (1 - pow((last_inside_b + last_outside_b) / (k),phi)) - mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
        
        outside_b[t] =
          (1 - u) * last_outside_b +   ((phi + 1) / phi) * (1 - p_mpa) * r * (last_inside_b + last_outside_b) * (1 - pow((last_inside_b + last_outside_b) / (k),phi)) + mu * (last_inside_b - (p_mpa / (1 - p_mpa)) * last_outside_b);
        
        last_inside_b = inside_b[t];
        
        last_outside_b = outside_b[t];
        
        
      } // close local dd ifelse
      
    } // close PT ifelse statement
    
    yield[t] = u * outside_b[t];
    
  } // close years loop
  
  return Rcpp::List::create(
    Rcpp::Named("inside_b") = inside_b,
    Rcpp::Named("outside_b") = outside_b,
    Rcpp::Named("yield") = yield
  );
  
}
