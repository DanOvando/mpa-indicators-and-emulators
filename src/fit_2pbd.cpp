#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  /* fit a two-patch biomass dynamics model (2pbd) */
  DATA_MATRIX(b_t_p);
  
  DATA_SCALAR(patch_size); // proportion of k present in the initial seeded patch
  
  DATA_SCALAR(log_phi);
  
  /* parameter block */
  
  PARAMETER(log_g); // PT growth rate
  
  PARAMETER(log_k); // PT total carrying capacity
  
  // PARAMETER(log_phi); // PT shape parameter
  
  PARAMETER(logit_mu); // movement rate
  
  PARAMETER(log_sigma_obs); // observation error
  
  PARAMETER(logit_init_dep);
  
  /* model */ 
  Type g = exp(log_g);
  
  Type k = exp(log_k);
  
  Type phi = exp(log_phi);
  
  Type mu = 1 / (1 + exp(-logit_mu));
  
  Type sigma_obs = exp(log_sigma_obs);
  
  Type nll = 0;
  
  Type init_dep;
  
  int years = b_t_p.rows();
  
  int patches = b_t_p.cols();
  
  init_dep = 1 / (1 + exp(-logit_init_dep));
  
  matrix<Type> hat_b_t_p(b_t_p.rows(),b_t_p.cols());
  
  hat_b_t_p(0,0) = k * init_dep; // when you add the density dependent option here, change this to k * patch_size * init_dep 
  
  nll -= dnorm(log(b_t_p(0,0)), log(hat_b_t_p(0,0)), sigma_obs, true);
  
  hat_b_t_p(0,1) = 0.0;
  
  for (int t = 1; t < years; t++){
    
    Type last_b = hat_b_t_p.row(t-1).sum();
    
    // calculate total growth
    
    Type growth_mult =  last_b / k;
    
    Type total_growth = g * last_b * (1 - last_b / k);
    
    // Type total_growth = growth_mult * ((phi + 1) / phi) * g * last_b  * (1 - pow(last_b / k,phi));
    
    // assign growth to each patch and apply movement
    
    hat_b_t_p(t,0) =  hat_b_t_p(t-1,0) + total_growth * patch_size  - mu * (hat_b_t_p(t-1,0) - (patch_size / (1 - patch_size)) * hat_b_t_p(t-1,1));
    
    hat_b_t_p(t,1) =  hat_b_t_p(t-1,1)  + total_growth * (1 - patch_size) + mu * (hat_b_t_p(t-1,0) - (patch_size / (1 - patch_size)) *  hat_b_t_p(t-1,1));
  
    nll -= dnorm(log(b_t_p(t,0)), log(hat_b_t_p(t,0)), sigma_obs, true);
  
    nll -= dnorm(log(b_t_p(t,1)), log(hat_b_t_p(t,1)), sigma_obs, true);
  
  
  }
  
  REPORT(hat_b_t_p);
  
  ADREPORT(hat_b_t_p);
  
  
  // f -= dnorm(x, mu, exp(logSigma), true).sum();

  return nll;

  /* Quick Reference
     ===============

     ** Macros to read data and declare parameters:

     _Template_Syntax_              _C++_type_                     _R_type_
     DATA_VECTOR(name)              vector<Type>                   vector
     DATA_MATRIX(name)              matrix<Type>                   matrix
     DATA_SCALAR(name)              Type                           numeric(1)
     DATA_INTEGER(name)             int                            integer(1)
     DATA_FACTOR(name)              vector<int>                    factor
     DATA_SPARSE_MATRIX(name)       Eigen::SparseMatrix<Type>      dgTMatrix
     DATA_ARRAY(name)               array<Type>                    array
     PARAMETER_MATRIX(name)         matrix<Type>                   matrix
     PARAMETER_VECTOR(name)         vector<Type>                   vector
     PARAMETER_ARRAY(name)          array<Type>                    array
     PARAMETER(name)                Type                           numeric(1)

     ** Macro to report intermediate expressions back to R:

     REPORT(x)
     ADREPORT(x)

     ** Basic constructors:

     vector<Type> v(n1);
     matrix<Type> m(n1,n2);
     array<Type> a(n1,n2,n3)

     ** Basic operations:

     v+v,v-v,v*v,v/v                Pointwise binary operations
     m*v                            Matrix-vector multiply
     a.col(i)                       R equivalent of a[,,i]
     a.col(i).col(j)                R equivalent of a[,j,i]
     a(i,j,k)                       R equivalent of a[i,j,k]
     exp(v)                         Pointwise math
     m(i,j)                         R equivalent of m[i,j]
     v.sum()                        R equivalent of sum(v)
     m.transpose()                  R equivalent of t(m)

     ** Distributions:

     Type dnbinom2(const Type &x, const Type &mu, const Type &var, int give_log=0)
     Type dpois(const Type &x, const Type &lambda, int give_log=0)
     Type dlgamma(Type y, Type shape, Type scale, int give_log=0)
     Type dnorm(Type x, Type mean, Type sd, int give_log=0)

     ** Parallel accumulator declaration (only methods "+=" and "-="):
     
     parallel_accumulator<Type> res(this);

  */

}
