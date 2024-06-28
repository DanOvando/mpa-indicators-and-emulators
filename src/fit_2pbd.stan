//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> n_t; // number of time steps
  int<lower=0> n_p; // number of patches
  matrix[n_t,n_p] b_t_p;
  real<upper = 1> patch_size;
  int<lower = 0, upper = 1> local_dd;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower = 1e-2,upper = 0.5> g;
  real log_k;
  real<upper = log(5)> log_phi; 
  real<lower = 0, upper = 1> mu;
  real<lower=1e-3> sigma;
  real<lower = 1e-3, upper = 1> init_dep;
}

transformed parameters{
  
real k;

real phi;

real last_b;

real total_growth;

real movement;

real one;

real two;

real growth_mult;

real plim;

phi = exp(log_phi);

k = exp(log_k);

matrix[n_t,n_p] hat_b_t_p = rep_matrix(0, n_t, n_p);

hat_b_t_p[1,1] = k * init_dep; // when you add the density dependent option here, change this to k * patch_size * init_dep 

for (t in 2:n_t){
  
  if (local_dd == 0){
  
    last_b = sum(hat_b_t_p[t - 1,1:n_p]);
  
    growth_mult =  1; //fmin(1.0,(last_b) / (plim * k));

    total_growth =  ((phi + 1) / phi) * g * last_b * (1 - pow(last_b / (k),phi));

    movement = mu * (hat_b_t_p[t-1,1] - (patch_size / (1 - patch_size)) * hat_b_t_p[t-1,2]);
    
    // assign growth to each patch and apply movement
    
    hat_b_t_p[t,1] =  fmax(1e-3,hat_b_t_p[t-1,1] + total_growth * patch_size  - movement);
    
    hat_b_t_p[t,2] =  fmax(1e-3,hat_b_t_p[t-1,2]  + total_growth * (1 - patch_size) + movement);
    
  } else {
    
  movement = mu * (hat_b_t_p[t-1,1] - (patch_size / (1 - patch_size)) * hat_b_t_p[t-1,2]);
  
  // assign growth to each patch and apply movement
  
  one = hat_b_t_p[t-1,1];
  
  two = hat_b_t_p[t-1,2];
  
  growth_mult =  1; //fmin(1.0,one / (plim * k * patch_size));

  
  // 
  hat_b_t_p[t,1] =  fmax(1e-3,hat_b_t_p[t-1,1] +  growth_mult * ((phi + 1) / phi) * g * hat_b_t_p[t-1,1] * (1 - pow(one / (k * patch_size),phi))  - movement);
  
  growth_mult =  1; //fmin(1.0,two / (plim * k * patch_size));

  hat_b_t_p[t,2] =  fmax(1e-3,hat_b_t_p[t-1,2]  + growth_mult * ((phi + 1) / phi) * g * hat_b_t_p[t-1,2] * (1 - pow(two / (k * (1 - patch_size)),phi)) + movement);
  // 
  //   
    
  }
  
} // close time loop
  
} // close transformed parameters

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  log(b_t_p[1,1]) ~ normal(log(hat_b_t_p[1,1]), sigma);
  
  for (t in 2:n_t){
    
    for (p in 1:n_p){
      
        log(b_t_p[t,p]) ~ normal(log(hat_b_t_p[t, p]), sigma);

    }
    
  }
  
  // log_k ~ normal(10,5);
  
}

