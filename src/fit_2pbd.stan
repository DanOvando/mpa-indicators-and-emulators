data {
  int<lower=0> n_t; // number of time steps
  int<lower=0> n_p; // number of patches
  matrix[n_t,n_p] b_t_p;
  real<upper = 1> mpa_size;
  int<lower = 0, upper = 1> local_dd;
}


parameters {
  real<lower = 1e-2,upper = 0.75> g;
  real<lower = 0, upper = log(10e9)> log_k;
  real<upper = log(2)> log_phi; 
  real<lower = 0, upper = 1> m;
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

real mu = m * (1 - mpa_size);

phi = exp(log_phi);

k = exp(log_k);

matrix[n_t,n_p] hat_b_t_p = rep_matrix(0, n_t, n_p);

hat_b_t_p[1,1] = k * init_dep; // when you add the density dependent option here, change this to k * mpa_size * init_dep 

for (t in 2:n_t){
  
  if (local_dd == 0){
  
    last_b = sum(hat_b_t_p[t - 1,1:n_p]);
  
    growth_mult =  1; //fmin(1.0,(last_b) / (plim * k));

    total_growth =  ((phi + 1) / phi) * g * last_b * (1 - pow(last_b / (k),phi));

    movement = mu * (hat_b_t_p[t-1,1] - (mpa_size / (1 - mpa_size)) * hat_b_t_p[t-1,2]);
    
    // assign growth to each patch and apply movement
    
    hat_b_t_p[t,1] =  fmax(1e-3,hat_b_t_p[t-1,1] + total_growth * mpa_size  - movement);
    
    hat_b_t_p[t,2] =  fmax(1e-3,hat_b_t_p[t-1,2]  + total_growth * (1 - mpa_size) + movement);
    
  } else {
    
  movement = mu * (hat_b_t_p[t-1,1] - (mpa_size / (1 - mpa_size)) * hat_b_t_p[t-1,2]);
  
  // assign growth to each patch and apply movement
  
  one = hat_b_t_p[t-1,1];
  
  two = hat_b_t_p[t-1,2];
  
  growth_mult =  1; //fmin(1.0,one / (plim * k * mpa_size));
  // 
  hat_b_t_p[t,1] =  fmax(1e-3,hat_b_t_p[t-1,1] +  growth_mult * ((phi + 1) / phi) * g * hat_b_t_p[t-1,1] * (1 - pow(one / (k * mpa_size),phi))  - movement);
  
  growth_mult =  1; //fmin(1.0,two / (plim * k * mpa_size));

  hat_b_t_p[t,2] =  fmax(1e-3,hat_b_t_p[t-1,2]  + growth_mult * ((phi + 1) / phi) * g * hat_b_t_p[t-1,2] * (1 - pow(two / (k * (1 - mpa_size)),phi)) + movement);
  // 
  //   
    
  }
  
} // close time loop
  
} // close transformed parameters

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  if (hat_b_t_p[1,1] <= 0){
        print(hat_b_t_p[1,1]);

  }

  log(b_t_p[1,1]) ~ normal(log(hat_b_t_p[1,1]), sigma);
  
  
  
  for (t in 2:n_t){
    
    for (p in 1:n_p){
      
        log(b_t_p[t,p]) ~ normal(log(hat_b_t_p[t, p]), sigma);

    }
    
  }
  
  log_k ~ normal(10,2);
  
  m ~ normal(0.2,.4);
  
  sigma ~ normal(0.05,.1);
  
  init_dep ~ normal(0.5,0.5);
  
  log_phi ~ normal(log(0.188),.4);

}

