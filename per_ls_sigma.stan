data{
  int N;
  int ls_N;
  vector[N] y;
  int ls[N];
}
parameters{
  vector[ls_N] a;
  vector<lower=0>[ls_N] sigma;
}
model{
  vector[N] mu;
  vector[N] sigma_i;
  sigma ~ exponential( 1 );
  a ~ normal( 0 , 0.3 );
  for ( i in 1:N ) {
    mu[i] = a[ls[i]];
    sigma_i[i] = sigma[ls[i]];
  }
  y ~ normal( mu , sigma_i );
}
