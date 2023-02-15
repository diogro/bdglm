library(rethinking)
library(cmdstanr)



# Model with constant variance and one predictor

N = 1000
x = rnorm(N)

mu = 0.5 + 1 * x
y = rnorm(N, mu, sd = 1)

m1 = ulam(alist(
  y ~ normal(mu, sigma),
  mu <- a + b * x,
  c(a, b) ~ normal(0, 1),
  sigma ~ exponential(1)
), data = list(x = x, y = y),
chains = 4, cores = 4)

precis(m1)

# Model with one continuous predictor of the variance

x = rnorm(N)
mu = 0.5 + 1 * x
sigma = 1.2*exp(x)^0.3
# log(sigma) = 1.2 + 0.3 * x
y = rnorm(N, mu, sd = sigma)
plot(x, y)

m2 = ulam(alist(
  y ~ normal(mu, sigma),
  mu <- a + b * x,
  sigma <- a_s * exp(x) ^ b_s,
  c(a, b) ~ normal(0, 0.3),
  c(a_s, b_s) ~ normal(0, 0.3)
  ), data = list(x = x, y = y),
chains = 4, cores = 4)
precis(m2)


# Model with one categorical predictor of the variance

ls = sample(1:2, N, T)
sigmas = rexp(n = 2, rate = 1)
y = rnorm(N, mean = 1 + 1 * ls, sd = sigmas[ls])
plot(ls, y)

m3 = ulam(alist(
  y ~ normal(mu, sd),
  mu <- a[ls],
  sd <- sigma[ls],
  a[ls] ~ normal(0, 0.3),
  sigma[ls] ~ exponential(1)
), data = list(ls = ls, y = y),
chains = 4, cores = 4)

precis(m3, depth = 2)
samples = as.data.frame(extract.samples(m3))
diff_dist = samples$sigma.1 - samples$sigma.2
xlim = c(min(c(diff_dist, 0)) - 0.1,
max(c(diff_dist, 0)) + 0.1)
hist(diff_dist, xlim = xlim, breaks = 100)
abline(v = 0, col =2, lwd = 3)

samples$sigma.1 - samples$sigma.2

# Same as m3, but with a Robust likelihood that is less sensitive to outliers

ls = sample(1:2, N, T)
sigmas = rexp(n = 2, rate = 1)
y = rnorm(N, mean = 1 + 1 * (ls-1), sd = sigmas[ls])
plot(ls, y)

m4 = ulam(alist(
  y ~ student_t(nu, mu, sd),
  mu <- a[ls],
  sd <- sigma[ls],
  a[ls] ~ normal(0, 0.3),
  sigma[ls] ~ exponential(1),
  nu ~ gamma(2, 0.1)
), data = list(ls = ls, y = y),
chains = 4, cores = 4)

precis(m4, depth = 2)
