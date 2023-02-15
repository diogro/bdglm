m3_compiled = cmdstan_model(here::here("per_ls_sigma.stan"))

N = 1000
n_ls= 3
ls = sample(1:n_ls, N, T, prob = c(0.3, 0.2, 0.5))
sigmas = rexp(n = n_ls, rate = 1)
y = rnorm(N, mean = 0, sd = sigmas[ls])
plot(ls, y)
data_list = list(N = N, ls_N = n_ls, ls = ls, y = y)
m3_compiled$sample(data = data_list, chains = 4, parallel_chains = 4)
