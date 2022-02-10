##### Simulation Tests #####
# Package version date: 2022-02-10


#### Test one: example small design ####
set.seed(202202)

n_person      <- 100
n_item        <- 4
n_neg         <- 2
n_block       <- 4
n_dim         <- 4
n_iter        <- 5000
n_burnin      <- 200
step_size_sd  <- 0.1

# simulate parameters
params <- simulate_thirt_params(n_person = n_person,
                                n_item   = n_item,
                                n_neg    = n_neg,
                                n_block  = n_block,
                                n_dim    = n_dim)
resp   <- do.call(simulate_thirt_resp, params)
gamma  <- params$gamma$gamma
lambda <- params$items$lambda
psisq  <- params$items$psisq
theta  <- params$persons[, -1]

# estimation output
start_mcmc <- Sys.time()
output     <- estimate_thirt_params_mcmc(resp  = resp$resp,
                                         items = resp$items,
                                         control = list(n_iter   = n_iter,
                                                        n_burnin = n_burnin,
                                                        step_size_sd = step_size_sd)
)
end_mcmc   <- Sys.time()

# correlate estimated and true parameters
diag(cor(theta, output$mean_mcmc$theta)) # 0.90388 0.20548 0.73321 0.89955
cor(gamma, output$mean_mcmc$gamma)       # 0.94713
cor(lambda, output$mean_mcmc$lambda)     # 0.94506
cor(psisq, output$mean_mcmc$psisq)       # 0.81975
(time_mcmc <- end_mcmc - start_mcmc)     # 7.85801 mins


# acceptance count
count_accept <- count_accept(all_iters = output$all_iters,
                             resp      = resp)

# save info
save(n_person, n_item, n_neg, n_block, n_dim, n_iter, n_burnin, step_size_sd,
     params, output, count_accept, time_mcmc,
     file = "2022-02-10_Test01.RData")


#### Test two: more typical design ####
set.seed(202202)

n_person      <- 300
n_item        <- 4
n_neg         <- 2
n_block       <- 20
n_dim         <- 4
n_iter        <- 5000
n_burnin      <- 200
step_size_sd  <- 0.1

# simulate parameters
params <- simulate_thirt_params(n_person = n_person,
                                n_item   = n_item,
                                n_neg    = n_neg,
                                n_block  = n_block,
                                n_dim    = n_dim)
resp   <- do.call(simulate_thirt_resp, params)
gamma  <- params$gamma$gamma
lambda <- params$items$lambda
psisq  <- params$items$psisq
theta  <- params$persons[, -1]

# estimation output
start_mcmc <- Sys.time()
output     <- estimate_thirt_params_mcmc(resp  = resp$resp,
                                         items = resp$items,
                                         control = list(n_iter   = n_iter,
                                                        n_burnin = n_burnin,
                                                        step_size_sd = step_size_sd)
)
end_mcmc   <- Sys.time()

# correlate estimated and true parameters
diag(cor(theta, output$mean_mcmc$theta)) # 0.94984 0.95028 0.96410 0.95249
cor(gamma, output$mean_mcmc$gamma)       # 0.93434
cor(lambda, output$mean_mcmc$lambda)     # 0.88075
cor(psisq, output$mean_mcmc$psisq)       # 0.60734
(time_mcmc <- end_mcmc - start_mcmc)     # 51.5442 mins

# acceptance count
count_accept <- count_accept(all_iters = output$all_iters,
                             resp      = resp)

# save info
save(n_person, n_item, n_neg, n_block, n_dim, n_iter, n_burnin, step_size_sd,
     params, output, count_accept, time_mcmc,
     file = "2022-02-10_Test02.RData")
