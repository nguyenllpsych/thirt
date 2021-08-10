#' Estimate parameters with MCMC
#'
#' @param resp a data.frame of length `[n_person x n_block]` with at least three first variables:
#'             variable `person` of the format `p` for person number `p`,
#'             variable `block` of the format `b` for block number `b`,
#'             variable `resp` of the format `r` for response number `r`
#'                which corresponds to mupp::find_permutation_index().
#' @param items a data.frame of length `[total items]` with five variables:
#'              variable `item` of the format `i` for item number `i`,
#'              variable `block` of the format `b` for block number `b`,
#'              variable `dim` of the format `d` for dimension number `d`,
#'              variable `key` with -1 for negatively- and 1 for positively-keyed items.
#' @param control a list of three parameters to control the MCMC algorithm:
#'                `n_iter` for the number of iterations,
#'                `n_burnin` for the number of burn-ins,
#'                `step_size_sd` for the step size of new parameter generation.
#' @param initial_params a list of initial parameters to start the algorithm.
#' @param fixed_params a list of fixed parameters to be excluded from estimation.
#'
#' @return a list of three objects:
#'         `all_iters` is a list of length `[n_iter]` for parameter estimates for all iterations,
#'         `mean_mcmc` is a list of length [4] for mean of four parameters after burn-ins,
#'         `sd_mcmc` is a list of length [4] for SD of four parameters after burn-ins.
#'
#' @examples
#' \dontrun{
#' set.seed(202108)
#'
#' # simulate parameters
#' params <- simulate_thirt_params(n_person = 5)
#' resp   <- do.call(simulate_thirt_resp, params)
#' gamma  <- params$gamma$gamma
#' lambda <- params$items$lambda
#' psisq  <- params$items$psisq
#' theta  <- params$persons[, -1]
#'
#' # estimation output
#' output <- estimate_thirt_params_mcmc(resp  = resp$resp,
#'                                      items = resp$items,
#'                                      control = list(n_iter = 5000,
#'                                                     n_burnin = 200,
#'                                                     step_size_sd = 0.1))
#'
#' # correlate estimated and true parameters
#' diag(cor(theta, output$mean_mcmc$theta))
#' cor(gamma, output$mean_mcmc$gamma)
#' cor(lambda, output$mean_mcmc$lambda)
#' cor(psisq,   output$mean_mcmc$psisq)
#' }
#'
#' @importFrom magrittr
#'             "%>%"
#' @export
estimate_thirt_params_mcmc <- function(resp,
                                       items,
                                       control = list(),
                                       initial_params = NULL,
                                       fixed_params   = NULL) {

  # set up new environment to store arguments
  mcmc_envir     <- new.env()

  # store test design for easy access
  n_item       <- as.data.frame(table(items$block))[ , 2]
  block_size   <- choose2(n_item)
  n_block      <- length(unique(items$block))
  n_person     <- length(unique(resp$person))
  n_dim        <- length(unique(items$dim))
  assign(x     = "design",
         value = list(
           n_item     = n_item,
           block_size = block_size,
           n_block    = n_block,
           n_person   = n_person,
           n_dim      = n_dim,
           key        = items$key),
         envir = mcmc_envir)

  # default controls
  control_default <- list(n_iter       = 10000,
                          n_burnin     = 1000,
                          step_size_sd = 0.1)
  assign(x     = "control",
         value = modifyList(x   = control_default,
                            val = control),
         envir = mcmc_envir)

  # initial parameters
  assign(x     = "arguments",
         value = modifyList(
           x   = initialize_thirt_params(resp  = resp,
                                         items = items,
                                         initial_params = initial_params,
                                         design = mcmc_envir$design),
           val = list(resp  = resp,
                      items = items)
         ),
         envir = mcmc_envir)

  # initial log-likelihood
  assign(x     = "loglik",
         value = do.call(what = loglik_thirt_mcmc,
                         args = append(mcmc_envir$arguments,
                                       list(design = mcmc_envir$design))),
         envir = mcmc_envir)

  # progress bar
  pb <- txtProgressBar(max   = mcmc_envir$control$n_iter,
                       char  = "mcmc",
                       style = 3)

  # all_iters list for all parameter estimates
  all_iters = list(gamma  = list(),
                   lambda = list(),
                   psisq  = list(),
                   theta  = list())

  # iterations
  for(iter in seq_len(mcmc_envir$control$n_iter)) {

    # update environment
    mcmc_envir <- estimate_thirt_params_mcmc_one(envir        = mcmc_envir,
                                                 fixed_params = fixed_params)

    # stores parameters
    all_iters$gamma[[iter]]  <- mcmc_envir$arguments$gamma
    all_iters$lambda[[iter]] <- mcmc_envir$arguments$lambda
    all_iters$psisq[[iter]]  <- mcmc_envir$arguments$psisq
    all_iters$theta[[iter]]  <- mcmc_envir$arguments$theta

    # progress bar
    setTxtProgressBar(pb    = pb,
                      value = iter)

  } # END for iter LOOP

  # remove burn-ins
  final_iters <- lapply(all_iters,
                        FUN = function(x) x[-mcmc_envir$control$n_burnin]
  )

  # mean and sd of parameters after removing burn-ins
  mean_mcmc <- lapply(final_iters,
                      FUN = function(x) {
                        y = do.call(cbind, x)
                        y = array(y, dim=c(dim(x[[1]]), length(x)))
                        apply(X = y,
                              MARGIN = c(1, 2),
                              FUN = function(x) mean(x, na.rm = T))
  }) # END mean_mcmc lapply
  sd_mcmc  <- lapply(final_iters,
                     FUN = function(x) {
                       y = do.call(cbind, x)
                       y = array(y, dim=c(dim(x[[1]]), length(x)))
                       apply(X = y,
                             MARGIN = c(1, 2),
                             FUN = function(x) sd(x, na.rm = T))
  }) # END sd_mcmc lapply

  # return list
  return(list(all_iters = all_iters,
              mean_mcmc = mean_mcmc,
              sd_mcmc   = sd_mcmc))

} # END estimate_thirt_params_mcmc FUNCTION


## SINGLE ITERATION ##

estimate_thirt_params_mcmc_one <- function(envir,
                                           fixed_params = c()) {

  # list of params and fixed params
  params_list   <- c("theta", "gamma", "lambda", "psisq")

  # update params for those not in fixed_params
  for(params in setdiff(params_list, fixed_params)) {

    # individual update functions
    update_fun_one <- paste0("update_thirt_", params, "_mcmc")
    update_fun     <- function(x) {
      do.call(what = update_fun_one,
              args = list(x))
    }
s
    # returns new environment
    envir <- update_fun(x = envir)
  } # END for params LOOP

  # return envir with updated parameters
  return(envir)

} # END estimate_thirt_params_mcmc_one FUNCTION


## UPDATE ITEMS PARAMETERS ##

update_thirt_gamma_mcmc  <- function(envir) {
  update_thirt_params_mcmc(envir = envir,
                           params_name = "gamma")
}
update_thirt_lambda_mcmc <- function(envir) {
  update_thirt_params_mcmc(envir = envir,
                           params_name = "lambda")
}
update_thirt_psisq_mcmc  <- function(envir) {
  update_thirt_params_mcmc(envir = envir,
                           params_name = "psisq")
}

update_thirt_params_mcmc <- function(envir,
                                     params_name = "gamma") {

  # generate new item params
  step_size_sd  <- envir$control$step_size_sd
  params_old    <- envir$arguments[[params_name]]
  params_new    <- apply(X   = params_old,
                         FUN = function(x) {
                           generate_new_params(params_old   = x,
                                               step_size_sd = step_size_sd)
                         },
                         MARGIN = c(1,2)
  )

  # update params in arguments to calculate log-likelihood
  envir$arguments[[params_name]] <- params_new

  # calculate old and new log-likelihoods for each block
  logliks       <- list(old   = envir$loglik,
                        new   = do.call(loglik_thirt_mcmc,
                                        append(envir$arguments, list(envir$design)))
  )
  logliks_items <- lapply(X   = logliks,
                          FUN = function(x) colSums(x, na.rm = T))

  # block index
  if (params_name == "gamma") {
    # gamma: block index for number of pairs per block
    block_index   <- rep(seq(length(envir$design$block_size)),
                         times = envir$design$block_size)
  } else {
    # other params: block index for number of items per block
    block_index   <- envir$arguments$items$block
  } # END block_index if else STATEMENT

  # pull out prior functions
  d_fun_one <- paste0("d_", params_name, "_prior")
  d_fun     <- function(x) do.call(what = d_fun_one, args = list(x))

  # calculate old and new priors per block
  if (params_name == "lambda") {
    priors_items <- lapply(X   = list(old = params_old,
                                      new = params_new),
                           FUN = function(x) {
                             priors <- d_lambda_prior(x = x, direction = envir$design$key)
                             tapply(X     = x,
                                    INDEX = block_index,
                                    FUN   = function(x) {
                                      sum(log(x = priors), na.rm = T)
                                    }) # END tapply
                           }) # END priors_items lapply

  } else {
    priors_items <- lapply(X   = list(old = params_old,
                                      new = params_new),
                           FUN = function(x) {
                             tapply(X     = x,
                                    INDEX = block_index,
                                    FUN   = function(x) {
                                      sum(log(d_fun(x)),
                                          na.rm = T)
                                    }) # END tapply
                           }) # END priors_items lapply
  }

  # accept and update new parameters or not
  block_new <- update_with_metrop(loglik_new = logliks_items$new,
                                  loglik_old = logliks_items$old,
                                  prior_new  = priors_items$new,
                                  prior_old  = priors_items$old)

  # re-format items_new to repeat T/F within block
  if (params_name == "gamma") {
    # gamma: replication represents number of pairs
    items_new <- rep(block_new, times = envir$design$block_size)
  } else {
    # other params: replication represents number of items
    items_new <- rep(block_new, times = envir$design$n_item)
  } # END if else STATEMENT

  # update params and loglik in environment arguments
  # block_new is T/F to identify whether update is needed per block
  # items_new is T/F to identify whether update is needed per item
  params_old[items_new, ]        <- params_new[items_new, ]
  envir$arguments[[params_name]] <- params_old
  envir$loglik[, block_new]      <- logliks$new[, block_new]

  # return envir with updated gammas
  return(envir)

} # END update_thirt_params_mcmc FUNCTION


## UPDATE THETAS ##

update_thirt_theta_mcmc <- function(envir) {

  # generate new thetas
  step_size_sd <- envir$control$step_size_sd
  thetas_old   <- envir$arguments$theta
  thetas_new   <- apply(X   = thetas_old,
                        FUN = function(x) {
                          generate_new_params(params_old   = x,
                                              step_size_sd = step_size_sd)
                        },
                        MARGIN = c(1,2)
  )

  # update params in arguments to calculate log-likelihood
  envir$arguments <- modifyList(x   = envir$arguments,
                                val = list(theta = thetas_new))

  # calculate old and new log-likelihoods
  logliks      <- list(old   = envir$loglik,
                       new   = do.call(loglik_thirt_mcmc,
                                       append(envir$arguments, list(envir$design)))
  )
  logliks_pers <- lapply(X   = logliks,
                         FUN = function(x) rowSums(x, na.rm = T))

  # calculate old and new priors
  priors_pers  <- lapply(X   = list(old = thetas_old,
                                    new = thetas_new),
                         FUN = function(x) {
                           log(d_thetas_prior(x)) %>%
                             rowSums(na.rm = T)
                         })

  # accept and update new parameters or not
  persons_new <- update_with_metrop(loglik_new = logliks_pers$new,
                                    loglik_old = logliks_pers$old,
                                    prior_new  = priors_pers$new,
                                    prior_old  = priors_pers$old)

  # update thetas in environment arguments
  # persons_new is T/F to identify whether update is needed
  thetas_old[persons_new, ]   <- thetas_new[persons_new, ]
  envir$arguments$theta       <- thetas_old
  envir$loglik[persons_new, ] <- logliks$new[persons_new, ]

  # return envir with updated thetas
  return(envir)

} # END update_thirt_theta_mcmc FUNCTION


## MODIFIED LOG LIKELIHOOD FUNCTION ##

#' @importFrom tibble
#'             rownames_to_column
loglik_thirt_mcmc <- function(gamma,
                              lambda,
                              psisq,
                              theta,
                              resp,
                              items,
                              design) {

  # resp and items: format of simulate_thirt_resp() output
  # gamma, lambda, psisq, theta: format of initialize_thirt_params() output
  # design: info about test design from envir

  # formulate items parameter for loglik_thirt()
  items <- as.data.frame(
    cbind(items, lambda, psisq)
  )

  # formulate persons parameter for loglik_thirt()
  persons <- as.data.frame(theta) %>%
    rownames_to_column(var = "persons")

  # formulate gamma parameter for loglik_thirt()
  pairs      <- c()
  for (block in seq_len(design$n_block)) {
    # all item pairs for each block
    combo       <- combn2(
      seq(from = 1,
          to   = design$n_item[block])
    )

    # append all pair names to pairs vector
    for (pair in seq(ncol(combo))){
      pairs <- append(pairs, paste0(combo[1, pair], "-", combo[2, pair]))
    } # END for pair LOOP
  } # END for block LOOP
  gamma <- data.frame(pair  = pairs,
                      block = rep(seq_len(design$n_block),
                                  times = design$block_size),
                      gamma = gamma)

  # run loglik_thirt() with correct params
  loglik_thirt(gamma   = gamma,
               items   = items,
               persons = persons,
               resp    = resp)
} # END loglik_thirt_mcmc FUNCTION


## INITIALIZE PARAMETERS ##
# TODO: specify some, not all, of one parameter

initialize_thirt_params <- function(resp, items,
                                    initial_params = NULL,
                                    design) {
  # pull out data characteristics
  # resp and items: format of simulate_thirt_resp() output
  # design: info about test design from envir
  params_list <- list("gamma", "lambda", "psisq", "theta")

  #argument checks for initial_params if not NULL
  if(!is.null(initial_params$gamma)
     && any(dim(initial_params$gamma) != c(sum(design$block_size), 1))) {
    stop("initial gamma must be a matrix with nrow equal total item pairs")
  }
  if(!is.null(initial_params$lambda)
     && any(dim(initial_params$lambda) != c(sum(design$n_item), 1))) {
    stop("initial lambda must be a matrix with nrow equal total items")
  }
  if(!is.null(initial_params$psisq)
     && any(dim(initial_params$psisq) != c(sum(design$n_item), 1))) {
    stop("initial psisq must be a matrix with nrow equal total items")
  }
  if(!is.null(initial_params$theta)
     && any(dim(initial_params$theta) != c(design$n_person, design$n_dim))) {
    stop("initial theta must be a matrix with dimensions [n_person X n_dim]")
  }

  # gamma pairs start at 0
  gamma <- matrix(0,
                  nrow = sum(design$block_size)) # all pairs across blocks

  # lambda start at 1 or -1 depending on item keys
  lambda <- matrix(design$key)

  # psisq start at 1
  psisq  <- matrix(1,
                   nrow = sum(design$n_item))

  # theta start at 0
  theta <- matrix(0,
                  nrow = design$n_person,
                  ncol = design$n_dim)

  # use custom initial_params if not NULL
  for (params in params_list) {
    if (!is.null(initial_params[[params]])) {
      assign(params, initial_params[[params]])
    }
  } # END for params LOOP

  # return list of individual parameters
  return(list(gamma = gamma,
              lambda = lambda,
              psisq = psisq,
              theta = theta))
} # END initialize_thirt_params FUNCTION


## GENERATE NEW PARAMETERS ##

generate_new_params <- function(params_old, step_size_sd) {
  rnorm(n = 1, mean = params_old, sd = step_size_sd)
} # END generate_new_params FUNCTION


## METROPOLIS HASTINGS UPDATES ##

update_with_metrop <- function(loglik_old, loglik_new,
                               prior_old, prior_new) {

  # acceptance probability
  A <- exp(loglik_new + prior_new) / exp(loglik_old + prior_old)
  A[is.na(A)] <- 0

  # random number gen
  U <- runif(n = length(A))

  # bool for acceptance of moving
  return (U < A)
} # END update_with_metrop FUNCTION
