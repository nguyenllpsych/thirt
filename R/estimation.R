#' Estimate parameters with MCMC
#'
#' @param resp
#' @param items
#' @param control
#' @param initial_params
#' @param fixed_params
#'
#' @return
#'
#' @examples
#'
#' @export
# TODO: set up fixed params
# TODO: set up single and multiple iterations
# TODO: set up controls
estimate_thirt_params_mcmc <- function(resp,
                                       items,
                                       control,
                                       initial_params = NULL,
                                       fixed_params   = NULL) {

  # set up new environment to store arguments
  mcmc_envir     <- new.env()

  # initial parameters
  assign(x     = "arguments",
         value = modifyList(
           x   = initialize_thirt_params(resp  = resp,
                                         items = items,
                                         initial_params = initial_params),
           val = list(resp  = resp,
                      items = items)
         ),
         envir = mcmc_envir)

  # initial log-likelihood
  assign(x     = "loglik",
         value = do.call(loglik_thirt_mcmc,
                         mcmc_envir$arguments),
         envir = mcmc_envir)
}


## TODO: UPDATE ITEMS AND GAMMA PARAMETERS ##
## TODO: pull out step_size_sd to store/edit in `control`

## UPDATE THETAS ##

#' @importFrom magrittr
#'             "%>%"
update_thirt_thetas_mcmc <- function(envir) {

  # generate new thetas
  thetas_old   <- envir$arguments$theta
  thetas_new   <- apply(X   = thetas_old,
                        FUN = function(x) generate_new_params(params_old = x,
                                                              step_size_sd = 0.1),
                        MARGIN = c(1,2)
  )

  # update params in arguments to calculate log-likelihood
  envir$arguments <- modifyList(x   = envir$arguments,
                                val = list(theta = thetas_new))

  # calculate old and new log-likelihoods
  logliks      <- list(old   = envir$loglik,
                       new   = do.call(loglik_thirt_mcmc, envir$arguments))
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
  envir$loglik[persons_new, ] <- logliks$new[persons_new, ]

  # return updated thetas
  return(thetas_old)

} # END update_thirt_thetas_mcmc FUNCTION

## MODIFIED LOG LIKELIHOOD FUNCTION ##

#' @importFrom tibble
#'             rownames_to_column
loglik_thirt_mcmc <- function(gamma,
                              lambda,
                              psisq,
                              theta,
                              resp,
                              items) {

  # resp and items: format of simulate_thirt_resp() output
  # gamma, lambda, psisq, theta: format of initialize_thirt_params() output

  # formulate items parameter for loglik_thirt()
  items <- as.data.frame(
    cbind(items, lambda, psisq)
  )

  # formulate persons parameter for loglik_thirt()
  persons <- as.data.frame(theta) %>%
    rownames_to_column(var = "persons")

  # formulate gamma parameter for loglik_thirt()
  n_item     <- as.data.frame(table(items$block))[ , 2]
  block_size <- choose2(n_item)
  n_block    <- length(unique(items$block))
  pairs      <- c()
  for (block in seq_len(n_block)) {
    # all item pairs for each block
    combo       <- combn2(
      seq(from = 1,
          to   = n_item[block])
    )

    # append all pair names to pairs vector
    for (pair in seq(ncol(combo))){
      pairs <- append(pairs, paste0(combo[1, pair], "-", combo[2, pair]))
    } # END for pair LOOP
  } # END for block LOOP
  gamma <- data.frame(pair  = pairs,
                      block = rep(seq_len(n_block), times = block_size),
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
                                    initial_params = NULL) {
  # pull out data characteristics
  # resp and items: format of simulate_thirt_resp() output
  n_block    <- length(unique(items$block))
  n_dim      <- length(unique(items$dim))
  n_person   <- length(unique(resp$person))
  n_item     <- as.data.frame(table(items$block))[ , 2]
  block_size <- choose2(n_item) #number of pairs per block
  params_list <- list("gamma", "lambda", "psisq", "theta")

  #argument checks for initial_params if not NULL
  if(!is.null(initial_params$gamma)
     && any(dim(initial_params$gamma) != c(sum(block_size), 1))) {
    stop("initial gamma must be a matrix with nrow equal total item pairs")
  }
  if(!is.null(initial_params$lambda)
     && any(dim(initial_params$lambda) != c(sum(n_item), 1))) {
    stop("initial lambda must be a matrix with nrow equal total items")
  }
  if(!is.null(initial_params$psisq)
     && any(dim(initial_params$psisq) != c(sum(n_item), 1))) {
    stop("initial psisq must be a matrix with nrow equal total items")
  }
  if(!is.null(initial_params$theta)
     && any(dim(initial_params$theta) != c(n_person, n_dim))) {
    stop("initial theta must be a matrix with dimensions [n_person X n_dim]")
  }

  # gamma pairs start at 0
  gamma <- matrix(0,
                  nrow = sum(block_size)) # all pairs across blocks

  # lambda start at 1 or -1
  lambda <- matrix(sample(c(-1, 1), size = sum(n_item), replace = T),
                   nrow = sum(n_item))

  # psisq start at 1
  psisq  <- matrix(1,
                   nrow = sum(n_item))

  # theta start at 0
  theta <- matrix(0,
                  nrow = n_person,
                  ncol = n_dim)

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

## GENERATE NEW PARAMETERS WITH STEP SIZE ##

generate_new_params <- function(params_old, step_size_sd) {
  rnorm(n = 1, mean = params_old, sd = step_size_sd)
} # END generate_new_params FUNCTION

## METROPOLIS HASTINGS UPDATES ##

update_with_metrop <- function(loglik_old, loglik_new,
                               prior_old, prior_new) {

  # acceptance probability
  A <- exp(loglik_new + prior_new) / exp(loglik_old + prior_old)

  # random number gen
  U <- runif(n = length(A))

  # bool for acceptance of moving
  return (U < A)
} # END update_with_metrop FUNCTION
