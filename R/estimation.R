## MCMC STEPS ##


## INITIALIZE PARAMETERS ##

initialize_thirt_params <- function(resp, items,
                                    initial_params = NULL) {
  # pull out data characteristics
  # resp and items: format of simulate_thirt_resp output
  n_block    <- length(unique(items$block))
  n_dim      <- length(unique(items$dim))
  n_person   <- length(unique(resp$person))
  n_item     <- as.data.frame(table(items$block))[ , 2]
  block_size <- choose2(n_item) #number of pairs per block
  params_list <- list("gamma", "lambda", "psisq", "theta")

  #argument checks for initial_params if not NULL
  if(!is.null(initial_params$gamma)
     && any(dim(initial_params$gamma) != c(sum(block_size), 1))) {
    stop("initial gamma must a matrix with nrow equal total item pairs")
  }
  if(!is.null(initial_params$lambda)
     && any(dim(initial_params$lambda) != c(sum(n_item), 1))) {
    stop("initial lambda must a matrix with nrow equal total items")
  }
  if(!is.null(initial_params$psisq)
     && any(dim(initial_params$psisq) != c(sum(n_item), 1))) {
    stop("initial psisq must a matrix with nrow equal total items")
  }
  if(!is.null(initial_params$theta)
     && any(dim(initial_params$theta) != c(n_person, n_dim))) {
    stop("initial theta must a matrix with dimensions [n_person X n_dim]")
  }

  # gamma pairs start at 0
  gamma <- matrix(0,
                  nrow = sum(block_size)) # all pairs across blocks

  # lambda start at 0
  # TODO: separate starting values for positive and negative keys?
  lambda <- matrix(0,
                   nrow = sum(n_item))

  # psisq start at 0
  psisq  <- matrix(0,
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
  U <- runif(n = 1)

  # bool for acceptance of moving
  return (U < A)
} # END update_with_metrop FUNCTION
