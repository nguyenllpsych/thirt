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
#'                `step_size_sd` for the step size of new parameter generation,
#'                               either one value for all 4 parameters,
#'                               or a vector of 4 values in the following order:
#'                               theta, gamma, lambda, psisq
#' @param initial_params a list of initial parameters to start the algorithm,
#'                       each parameter needs to be a matrix
#'                       named theta, gamma, lambda, or psisq.
#' @param fixed_params a list of fixed parameters to be excluded from estimation,
#'                     each parameter needs to be a matrix
#'                     named theta, gamma, lambda, or psisq.
#' @param op_params a list of fixed parameters for operational items,
#'                  each parameter needs to be a matrix
#'                  named gamma, lambda, or psisq.
#'
#' @return a list of three objects if no operational items are provided:
#'         `all_iters` is a list of length `[n_iter]` for parameter estimates for all iterations,
#'         `mean_mcmc` is a list of length 4 for mean of four parameters after burn-ins,
#'         `sd_mcmc` is a list of length 4 for SD of four parameters after burn-ins.
#'         three additional objects are included if operational items are provided:
#'         `all_iters_test` includes all iterations for test items calibration,
#'         `mean_mcmc_test` includes parameter estimates for test items calibration,
#'         `sd_mcmc_test` includes parameter SD across iterations for test items calibration.
#'
#' @examples
#' \dontrun{
#' set.seed(202108)
#'
#' # designs
#' n_person      <- 100
#' n_item        <- 3
#' n_neg         <- 1
#' n_block       <- 2
#' n_dim         <- 4
#' n_iter        <- 1000
#' n_burnin      <- 20
#' step_size_sd  <- 0.1
#'
#' # simulate parameters
#' params <- simulate_thirt_params(n_person = n_person,
#'                                 n_item   = n_item,
#'                                 n_neg    = n_neg,
#'                                 n_block  = n_block,
#'                                 n_dim    = n_dim)
#' resp   <- do.call(simulate_thirt_resp, params)
#' gamma  <- params$gamma$gamma
#' lambda <- params$items$lambda
#' psisq  <- params$items$psisq
#' theta  <- params$persons[, -1]
#'
#' # operational parameters
#' op_gamma <- matrix(
#'   # first pair 1-2 is fixed for each of 2 blocks
#'   c(0.4, NA, NA,
#'    -0.9, NA, NA)
#'   )
#' op_lambda <- matrix(
#'   # first 2 items are fixed for each of 2 blocks
#'   c(-0.65, 0.55, NA,
#'     0.57, -0.40, NA)
#'   )
#' op_psisq <- matrix(
#'   # first 2 items are fixed for each of 2 blocks
#'   c(0.89, 0.72, NA,
#'     0.02, 0.41, NA)
#' )
#'
#' # estimation output
#' start_mcmc <- Sys.time()
#' output     <- estimate_thirt_params_mcmc(resp  = resp$resp,
#'                                          items = resp$items,
#'                                          control = list(n_iter   = n_iter,
#'                                                         n_burnin = n_burnin,
#'                                                         step_size_sd = step_size_sd),
#'                                          op_params = list(gamma = op_gamma,
#'                                                           lambda = op_lambda,
#'                                                           psisq = op_psisq)
#' )
#' end_mcmc   <- Sys.time()
#'
#' # correlate estimated and true parameters
#' diag(cor(theta, output$mean_mcmc$theta))
#' cor(gamma, output$mean_mcmc_test$gamma)
#' cor(lambda, output$mean_mcmc_test$lambda)
#' cor(psisq, output$mean_mcmc_test$psisq)
#' (time_mcmc <- end_mcmc - start_mcmc)
#' }
#'
#' @importFrom magrittr
#'             "%>%"
#' @importFrom mupp
#'             "find_all_permutations"
#' @export
estimate_thirt_params_mcmc <- function(resp,
                                       items,
                                       control = list(),
                                       initial_params = list(),
                                       fixed_params   = list(),
                                       op_params = list()) {

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

  # rep step_size_sd if only 1 input
  if("step_size_sd" %in% names(control) & !is.null(control$step_size_sd)) {
    control$step_size_sd <- rep_len(control$step_size_sd, 4)
  }
  # default controls
  control_default <- list(n_iter       = 10000,
                          n_burnin     = 1000,
                          step_size_sd = c(0.5,   # theta,
                                           0.01,  # gamma,
                                           0.01,  # lambda,
                                           0.075))# psisq
  assign(x     = "control",
         value = modifyList(x   = control_default,
                            val = control),
         envir = mcmc_envir)

  # fixed parameters
  initial_params <- modifyList(x   = initial_params,
                               val = fixed_params)
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

  #### WITHOUT FIELD-TEST ITEMS ####

  if(length(op_params) == 0) {

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
                          FUN = function(x) x[-c(1:mcmc_envir$control$n_burnin)]
    )

    # mean and sd of parameters after removing burn-ins
    mean_mcmc <- lapply(final_iters,
                        FUN = function(x) {
                          y = do.call(cbind, x)
                          y = array(y, dim=c(dim(as.matrix(x[[1]])), length(x)))
                          apply(X = y,
                                MARGIN = c(1, 2),
                                FUN = function(x) mean(x, na.rm = T))
    }) # END mean_mcmc lapply
    sd_mcmc  <- lapply(final_iters,
                       FUN = function(x) {
                         y = do.call(cbind, x)
                         y = array(y, dim=c(dim(as.matrix(x[[1]])), length(x)))
                         apply(X = y,
                               MARGIN = c(1, 2),
                               FUN = function(x) sd(x, na.rm = T))
    }) # END sd_mcmc lapply

    # return list
    return(list(all_iters = all_iters,
                mean_mcmc = mean_mcmc,
                sd_mcmc   = sd_mcmc))

  } # END IF no field test STATEMENT

  #### WITH FIELD-TEST ITEMS ####

  if(length(op_params) > 0) {

    # create indices for fixed operational items
    op_gamma_idx <- !is.na(op_params$gamma)
    op_lambda_idx <- !is.na(op_params$lambda)
    op_psisq_idx <- !is.na(op_params$psisq)

    # update items dataframe to exclude field test items
    #   using lambda indices -- should be the same for all item params
    op_items <- items[op_lambda_idx,]
    #   remove blocks with only one or fewer operational item
    op_block_idx <- op_items$block[table(op_items$block) > 1]
    op_items <- op_items[op_items$block %in% op_block_idx,]
    #   create new item indices
    op_items$new <- as.vector(
      unlist(sapply(unique(op_items$block), function(x) {
        seq_along(op_items$block[op_items$block == x])
        })))

    # update resp dataframe to have only operational items
    #   remove blocks with only one or fewer operational item
    op_resp <- resp[resp$block %in% op_block_idx,]
    #   initialize vectors of op items indices
    #   to be used for each block
    op_idx <- op_lambda_idx
    #   initialize dataframe for op_resp
    op_resp <- data.frame()
    #   iterate through blocks to clean up resp and append to op_resp
    for(block in seq(n_block)) {
      # extract test items for current blocks
      current_n_item   <- n_item[block]
      current_op_idx   <- op_idx[seq(current_n_item)]
      current_test     <- which(current_op_idx == FALSE)
      current_op_items <- op_items[op_items$block == block,]
      # check if current block has more than 1 operational item
      if(block %in% op_items$block) {

        # grab resp dataframe for the current block
        current_resp <- resp[resp$block == block,]

        # remove test items from seq
        current_resp$seq <- lapply(current_resp$seq,
                                   function(x) x[!(x %in% current_test)])
        current_resp$seq <- sapply(current_resp$seq,
                                   function(x) paste(x, collapse = ", "))

        # replace old item numbers with new item numbers
        for(i in seq_len(nrow(current_op_items))) {
          current_resp$seq <- gsub(
            pattern = current_op_items$item[i],
            replacement = current_op_items$new[i],
            x = current_resp$seq
          )
        }

        # update resp patterns
        #   all permutations for the operational items
        resp_ref <- as.data.frame(
          mupp::find_all_permutations(n = sum(current_op_idx),
                                      init = 1)
        )
        resp_ref$seq <- apply(resp_ref, 1, function(row) paste(row, collapse = ", "))
        resp_ref <- data.frame(
          resp = 1:nrow(resp_ref),
          seq  = resp_ref$seq
        )
        #   match seq to update current_resp with new resp indicator
        match_indices <- match(current_resp$seq, resp_ref$seq)
        current_resp$resp <- resp_ref$resp[match_indices]

        # remove the indices for this current block before moving on to next
        op_idx <- op_idx[-seq(current_n_item)]

      } else {
        # skip blocks with one or fewer operational item
        #   but first remove the indices for this current block
        op_idx <- op_idx[-seq(current_n_item)]
        current_resp <- data.frame()
      }

      # append current_resp to the op_resp dataframe
      op_resp <- rbind(op_resp, current_resp)
    } # END for block LOOP

    # replace item with new item keys in op_items
    op_items$item <- op_items$new
    op_items$new <- NULL

    # store original design in an object to restore to mcmc_envir after theta est
    original_design <- mcmc_envir$design

    # update test design to exclude field test items
    op_n_item       <- as.data.frame(table(op_items$block))[ , 2]
    op_block_size   <- choose2(op_n_item)
    op_n_block      <- length(unique(op_items$block))
    op_n_person     <- length(unique(op_resp$person))
    op_n_dim        <- length(unique(op_items$dim))
    assign(x     = "design",
           value = list(
             n_item     = op_n_item,
             block_size = op_block_size,
             n_block    = op_n_block,
             n_person   = op_n_person,
             n_dim      = op_n_dim,
             key        = op_items$key),
           envir = mcmc_envir)

    # ESTIMATE THETA FROM OPS ITEMS ----

    # fix all item parameters (because all items are operational)
    op_fixed_params   <- modifyList(x = fixed_params,
                                    val = list(
                                      gamma  = op_params$gamma[!is.na(op_params$gamma)],
                                      lambda = op_params$lambda[!is.na(op_params$lambda)],
                                      psisq  = op_params$psisq[!is.na(op_params$psisq)]))
    op_initial_params <- modifyList(x   = initial_params,
                                    val = op_fixed_params)

    # update envir arguments with initial params
    assign(x     = "arguments",
           value = modifyList(
             x   = initialize_thirt_params(resp  = op_resp,
                                           items = op_items,
                                           initial_params = op_initial_params,
                                           design = mcmc_envir$design),
             val = list(resp  = op_resp,
                        items = op_items)
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
                         char  = "score-",
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
                                                   fixed_params = op_fixed_params)

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
                          FUN = function(x) x[-c(1:mcmc_envir$control$n_burnin)]
    )

    # mean and sd of parameters after removing burn-ins
    mean_mcmc <- lapply(final_iters,
                        FUN = function(x) {
                          y = do.call(cbind, x)
                          y = array(y, dim=c(dim(as.matrix(x[[1]])), length(x)))
                          apply(X = y,
                                MARGIN = c(1, 2),
                                FUN = function(x) mean(x, na.rm = T))
                        }) # END mean_mcmc lapply
    sd_mcmc  <- lapply(final_iters,
                       FUN = function(x) {
                         y = do.call(cbind, x)
                         y = array(y, dim=c(dim(as.matrix(x[[1]])), length(x)))
                         apply(X = y,
                               MARGIN = c(1, 2),
                               FUN = function(x) sd(x, na.rm = T))
                       }) # END sd_mcmc lapply

    # ESTIMATE FIELD TEST ITEMS FROM OPS ITEMS AND FIXED THETAS ----
    # restore design to mcmc_envir
    assign(x     = "design",
           value = original_design,
           envir = mcmc_envir)
    # store operational items parameters in mcmc_envir
    #   this should only be present for this second step, NOT first step
    #   because first step only includes operational items,
    #   so they are covered under the fixed_params argument
    assign(x     = "operational",
           value = op_params,
           envir = mcmc_envir)

    # fix theta (because they have been estimated in step one)
    test_fixed_params   <- modifyList(x = fixed_params,
                                      val = list(
                                        theta  = mean_mcmc$theta))
    test_initial_params <- modifyList(x   = initial_params,
                                      val = list(
                                        theta = test_fixed_params))

    # initial parameters
    assign(x     = "arguments",
           value = modifyList(
             x   = initialize_thirt_params(resp  = resp,
                                           items = items,
                                           initial_params = test_initial_params,
                                           design = mcmc_envir$design),
             val = list(resp  = resp,
                        items = items)
           ),
           envir = mcmc_envir)

    # update initial arguments for fixed operational items
    mcmc_envir$arguments$gamma[op_gamma_idx] <- op_params$gamma[op_gamma_idx]
    mcmc_envir$arguments$lambda[op_lambda_idx] <- op_params$lambda[op_lambda_idx]
    mcmc_envir$arguments$psisq[op_psisq_idx] <- op_params$psisq[op_psisq_idx]

    # initial log-likelihood
    assign(x     = "loglik",
           value = do.call(what = loglik_thirt_mcmc,
                           args = append(mcmc_envir$arguments,
                                         list(design = mcmc_envir$design))),
           envir = mcmc_envir)

    # progress bar
    pb <- txtProgressBar(max   = mcmc_envir$control$n_iter,
                         char  = "items-",
                         style = 3)

    # all_iters list for all parameter estimates
    all_iters_test = list(gamma  = list(),
                          lambda = list(),
                          psisq  = list(),
                          theta  = list())

    # iterations
    for(iter in seq_len(mcmc_envir$control$n_iter)) {

      # update environment
      mcmc_envir <- estimate_thirt_params_mcmc_one(envir        = mcmc_envir,
                                                   fixed_params = test_fixed_params)

      # stores parameters
      all_iters_test$gamma[[iter]]  <- mcmc_envir$arguments$gamma
      all_iters_test$lambda[[iter]] <- mcmc_envir$arguments$lambda
      all_iters_test$psisq[[iter]]  <- mcmc_envir$arguments$psisq
      all_iters_test$theta[[iter]]  <- mcmc_envir$arguments$theta

      # progress bar
      setTxtProgressBar(pb    = pb,
                        value = iter)

    } # END for iter LOOP

    # remove burn-ins
    final_iters_test <- lapply(all_iters_test,
                               FUN = function(x) x[-c(1:mcmc_envir$control$n_burnin)]
    )
    final_iters_test <- final_iters_test[names(final_iters_test) != "theta"]

    # mean and sd of parameters after removing burn-ins
    mean_mcmc_test <- lapply(final_iters_test,
                        FUN = function(x) {
                          y = do.call(cbind, x)
                          y = array(y, dim=c(dim(as.matrix(x[[1]])), length(x)))
                          apply(X = y,
                                MARGIN = c(1, 2),
                                FUN = function(x) mean(x, na.rm = T))
                        }) # END mean_mcmc_test lapply
    sd_mcmc_test  <- lapply(final_iters_test,
                       FUN = function(x) {
                         y = do.call(cbind, x)
                         y = array(y, dim=c(dim(as.matrix(x[[1]])), length(x)))
                         apply(X = y,
                               MARGIN = c(1, 2),
                               FUN = function(x) sd(x, na.rm = T))
                       }) # END sd_mcmc_test lapply
  # return list
  return(list(all_iters = all_iters,
              mean_mcmc = mean_mcmc,
              sd_mcmc   = sd_mcmc,
              all_iters_test = all_iters_test,
              mean_mcmc_test = mean_mcmc_test,
              sd_mcmc_test   = sd_mcmc_test))

  } # END IF field test STATEMENT

} # END estimate_thirt_params_mcmc FUNCTION


## SINGLE ITERATION ##

estimate_thirt_params_mcmc_one <- function(envir,
                                           fixed_params) {

  # list of params and fixed params
  params_list   <- c("theta", "gamma", "lambda", "psisq")
  fixed_list    <- names(fixed_params)

  # update params for those not in fixed_params
  for(params in setdiff(params_list, fixed_list)) {

    # individual update functions
    update_fun_one <- paste0("update_thirt_", params, "_mcmc")
    update_fun     <- function(x) {
      do.call(what = update_fun_one,
              args = list(x))
    }

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

  # specify step size depending on parameter
  if(params_name == "gamma"){
    step_size_sd <- envir$control$step_size_sd[2]
  } else if(params_name == "lambda") {
    step_size_sd <- envir$control$step_size_sd[3]
  } else if(params_name == "psisq") {
    step_size_sd <- envir$control$step_size_sd[4]
  }

  # generate new item params
  params_old    <- envir$arguments[[params_name]]
  params_new    <- apply(X   = params_old,
                         FUN = function(x) {
                           generate_new_params(params_old   = x,
                                               step_size_sd = step_size_sd)
                         },
                         MARGIN = c(1,2)
  )

  # replace new params with fixed operational params if any
  if(!is.null(envir$operational[[params_name]])){

      # create T/F indices for whether an item is operational
      op_idx <- !is.na(envir$operational[[params_name]])

      # restore params_new to fixed for operational items
      params_new[op_idx] <- envir$operational[[params_name]][op_idx]
  }

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
  step_size_sd <- envir$control$step_size_sd[1]
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
                      block = rep(unique(items$block),
                                  times = design$block_size),
                      gamma = gamma)

  # run loglik_thirt() with correct params
  loglik_thirt(gamma   = gamma,
               items   = items,
               persons = persons,
               resp    = resp)
} # END loglik_thirt_mcmc FUNCTION


## INITIALIZE PARAMETERS ##
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
  A <- exp(loglik_new + prior_new - loglik_old - prior_old)
  A[is.na(A)] <- 0

  # random number gen
  U <- runif(n = length(A))

  # bool for acceptance of moving
  return (U < A)
} # END update_with_metrop FUNCTION


## ACCEPTANCE RATE COUNTER ##
#' Calculate acceptance rate of MCMC chain
#'
#' @param all_iters the output object from estimate_thirt_params_mcmc function
#'
#' @param resp a data.frame of length `[n_person x n_block]` with at least three first variables:
#'             variable `person` of the format `p` for person number `p`,
#'             variable `block` of the format `b` for block number `b`,
#'             variable `resp` of the format `r` for response number `r`
#'                which corresponds to mupp::find_permutation_index().
#' @export
count_accept <- function(all_iters,
                         resp) {

  n_iters       <- length(all_iters$gamma) - 1
  n_persons     <- length(unique(resp$resp$person))
  n_item        <- as.data.frame(table(resp$items$block))[ , 2]
  block_size    <- choose2(n_item)
  n_block       <- length(unique(resp$items$block))
  n_person      <- length(unique(resp$resp$person))
  n_dim         <- length(unique(resp$items$dim))

  gamma_index   <- rep(c(1:n_block), times = block_size)
  items_index   <- rep(c(1:n_block), times = n_item)
  persons_index <- rep(c(1:n_persons), times = n_dim)
  gamma_count   <- theta_count <- psisq_count <- lambda_count <- list()

  ## Gamma ##
  for (iters in seq(from = 2, to = n_iters)) {
    gamma_list <-
      as.numeric(all_iters$gamma[[iters]] != all_iters$gamma[[iters - 1]])
    gamma_count[[iters - 1]] <- tapply(X = gamma_list,
                                       INDEX = gamma_index,
                                       FUN = unique) %>% matrix()
  }
  gamma_count <- Reduce(`+`, gamma_count)/n_iters

  ## Lambda ##
  for (iters in seq(from = 2, to = n_iters)) {
    lambda_list <-
      as.numeric(all_iters$lambda[[iters]] != all_iters$lambda[[iters - 1]])
    lambda_count[[iters - 1]] <- tapply(X = lambda_list,
                                        INDEX = items_index,
                                        FUN = unique) %>% matrix()
  }
  lambda_count <- Reduce(`+`, lambda_count)/n_iters

  ## Psisq ##
  for (iters in seq(from = 2, to = n_iters)) {
    psisq_list <-
      as.numeric(all_iters$psisq[[iters]] != all_iters$psisq[[iters - 1]])
    psisq_count[[iters - 1]] <- tapply(X = psisq_list,
                                       INDEX = items_index,
                                       FUN = unique) %>% matrix()
  }
  psisq_count <- Reduce(`+`, psisq_count)/n_iters

  ## Theta ##
  for (iters in seq(from = 2, to = n_iters)) {
    theta_list <-
      as.numeric(all_iters$theta[[iters]] != all_iters$theta[[iters - 1]])
    theta_count[[iters - 1]] <- tapply(X = theta_list,
                                       INDEX = persons_index,
                                       FUN = unique) %>% matrix()
  }
  theta_count <- Reduce(`+`, theta_count)/n_iters


  return(list(gamma  = gamma_count,
              lambda = lambda_count,
              psisq  = psisq_count,
              theta  = theta_count))
}

