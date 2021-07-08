#' Simulate parameters
#'
#' Generate parameters that conform to the Thurstonian IRT model.
#'
#' @param n_person integer indicating the number of people
#' @param n_item integer indicating the number of items per block
#' @param n_block integer indicating the number of blocks
#' @param n_dim integer indicating the number of dimensions or traits
#'
#' @return a list of parameters including gamma for item pairs,
#'         individual item parameters, and person parameters
#' @export
simulate_thirt_params <- function(n_person = 1,
                                  n_item   = 2,
                                  n_block  = 1,
                                  n_dim    = 2) {

  # argument checks #

  # we should make sure that n_person, n_item, n_block, n_dim are legal!

  # block size - number of pairs per block
  block_size <- choose2(n_item)

  # pair - all possible item pairs per block
  pairs <- c("")
  for (block in seq_len(n_block)) {
    combo <- combn2(seq(from = 1 + n_item * (block - 1),
                        to   = n_item * block))
    for (pair in seq_len(ncol(combo))){
      pairs[[pair + (block - 1) * block_size]] <- paste0(combo[1, pair], "-", combo[2, pair])
    } # END for pair LOOP
  } # END for block LOOP

  # data frame for gamma parameters for item pairs
  gamma   <- data.frame(
    pair  = pairs,
    gamma = r_gamma_prior(n = block_size))

  # data frame for item parameters
  item_params  <- data.frame(
    item   = seq(n_item * n_block),
    block  = seq(n_block),
    dim    = seq(n_dim),
    lambda = r_lambda_prior(n = n_item * n_block),
    psisq  = r_psisq_prior(n  = n_item * n_block))

  # data frame for person parameters
  person_params <- data.frame(
    person = seq(n_person))
  for (dim in seq(n_dim)) {
    person_params[dim+1] <- r_thetas_prior(n = n_person)
  } # END for dim LOOP
  names(person_params)[2:(n_dim+1)] <- paste0("theta_", seq(n_dim))

  # return lists
  return(list("gamma" = gamma,
              "item_params"   = item_params,
              "person_params" = person_params))
} # END simulate_thirt_params FUNCTION
