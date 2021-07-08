#' Simulate parameters
#'
#' Generate parameters that conform to the Thurstonian IRT model.
#'
#' @param n_person integer indicating the number of people
#' @param n_item vector of length `[n_block]` indicating the number of items for each block
#' @param n_block integer indicating the number of blocks
#' @param n_dim integer indicating the number of dimensions or traits
#'
#' @return a list of parameters including gamma for item pairs,
#'         individual item parameters, and person parameters
#' @export
simulate_thirt_params <- function(n_person = 1,
                                  n_item   = c(2, 4),
                                  n_block  = 2,
                                  n_dim    = 3) {

  #####################
  ## argument checks ##
  #####################

  if (!is.numeric(n_person) || n_person < 1) {
    stop("n_person must be a number greater than 0")
  } # END n_person arg check

  if (any(!is.numeric(n_item)) || any(n_item < 2) || length(n_item) != n_block) {
    stop("n_item must be a vector of length n_block of integers greater than 1")
  } # END n_item arg check

  if (!is.numeric(n_block) || n_block < 1) {
    stop("n_block must be a number greater than 0")
  } # END n_block arg check

  if (!is.numeric(n_dim) || n_dim < 2) {
    stop("n_dim must be a number greater than 1")
  } # END n_dim arg check

  ################
  ## parameters ##
  ################

  # block size - number of pairs per block
  block_size <- choose2(n_item)

  # empty list for pair names across all blocks
  pairs <- c()

  # pairs for first block
  combo <- combn2(seq(from = 1, to = n_item[1]),
                  m = 2)
  for (pair in seq_len(ncol(combo))) {
    pairs <- append(pairs, paste0(combo[1, pair], "-", combo[2, pair]))
  }

  # pairs for subsequent blocks
  if (n_block > 1) {

    # combinations per block for all subsequent blocks
    for (block in seq(2, n_block)) {
      combo <- combn(seq(from = 1 + n_item[block - 1] * (block - 1),
                         to   = 1 + n_item[block - 1] * (block - 1) + n_item[block] - 1),
                     m = 2)

      # append all pair names to pairs vector
      for (pair in seq(ncol(combo))){
        pairs <- append(pairs, paste0(combo[1, pair], "-", combo[2, pair]))
      } # END for pair LOOP
    } # END for block LOOP
  } # END if n_block > 1 STATEMENT

  # data frame for gamma parameters for item pairs
  gamma   <- data.frame(
    pair  = pairs,
    gamma = r_gamma_prior(n = sum(block_size)))

  # data frame for item parameters
  items  <- data.frame(
    item   = seq(sum(n_item)),
    block  = rep(seq(n_block), times = n_item),
    dim    = seq(n_dim),
    lambda = r_lambda_prior(n = sum(n_item)),
    psisq  = r_psisq_prior(n  = sum(n_item)))

  # data frame for person parameters
  persons <- data.frame(
    person = seq(n_person))
  for (dim in seq(n_dim)) {
    persons[dim + 1] <- r_thetas_prior(n = n_person)
  } # END for dim LOOP
  names(persons)[2:(n_dim + 1)] <- paste0("theta_", seq(n_dim))

  # return lists
<<<<<<< HEAD
  return(list("gamma"   = gamma,
              "items"   = item,
              "persons" = persons))

=======
  return(list("gamma" = gamma,
              "item_params"   = item_params,
              "person_params" = person_params))
>>>>>>> c6a93012204e740083af14332b80ebbf76486779
} # END simulate_thirt_params FUNCTION
