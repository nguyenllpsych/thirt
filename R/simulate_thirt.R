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
simulate_thirt_params <- function(n_person = 2,
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
  combo <- combn2(seq(from = 1, to = n_item[1]))
  for (pair in seq_len(ncol(combo))) {
    pairs <- append(pairs, paste0(combo[1, pair], "-", combo[2, pair]))
  }

  # pairs for subsequent blocks
  if (n_block > 1) {

    # combinations per block for all subsequent blocks
    for (block in seq(2, n_block)) {
      combo <- combn2(seq(from = 1 + n_item[block - 1] * (block - 1),
                         to   = 1 + n_item[block - 1] * (block - 1) + n_item[block] - 1))

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
  return(list("gamma"   = gamma,
              "items"   = items,
              "persons" = persons))
} # END simulate_thirt_params FUNCTION


#' Simulate THIRT responses
#'
#' Generate responses that can be used for the Thurstonian IRT model.
#'
#' @param gamma a data.frame of length `[total binary outcomes]` with two variables:
#'              variable `pair` of the format `i-j` for item pair `ij`
#'              variable `gamma` for threshold parameters
#' @param items a data.frame of length [total items] with five variables:
#'              variable `item` of the format `i` for item number `i`,
#'              variable `block` of the format `b` for block number `b`,
#'              variable `dim` of the format `d` for dimension number `d`,
#'              variable `lambda` for loadings,
#'              variable `psisq` for uniqueness,
#'              variable `dim` for dimensions
#' @param persons a data.frame of length `[number of people]` with variables:
#'                variable `person` of the format `p` for person number `p`,
#'                variables named `theta_d` for dimension number `d`.
#'
#' @return a list with (1) data.frame `items` indicating items, blocks, and dimensions and
#'                     (2) data.frame `resp` indicating people, blocks, and response pattern.
#'                     Response patterns are numbered and compatible with mupp::find_permutation_order()
#'
#' @examples
#' \dontrun{
#' set.seed(202106)
#'
#' params <- simulate_thirt_params()
#'
#' do.call(simulate_thirt_resp, params)
#' }
#'
#' @export
simulate_thirt_resp <- function(gamma, items, persons) {

  # find probability of all possible response patterns given parameters
  probs <- p_thirt(gamma, items, persons)

  # simulate response patterns given the probabilities
  # a list of length n_block with the index of response patterns per block
  resp_ <- simulate_thirt_resp_(probs)

  # pull out names and characteristics of parameters
  item_name   <- names(items)[1]
  block_name  <- names(items)[2]
  dim_name    <- names(items)[3]
  person_name <- names(persons)[1]
  n_block     <- length(unique(items[ , block_name]))
  n_person    <- nrow(persons)

  # items object
  items       <- items[ , c(item_name, block_name, dim_name)]

  # resp object
  resp        <- data.frame(
    person = rep(seq_len(n_person), times   = n_block),
    block  = rep(seq_len(n_block),  each    = n_person),
    resp   = unlist(resp_))

  # return
  return(list(items = items,
              resp  = resp))
} # END simulate_thirt_resp FUNCTION

#######################
## Utility Functions ##
#######################

# wrapper to simulate responses across blocks of probabilities
simulate_thirt_resp_ <- function(probs){
  lapply(probs,
         FUN = simulate_thirt_resp1)
}

# simulate responses for one block of probabilities
simulate_thirt_resp1 <- function(probs){

  # convert names of response patterns to numeric
  # compatible with mupp::find_permutation_order()
  colnames(probs) <- seq_len(ncol(probs))

  # converting to cumulative sum
  probs <- t(apply(probs, MARGIN = 1, FUN = cumsum))

  # simulating response for everybody
  u     <- runif(n = nrow(probs))
  rowSums(u >= probs) + 1
}
