#' Calculate log-likelihood
#'
#' @param gamma a data.frame of length `[total binary outcomes]` with two variables:
#'              variable `pair` of the format `i-j` for item pair `ij`,
#'              variable `gamma` for threshold parameters.
#' @param items a data.frame of length `[total items]` with five variables:
#'              variable `item` of the format `i` for item number `i`,
#'              variable `block` of the format `b` for block number `b`,
#'              variable `dim` of the format `d` for dimension number `d`,
#'              variable `lambda` for loadings,
#'              variable `psisq` for uniqueness,
#'              variable `dim` for dimensions.
#' @param persons a data.frame of length `[number of people]` with variables:
#'                variable `person` of the format `p` for person number `p`,
#'                variables named `theta_d` for dimension number `d`.
#' @param resp a data.frame of length `[n_person x n_block]` with at least three first variables:
#'             variable `person` of the format `p` for person number `p`,
#'             variable `block` of the format `b` for block number `b`,
#'             variable `resp` of the format `r` for response number `r`
#'                which corresponds to mupp::find_permutation_index().
#'
#' @return a matrix of dimensions `[n_person X n_block]` of log-likelihoods
#'         of chosen responses for each person for each block.
#'
#' @examples
#'
#' \dontrun{
#' set.seed(202106)
#'
#' params   <- simulate_thirt_params()
#'
#' gamma    <- params$gamma
#' items    <- params$items
#' persons  <- params$persons
#' resp     <- do.call(simulate_thirt_resp, params)$resp
#'
#' loglik_thirt(gamma = gamma, items = items, persons = persons, resp = resp)
#' }
#'
#' @importFrom reshape2
#'             dcast
#'
#' @export
# TODO: compute probabilities for only selected responses, not all probabilities
loglik_thirt <- function(gamma, items, persons, resp) {

  # pull out important info
  n_block   <- length(unique(items$block))
  n_person  <- nrow(persons)

  # calculate all probabilities
  probs     <- p_thirtC(gamma = gamma, items = items, persons = persons)

  # resp to wide data.frame of dimension [n_person X n_block]
  resp_wide <- dcast(resp, person ~ block, value.var = "resp")[-1]

  # extract probabilities for selected responses
  loglik    <- matrix(nrow = n_person,
                      ncol = n_block)
  for (block in seq_len(n_block)) {
    # extract response number for each person in each block
    r       <- resp_wide[, block]

    # combine person and resp indices
    idx   <- cbind(seq_len(n_person), r)

    # extract associated probability
    p     <- probs[[block]][idx]

    # take the log
    loglik[, block] <- log(p)
  } # END for block LOOP

  return(loglik)
}
