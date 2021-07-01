#' Probability calculation
#'
#' Calculate the probability of all potential response patterns for each respondent
#'
#' @param n_item number of items per block
#' @param n_block number of blocks
#' @param n_person number of respondents
#' @param dict a data.frame with a `dim` column identifying traits by number `1, 2, 3...`
#'             and an `item` column identifying item indicators for different traits by number `1, 2, 3...`
#' @param gamma a data.frame with one variable of length `[total binary outcomes]`,
#'              row names should be of the format `i-j` for item pair `ij`
#' @param lambda a data.frame with one variable of length `[total items]`,
#'               row names should be of the format `i` for item `i`
#' @param theta a data.frame of dimension `[number of people X number of dimensions]`,
#'              row names should be of the format `p` for person `p`
#' @param psisq a data.frame with one variable of length `[total items]`,
#'              row names should be of the format `i` for item `i`
#'
#' @return a list of `[permutation_list, probability]`,
#'         permutation_list is a matrix of dimension `[number of permutations X number of items]`,
#'         probability is a matrix of dimension `[person X permutation]` of probability for each response pattern
#'
#' @examples
#' \dontrun{
#' set.seed(202106)
#'
#' #####################
#' ## Test Conditions ##
#' #####################
#'
#' n_item    = 4
#' n_block   = 1
#' n_dim     = 4
#' n_person  = 200
#' block_size <- n_item * (n_item - 1) / 2
#'
#' ################
#' ## Parameters ##
#' ################
#'
#' # thresholds - gamma
#' # vector of length [total binary outcomes]
#' # row names should be of the format "i-j" for item pair ij
#' gamma   <- data.frame(gamma = round(runif(n = block_size * n_block, min = -1,  max = 1), digits = 3))
#' row.names(gamma) <- c("1-2", "1-3", "1-4", "2-3", "2-4", "3-4")
#'
#' # loadings - lambda
#' # vector of length [total items]
#' # row names should be of the format "i" for item i
#' lambda  <- data.frame(lambda = round(runif(n = n_item * n_block, min = .65, max = .95), digits = 3))
#'
#' # uniqueness - psi^2
#' # vector of length [total items]
#' # row names should be of the format "i" for item i
#' psisq   <- data.frame(psisq = round(1 - lambda^2, digits = 3))
#'
#' # theta - trait scores
#' # matrix of dimension [number of people X number of dimensions]
#' # row names should be of the format "p" for person p
#' theta   <- as.data.frame(matrix(nrow = n_person, ncol = n_dim))
#' for (dim in seq(n_dim)) {
#'   theta[dim] <- round(rnorm(n = n_person, mean = 0, sd = 1), digits = 3)
#' } # END for dim LOOP
#'
#' # create a dictionary with a "dim" column identifying traits
#' #   and an "item" column identifying item indicators for different traits
#' dict <- data.frame(
#'   dim = c(seq(n_dim)),
#'   item = c(seq(n_item * n_block)))
#'
#' ###################
#' ## Test Function ##
#' ###################
#'
#' # test function with fixed parameters
#' prob <- p_thirt(n_item, n_block, n_person, dict,
#'                 gamma, lambda, theta, psisq)
#'
#' prob$permutation_list
#' summary(prob$probability)
#' }
#'
#' @export
p_thirt <- function(n_item, n_block, n_person, dict,
                    gamma, lambda, theta, psisq) {

  # calculate block size
  block_size <- n_item * (n_item - 1) / 2

  ######################
  ## all permutations ##
  ######################

  # find all permutations for one block
  permutation_list <- as.data.frame(
    mupp::find_all_permutations(n = n_item, init = 1)
  )

  # find all pair combinations
  combo <- combn(n_item, 2)

  # find all pair combinations specific to each permutation
  for (col in seq(ncol(combo))) {
    for (row in seq(nrow(permutation_list))) {

      # add a new variable in permutation_list for each pair in combo
      permutation_list[row, paste0(combo[, col][1], "-", combo[, col][2])] =

        # if item i in pair ij is ranked higher than item j
        #   -> variable i-j has value 1, else 0
        as.numeric(
          grep(paste0(combo[, col][1]), permutation_list[row, seq(n_item)])
            < grep(paste0(combo[, col][2]), permutation_list[row, seq(n_item)]))
    } # END for row LOOP
  } # END for col LOOP

  #################
  ## probability ##
  #################

  # create [person X permutation] data frame with probabilities of 1
  probability <- as.data.frame(
    matrix(1,
           nrow = n_person,
           ncol = nrow(permutation_list))
  )

  # joint probability for each permutation
  for (person in seq(nrow(probability))) {
    for (resp in seq(ncol(probability))) {
      for (pair in colnames(tail(permutation_list, c(0, -n_item)))){

        # p_thirt_one calculations
        p_one <- p_thirt_one(gamma   = gamma[paste0(pair),],
                             lambda1 = lambda[split_pair(pair, 1),],
                             lambda2 = lambda[split_pair(pair, 2),],
                             theta1  = theta[
                               # select row in theta for person
                               person,
                               # select column in theta for the correct dim
                               dict[
                                 # select the dim from dict for the item
                                 which(dict$item == split_pair(pair, 1)),]$dim],
                             theta2  = theta[
                               # select row in theta for the person
                               person,
                               # select column in theta for the correct dim
                               dict[
                                 # select the dim from dict for the item
                                 which(dict$item == split_pair(pair, 2)),]$dim],
                             psisq1  = psisq[split_pair(pair, 1),],
                             psisq2  = psisq[split_pair(pair, 2),])

        # joint multiply all pair-wise probabilities
        probability[person, resp] = probability[person, resp] *

          # p_one if pair == 1
          p_one ^ permutation_list[resp, pair] *

          # p_one if pair == 0
          (1 - p_one) ^ (1 - permutation_list[resp, pair])
      } # END for pair LOOP
    } # END for resp LOOP
  } # END for person LOOP

  # return list of permutation and probability
  # permutation_list: a matrix of [permutations X items]
  # probability: a matrix of [person X permutation] of probabilities
  return_list <- list("permutation_list" = permutation_list[,seq(n_item)],
                      "probability"      = probability)
  return(return_list)

  # TO DO: multiple blocks
} # END p_thirt FUNCTION

#######################
## Utility Functions ##
#######################

# individual pair-wise probability
p_thirt_one <- function(gamma, lambda1, lambda2,
                        theta1, theta2, psisq1, psisq2) {
  prob = pnorm((-gamma + lambda1 * theta1 - lambda2 * theta2)/
                 sqrt(psisq1^2 + psisq2^2))
  return(prob)
} # END p_thirt_one FUNCTION

# split pair name to select first or second item
split_pair <- function(pair, item) {
  return(unlist(strsplit(pair, split = "-"))[item])
} # END split_pair FUNCTION
