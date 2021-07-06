#' Probability calculation
#'
#' Calculate the probability of all potential response patterns for each respondent
#'
#' @param gamma a data.frame of length `[total binary outcomes]` with two variables:
#'              variable `pair` of the format `i-j` for item pair `ij`
#'              variable `gamma` for threshold parameters
#' @param item_params a data.frame of length [total items] with five variables:
#'                    variable `item` of the format `i` for item number `i`,
#'                    variable `block` of the format `b` for block number `b`,
#'                    variable `dim` of the format `d` for dimension number `d`,
#'                    variable `lambda` for loadings,
#'                    variable `psisq` for uniqueness,
#'                    variable `dim` for dimensions
#' @param person_params a data.frame of length `[number of people]` with variables:
#'                    variable `person` of the format `p` for person number `p`,
#'                    variables named `theta_d` for dimension number `d`.
#'
#' @return a matrix of dimension `[person X permutation]` of probability
#'         for each response pattern across blocks
#'
#' @examples
#' \dontrun{
#' set.seed(202106)
#'
#' params <- simulate_thirt_params(n_person = 200,
#'                                 n_item = 3,
#'                                 n_block = 2,
#'                                 n_dim = 3)
#' gamma          <- params$gamma
#' item_params    <- params$item_params
#' person_params  <- params$person_params
#'
#' prob <- p_thirt(gamma, item_params, person_params)
#'
#' summary(prob)
#' }
#'
#' @importFrom mupp
#'             find_all_permutations
#'
#' @export
p_thirt <- function(gamma, item_params, person_params) {

  ##################
  ## test designs ##
  ##################

  # number of blocks, dimensions, people, items per block
  n_block  <- length(unique(item_params$block))
  n_dim    <- length(unique(item_params$dim))
  n_person <- nrow(person_params)
  n_item   <- nrow(item_params)/n_block

  # item parameters as individual data frames
  lambda  <- data.frame(lambda = item_params$lambda)
  psisq   <- data.frame(psisq  = item_params$psisq)
  dict    <- data.frame(item   = seq(nrow(item_params)),
                        dim    = item_params$dim)

  # person parameters with only thetas
  theta   <- person_params[,-1]

  ######################
  ## all permutations ##
  ######################

  # create an empty list of size n_block
  permutation_list <- vector("list", n_block)

  # create an empty data frame size [n_permutation X block]
  permutation_list_id <- as.data.frame(
    matrix(nrow = nrow(find_all_permutations(n = n_item, init = 1)),
           ncol = n_block)
  )

  # add each block permutation to the list
  for (block in seq(n_block)) {

    # create permutation list for each block
    permutation_block <- as.data.frame(
      find_all_permutations(n = n_item,
                            init = 1 + (block - 1) * n_item))

    # id variable to identify each response pattern in a block
    # so later different blocks can be combined in a smaller matrix of ids
    permutation_block$id <- paste0(

      # block identifier
      "b", block,

      # response pattern identifier
      "r", seq(nrow(
        find_all_permutations(n = n_item,
                              init = 1 + (block - 1) * n_item))))

    # append each block's permutation to a large permutation list
    permutation_list[[block]] <- permutation_block

    # append only block/resp id to the data frame permutation_list_id
    permutation_list_id[,block] <-  permutation_block$id

  } # END for block LOOP

  # create a list of all permutations across blocks
  permutation_list_id <- expand.grid(permutation_list_id, stringsAsFactors = F)

  # find all pair combinations specific to each permutation
  permutation_list <- lapply(permutation_list, function(x) {
    combo <- combn(min(x$V1):(n_item + min(x$V1) - 1), 2)
    for (col in seq(ncol(combo))) {
      for (row in seq(nrow(x))) {

        # add a new variable in permutation_list for each pair in combo
        x[row, paste0(combo[, col][1], "-", combo[, col][2])] =

          # if item i in pair ij is ranked higher than item j
          #   -> variable i-j has value 1, else 0
          as.numeric(
            grep(paste0(combo[, col][1]), x[row, seq(n_item)])
              < grep(paste0(combo[, col][2]), x[row, seq(n_item)]))
      } # END for row LOOP
    } # END for col LOOP
  x}) # END lapply

  #################
  ## probability ##
  #################

  # create an empty list of size n_block
  probability_list <- vector("list", n_block)

  # probability_list is a [person X (permutation X block)] data frame
  for (block in seq(n_block)) {
    probability <- as.data.frame(
      matrix(1,
             nrow = n_person,
             ncol = nrow(permutation_list[[block]]))
    )

    # joint probability for each permutation per block
    for (person in seq(nrow(probability))) {
      for (resp in seq(ncol(probability))) {
        for (pair in colnames(tail(permutation_list[[block]], c(0, -(n_item+1))))){

          # p_thirt_one calculations
          p_one <- p_thirt_one(gamma   = gamma[which(gamma$pair == "1-2"),]$gamma,
                               lambda1 = lambda[split_pair(pair, 1),],
                               lambda2 = lambda[split_pair(pair, 2),],
                               theta1  = theta[
                                 # select row in person_params for person
                                 person,
                                 # select column in person_params for the correct dim
                                 dict[
                                   # select the dim from dict for the item
                                   which(dict$item == split_pair(pair, 1)),]$dim],
                               theta2  = theta[
                                 # select row in person_params for the person
                                 person,
                                 # select column in person_params for the correct dim
                                 dict[
                                   # select the dim from dict for the item
                                   which(dict$item == split_pair(pair, 2)),]$dim],
                               psisq1  = psisq[split_pair(pair, 1),],
                               psisq2  = psisq[split_pair(pair, 2),])

          # joint multiply all pair-wise probabilities
          probability[person, resp] = probability[person, resp] *

            # p_one if pair == 1
            p_one ^ permutation_list[[block]][resp, pair] *

            # p_one if pair == 0
            (1 - p_one) ^ (1 - permutation_list[[block]][resp, pair])

        } # END for pair LOOP
      } # END for resp LOOP
    } # END for person LOOP

    # rename variables in probability matrix to reflect block/resp
    # format: "b", [block number], "r", [response number]
    # e.g.  : b1r1 = 1st response pattern in 1st block
    # block/resp keys are in permutation_list[[block]]
    names(probability) <- c(permutation_list[[block]]$id)

    # append each block's probability matrix to large probability_list
    probability_list[[block]] <- probability

  } # END for block LOOP

  # combine probabilities for all blocks in probability_list
  probability_list <- Reduce(merge_df, probability_list)

  # probability_matrix for all cross-block permutations and probabilities
  probability_matrix <- as.data.frame(
    matrix(1,
           nrow = n_person,
           ncol = nrow(permutation_list_id))
  )

  # for each block/resp pattern in permutation_list_id
  for (respID in seq(nrow(permutation_list_id))) {

    # for each block pattern ("b1r1")
    for (blockID in permutation_list_id[respID, ]) {

      # joint multiply to compute each column of [person x 1] probabilities in probability_matrix
      probability_matrix[, respID] = probability_matrix[, respID] *
        probability_list[, blockID]
    } # END for blockID LOOP
  } # END for respID LOOP

  # return probability_matrix: [person X permutation] probabilities
  return(probability_matrix)

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

# merge all data frames in a list
merge_df <- function(df1, df2) {
  cbind(df1, df2)
}
