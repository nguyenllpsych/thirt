#' Probability calculation
#'
#' Calculate the probability of all potential response patterns for each respondent
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
#' @return a list of length `[block]` of matrices with dimension `[person X permutation]`
#'         of probabilities for each response pattern per block
#'
#' @examples
#' \dontrun{
#' set.seed(202106)
#'
#' params <- simulate_thirt_params(n_person = 200,
#'                                 n_item = 3,
#'                                 n_block = 2,
#'                                 n_dim = 3)
#'
#' prob <- do.call(p_thirt, params)
#'
#' for(block in seq(length(prob))){
#'   print(summary(prob[[block]]))
#' }
#'
#' @importFrom mupp
#'             find_all_permutations
#'
#' @export
p_thirt <- function(gamma, items, persons) {

  ##################
  ## test designs ##
  ##################

  # number of blocks, dimensions, people, items per block
  n_block  <- length(unique(items$block))
  n_dim    <- length(unique(items$dim))
  n_person <- nrow(persons)
  n_item   <- as.data.frame(table(items$block))[ , 2]

  # item parameters as individual data frames
  lambda   <- data.frame(lambda = items$lambda)
  psisq    <- data.frame(psisq  = items$psisq)
  dict     <- data.frame(item   = items$item,
                         dim    = items$dim)

  # person parameters with only thetas
  theta    <- persons[ , -1]

  ######################
  ## all permutations ##
  ######################

  # create an empty list of size n_block
  permutation_list <- vector("list", n_block)

  # add each block permutation to the list
  for (block in seq_len(n_block)) {

    # create permutation list for each block
    permutation_block <- as.data.frame(
      find_all_permutations(n    = n_item[block],
                            init = ifelse(
                              # for first block, init = 1
                              block == 1, 1,

                              # for subsequent block, init =
                              1 + n_item[block - 1] * (block - 1)))
    )

    # id variable to identify each response pattern in a block
    # so later different blocks can be combined in a smaller matrix of ids
    permutation_block$id <- paste0(

      # block identifier
      "b", block,

      # response pattern identifier
      "r", seq(nrow(permutation_block))
    )

    # append each block's permutation to a large permutation list
    permutation_list[[block]]     <- permutation_block
  } # END for block LOOP

  # find all pair combinations specific to each permutation
  permutation_list <- lapply(
    X   = permutation_list,
    FUN = function(x) {

      # the combinations and names of those combinations
      combo <- combn2(seq(min(x$V1), max(x$V1)))
      nms   <- paste0(combo[1, ], "-", combo[2, ])

      # the values and the column that each preference appears
      vals  <- as.matrix(x[ , -ncol(x)])
      cols  <- col(vals)

      # determining whether combination is higher
      for(col in seq_len(ncol(combo))){
        col1           <- rowSums(cols * (vals == combo[1, col]))
        col2           <- rowSums(cols * (vals == combo[2, col]))
        x[ , nms[col]] <- as.numeric(col2 > col1)
      }

      x
  }) # END lapply

  ###########################
  ## probability pair-wise ##
  ###########################

  # create an empty list of size n_block
  probability_pair <- vector("list", n_block)

  # probability_pair is list of size [n_block] with [person X block_size] matrices
  for (block in seq_len(n_block)) {

    # for each block create an empty matrix of size [person X block_size]
    probability_pair[[block]] <-
      matrix(NA,
             nrow = n_person,
             ncol = ncol(tail(
               permutation_list[[block]], c(0, -(n_item[block] + 1))))
    )

    # column names of each matrix indicate the item pairs in a block
    colnames(probability_pair[[block]]) <- colnames(
      tail(permutation_list[[block]], c(0, -(n_item[block] + 1)))
    )

    # populate matrices with pair-wise probabilities
    for (pair in colnames(probability_pair[[block]])) {

      # index everything
      pair1     <- split_pair(pair, 1)
      pair2     <- split_pair(pair, 2)

      gamma_idx <- gamma$pair == pair
      dim1      <- dict[dict$item == pair1, ]$dim
      dim2      <- dict[dict$item == pair2, ]$dim

      # p_thirt_one calculations
      probability_pair[[block]][ , pair] <-
        p_thirt_one(gamma   = gamma[gamma_idx, ]$gamma,
                    lambda1 = lambda[pair1, ],
                    lambda2 = lambda[pair2, ],
                    theta1  = theta[ , dim1],
                    theta2  = theta[ , dim2],
                    psisq1  = psisq[pair1, ],
                    psisq2  = psisq[pair2, ])
    }

    # add probabilities of reverse response patterns
    # with pair names formatted for easier access
    names <- colnames(probability_pair[[block]])
    for (col in seq_len(ncol(probability_pair[[block]]))) {

      # create and bind new col = 1 - old col
      new_col = 1 - probability_pair[[block]][ , col]
      probability_pair[[block]] = cbind(probability_pair[[block]], new_col)

      # pull out old column names, e.g. "1-2"
      old_names <- colnames(probability_pair[[block]])[col]

      # correct format for new pair names, e.g. "2-1"
      names <- append(names,
                      paste0(
                        # 2nd item switched to 1st position
                        split_pair(old_names, 2),
                        "-",
                        split_pair(old_names, 1)))
      colnames(probability_pair[[block]]) = names
    } # END for col LOOP
  } # END for block LOOP


  #################################
  ## probability ranked sequence ##
  #################################

  # create an empty list of size n_block
  probability_list <- vector("list", n_block)

  # probability_list is a list of length [n_block]
  #   that includes data frames `probability` of size [n_person X permutation]
  #   for probabilities of different response patterns per block
  for (block in seq_len(n_block)) {
    probability <- matrix(1,
                          nrow = n_person,
                          ncol = nrow(permutation_list[[block]]))

    # rename variables in probability matrix to reflect block/resp
    # format: "b", [block number], "r", [response number]
    # e.g.  : b1r1 = 1st response pattern in 1st block
    # block/resp keys are in permutation_list[[block]]
    colnames(probability) <- c(permutation_list[[block]]$id)

    # probability for each permutation per block
    # iterate through response patterns per block
    for (resp in colnames(probability)) {

      # iterate through number of separate probabilities to joint multiply
      # moving from the end/smaller probability first
      # e.g. 2 master probabilities for 3 items: p(1 > 2, 3) * p(2 > 3)
      for (probmaster in seq_len(n_item[block] - 1)) {

        # iterate through number of probabilities in the numerator
        # num_list is a vector of pairwise probability names
        # e.g. for master probability p(1 > 2, 3):
        #      numerator = p(1 > 2) * p(1 > 3)
        #      num_list  = c("1-2", "1-3")
        num_list <- c()
        for (numlen in seq_len(probmaster)) {
          num = join_pair(df   = permutation_list[[block]],
                          resp = resp,
                          col = c(n_item[block] - probmaster,
                                  n_item[block] - (probmaster - 1 * numlen)))
          num_list <- append(num_list, num)
        } # END for numlen LOOP

        # create a list of all items present in the numerators
        item_list <- unique(as.numeric(split_pair(num_list)))

        # create a list of all denominators that correspond to a numerator
        #   each element in den_list is a vector
        #   e.g. probmaster   = p(1 > 2, 3)
        #        numerator    = p(1 > 2) * p(1 > 3)
        #        denominator  = [p(1 > 2) * p(1 > 3)]
        #                       + [p(2 > 1) * p(2 > 3)]
        #                       + [p(3 > 1) * p(3 > 2)]
        #        den_list     = [[1]] c("1-2", "1-3")
        #                       [[2]] c("2-1", "2-3")
        #                       [[3]] c("3-1", "3-2")
        den_list  <- vector("list", length = length(item_list))
        for (item in seq(length(item_list))) {
          den_list[[item]] <- paste0(item_list[item], "-", item_list[-item])
        } # END for item LOOP

        # probability calculation: (1) multiply elements within each vector in den_list
        #                          (2) sum across all vectors within den_list
        #                          (3) multiply elements in num_list
        #                          (4) divide (3) by (2)
        #                          (5) multiply (4) across all probmaster
        step2 <- matrix(0, nrow = n_person, ncol = 1)
        for (den in seq(length(den_list))){

          # step 1: multiply elements within each vector in den_list
          #         e.g. p(1 > 2) * p(1 > 3) and p(2 > 1) * p(2 > 3) and ...
          step1 <- apply(as.matrix(probability_pair[[block]][ , den_list[[den]]]),
                         MARGIN = 1,
                         FUN = "prod")

          # step 2: sum across all vectors within den_list
          #         e.g. [p(1 > 2) * p(1 > 3)] + [p(2 > 1) * p(2 > 3)] + ...
          step2 <- step2 + step1
        }

        # step 3: multiply elements in num_list
        #         e.g. p(1 > 2) * p(1 > 3)
        step3   <- apply(as.matrix(probability_pair[[block]][ , num_list]),
                         MARGIN = 1,
                         FUN = "prod")

        # step 4: divide (3) by (2)
        step4   <- step3/step2

        # step 5: multiply (4) across all probmaster
        #         e.g. p(1 > 2, 3) * p(2 > 3)
        probability[ , resp] <- probability[ , resp] * step4
      } # END for probmaster LOOP
    } # END for resp LOOP
    probability_list[[block]] <- probability
  } # END for block LOOP
  return(probability_list)
} # END p_thirt FUNCTION

#######################
## Utility Functions ##
#######################

# individual pair-wise probability
p_thirt_one <- function(gamma, lambda1, lambda2,
                        theta1, theta2, psisq1, psisq2) {

  num <- -gamma + lambda1 * theta1 - lambda2 * theta2
  den <- sqrt(psisq1^2 + psisq2^2)

  pnorm(num / den)
} # END p_thirt_one FUNCTION

# split pair name to select first or second item
split_pair <- function(pair, item) {
  return(unlist(strsplit(pair, split = "-"))[item])
} # END split_pair FUNCTION

# join pair name from separate first and second items
join_pair <- function(df, resp, col) {
  return(paste0(df[which(df$id == resp),][col[1]], "-", df[which(df$id == resp),][col[2]]))
}
