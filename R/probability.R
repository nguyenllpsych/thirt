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
#' @return a list of length `[block]` of data frames with dimension `[person X permutation]`
#'         of probability for each response pattern per block
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
#' prob <- p_thirt(params$gamma, params$items, params$persons)
#'
#' summary(prob)
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
  n_item   <- as.data.frame(table(items$block))[,2]

  # item parameters as individual data frames
  lambda   <- data.frame(lambda = items$lambda)
  psisq    <- data.frame(psisq  = items$psisq)
  dict     <- data.frame(item   = items$item,
                         dim    = items$dim)

  # person parameters with only thetas
  theta    <- persons[, -1]

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

  #################
  ## probability ##
  #################

  # create an empty list of size n_block
  probability_list <- vector("list", n_block)

  # probability_list is a [person X (permutation X block)] data frame
  for (block in seq(n_block)) {
    probability <- matrix(1,
                          nrow = n_person,
                          ncol = nrow(permutation_list[[block]]))

    # joint probability for each permutation per block
    for (resp in seq(ncol(probability))) {
      for (pair in colnames(tail(permutation_list[[block]], c(0, -(n_item + 1))))){

        # index everything
        pair1     <- split_pair(pair, 1)
        pair2     <- split_pair(pair, 2)

        gamma_idx <- gamma$pair == pair
        dim1      <- dict[dict$item == pair1, ]$dim
        dim2      <- dict[dict$item == pair2, ]$dim

        # p_thirt_one calculations
        p_one     <- p_thirt_one(gamma   = gamma[gamma_idx, ]$gamma,
                                 lambda1 = lambda[pair1, ],
                                 lambda2 = lambda[pair2, ],
                                 theta1  = theta[ , dim1],
                                 theta2  = theta[ , dim2],
                                 psisq1  = psisq[pair1, ],
                                 psisq2  = psisq[pair2, ])

        # note: I don't think this is the right way of thinking about this,
        #       as it only sums up to 1 if you have 2 ^ n combinations rather
        #       than n!. but n! is right, so the probabilities aren't combined
        #       correctly.
        #
        # What we want:
        # P(x_1 > x_2 > x_3) = P(x_1 > x_2, x_3) * P(x_2 > x_3)
        # where
        # P(x_1 > x_2, x_3)  = [P(x_1 > x_2) * P(x_1 > x_3)] / [P(x_1 > x_2) * P(x_1 > x_3) + P(x_2 > x_1) * P(x_2 > x_3) + P(x_3 > x_1) * P(x_3 > x_2)]

        # indicating response
        u_one      <- permutation_list[[block]][resp, pair]

        # joint multiply all pair-wise probabilities
        probability[ , resp] <- {
          probability[ , resp] * ((p_one ^ u_one) * (1 - p_one) ^ (1 - u_one))
        }
      } # END for pair LOOP
    } # END for resp LOOP

    # is this the right way to do this?
    probability <- diag(1 / rowSums(probability)) %*% probability

    # rename variables in probability matrix to reflect block/resp
    # format: "b", [block number], "r", [response number]
    # e.g.  : b1r1 = 1st response pattern in 1st block
    # block/resp keys are in permutation_list[[block]]
    names(probability) <- c(permutation_list[[block]]$id)

    # append each block's probability matrix to large probability_list
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
