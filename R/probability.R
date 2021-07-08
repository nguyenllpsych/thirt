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
  lambda   <- data.frame(lambda = item_params$lambda)
  psisq    <- data.frame(psisq  = item_params$psisq)
  dict     <- data.frame(item   = seq(nrow(item_params)),
                         dim    = item_params$dim)

  # person parameters with only thetas
  theta    <- person_params[,-1]

  ######################
  ## all permutations ##
  ######################

  # create an empty list of size n_block
  permutation_list <- vector("list", n_block)

  # add each block permutation to the list
  for (block in seq_len(n_block)) {

    # create permutation list for each block (what if blocksize changes?)
    permutation_block <- as.data.frame(
      find_all_permutations(n    = n_item,
                            init = 1 + (block - 1) * n_item)
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
      combo <- combn(x = seq(min(x$V1), max(x$V1)),
                     m = 2)
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
    probability <- as.data.frame(
      matrix(1,
             nrow = n_person,
             ncol = nrow(permutation_list[[block]]))
    )

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

        # indicating response
        u_one      <- permutation_list[[block]][resp, pair]

        # joint multiply all pair-wise probabilities
        probability[ , resp] <- {
          probability[ , resp] * ((p_one ^ u_one) * (1 - p_one) ^ (1 - u_one))
        }
      } # END for pair LOOP
    } # END for resp LOOP

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
  prob = pnorm((-gamma + lambda1 * theta1 - lambda2 * theta2)/
                 sqrt(psisq1^2 + psisq2^2))
  return(prob)
} # END p_thirt_one FUNCTION

# split pair name to select first or second item
split_pair <- function(pair, item) {
  return(unlist(strsplit(pair, split = "-"))[item])
} # END split_pair FUNCTION
