#' Calculate probability
#'
#' Calculate the probability of all potential response patterns for each respondent
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
#' @param picked_order a data.frame of length `[person x block]` with four variables:
#'             variable `person` of the format `p` for person number `p`,
#'             variable `block` of the format `b` for block number `b`,
#'             variable `resp` of the format `r` for response order number `r`
#'                      which follows mupp::find_all_permutation orders,
#'             variable `seq` which includes the items in ranked order by each person.
#'             data.frame similar to output in `simulate_thirt_resp()$resp`
#'
#' @return a list of length `[block]` of matrices with dimension `[person X permutation]`
#'         of probabilities for each response pattern per block.
#'
#' @examples
#' \dontrun{
#' set.seed(202106)
#'
#' params <- simulate_thirt_params(n_person = 200,
#'                                 n_item   = 3,
#'                                 n_block  = 2,
#'                                 n_dim    = 3)
#'
#' prob   <- do.call(p_thirt, params)
#'
#' for(block in seq_along(prob)){
#'   print(summary(prob[[block]]))
#' }
#' }
#'
#' @importFrom mupp
#'             find_all_permutations
#'
#' @export
p_thirt <- function(gamma, items, persons, picked_order = NULL) {

  ##################
  ## test designs ##
  ##################

  # number of blocks, dimensions, people, items per block
  n_block  <- length(unique(items$block))
  n_dim    <- length(unique(items$dim))
  n_person <- nrow(persons)
  n_item   <- as.data.frame(table(items$block))[ , 2]

  # item parameters as individual data frames
  lambda   <- data.frame(block  = items$block,
                         lambda = items$lambda)
  psisq    <- data.frame(block  = items$block,
                         psisq  = items$psisq)
  dict     <- data.frame(item   = items$item,
                         block  = items$block,
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
                            init = 1)
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
  })

  ###########################
  ## probability pair-wise ##
  ###########################

  if (is.null(picked_order)) {
    Map(f         = p_thirt_block,
        perm_list = permutation_list,
        n_item    = n_item,
        n_person  = list(n_person),
        gamma     = list(gamma),
        lambda    = list(lambda),
        theta     = list(theta),
        psisq     = list(psisq),
        dict      = list(dict),
        block     = c(seq_len(n_block)))
  } else {
    Map(f         = p_thirt_block,
        perm_list = permutation_list,
        n_item    = n_item,
        n_person  = list(n_person),
        gamma     = list(gamma),
        lambda    = list(lambda),
        theta     = list(theta),
        psisq     = list(psisq),
        dict      = list(dict),
        picked_order = list(picked_order),
        block     = c(seq_len(n_block)))
  }
} # END p_thirt FUNCTION

######################
## Wrapper Function ##
######################

# initial attempt at wrapper / clean p_thirt #
p_thirt_block <- function(perm_list,
                          n_item,
                          n_person,
                          gamma,
                          lambda,
                          theta,
                          psisq,
                          dict,
                          picked_order = NULL,
                          block){

  # create an empty matrix of size [person X block_size]
  perms               <- tail(perm_list, c(0, -(n_item + 1)))

  prob_pair           <- matrix(nrow = n_person,
                                ncol = ncol(perms))
  colnames(prob_pair) <- colnames(perms)

  # populate matrices with pair-wise probabilities
  for (pair in colnames(prob_pair)) {

    # index everything
    pair1     <- as.numeric(split_pair(pair, 1))
    pair2     <- as.numeric(split_pair(pair, 2))

    gamma_idx <- gamma$pair == pair & gamma$block == block
    items_idx <- lambda$block == block
    dim1      <- dict[dict$item == pair1 & dict$block == block, ]$dim
    dim2      <- dict[dict$item == pair2 & dict$block == block, ]$dim

    # p_thirt_one calculations
    prob_pair[ , pair] <-
      p_thirt_one(gamma   = gamma[gamma_idx, ]$gamma,
                  lambda1 = lambda[items_idx, ][pair1, "lambda"],
                  lambda2 = lambda[items_idx, ][pair2, "lambda"],
                  theta1  = theta[ , dim1],
                  theta2  = theta[ , dim2],
                  psisq1  = psisq[items_idx, ][pair1, "psisq"],
                  psisq2  = psisq[items_idx, ][pair2, "psisq"])
  }

  # add probabilities of reverse response patterns
  # with pair names formatted for easier access
  names <- colnames(prob_pair)
  for (col in seq_len(ncol(prob_pair))) {

    # create and bind new col = 1 - old col
    new_col   <-  1 - prob_pair[ , col]
    prob_pair <- cbind(prob_pair, new_col)

    # pull out old column names, e.g. "1-2"
    old_names <- colnames(prob_pair)[col]

    # correct format for new pair names, e.g. "2-1"
    names     <- append(
      names,
      paste0(

        # 2nd item switched to 1st position
        split_pair(old_names, 2),
        "-",
        split_pair(old_names, 1)
      )
    )

    colnames(prob_pair) <- names
  } # END for col LOOP

  #################################
  ## probability ranked sequence ##
  #################################

  # probability_list is a list of length [n_block]
  #   that includes data frames `probability` of size [n_person X permutation]
  #   for probabilities of different response patterns per block
  probability <- matrix(1,
                        nrow = n_person,
                        ncol = nrow(perm_list))

  # rename variables in probability matrix to reflect block/resp
  # format: "b", [block number], "r", [response number]
  # e.g.  : b1r1 = 1st response pattern in 1st block
  # block/resp keys are in permutation_list
  colnames(probability) <- c(perm_list$id)

  ##################################
  ## for ALL permutations         ##
  ## if no picked_order provided  ##
  ##################################

  if(is.null(picked_order)) {

    # probability for each permutation per block
    # iterate through response patterns per block
    for (resp in colnames(probability)) {

      # iterate through number of separate probabilities to joint multiply
      # moving from the end/smaller probability first
      # e.g. 2 master probabilities for 3 items: p(1 > 2, 3) * p(2 > 3)
      for (probmaster in seq_len(n_item - 1)) {
        # iterate through number of probabilities in the numerator
        # num_list is a vector of pairwise probability names
        # e.g. for master probability p(1 > 2, 3):
        #      numerator = p(1 > 2) * p(1 > 3)
        #      num_list  = c("1-2", "1-3")
        num_list <- c()

        for (numlen in seq_len(probmaster)) {
          num      <-  join_pair(df   = perm_list,
                                 resp = resp,
                                 col = c(n_item - probmaster,
                                         n_item - (probmaster - 1 * numlen)))
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
          step1 <- apply(as.matrix(prob_pair[ , den_list[[den]], drop = FALSE]),
                         MARGIN = 1,
                         FUN = "prod")

          # step 2: sum across all vectors within den_list
          #         e.g. [p(1 > 2) * p(1 > 3)] + [p(2 > 1) * p(2 > 3)] + ...
          step2 <- step2 + step1
        }

        # step 3: multiply elements in num_list
        #         e.g. p(1 > 2) * p(1 > 3)
        step3   <- apply(as.matrix(prob_pair[ , num_list, drop = FALSE]),
                         MARGIN = 1,
                         FUN = "prod")

        # step 4: divide (3) by (2)
        step4   <- step3 / step2

        # step 5: multiply (4) across all probmaster
        #         e.g. p(1 > 2, 3) * p(2 > 3)
        probability[ , resp] <- probability[ , resp] * step4
      }  # END for probmaster LOOP
    } # END for resp LOOP

    # convert names of response patterns to numeric
    # compatible with mupp::find_permutation_order()
    colnames(probability) <- seq_len(ncol(probability))

  } else {

    ##############################
    ## for ONLY selected resp   ##
    ## if picked_order provided ##
    ##############################

    # probability is a matrix of dimension [person x 1]
    probability <- matrix(1,
                          nrow = n_person,
                          ncol = 1)

    # picked_order for current block
    resp <- picked_order[which(picked_order$block == block),]
    resp <- matrix(
      paste0("b", resp$block, "r", resp$resp),
      nrow = n_person)

    # iterate through number of separate probabilities to joint multiply
    # moving from the end/smaller probability first
    # e.g. 2 master probabilities for 3 items: p(1 > 2, 3) * p(2 > 3)
    for (probmaster in seq_len(n_item - 1)) {

      # iterate through number of probabilities in the numerator
      # num_list is a vector of pairwise probability names
      # e.g. for master probability p(1 > 2, 3):
      #      numerator = p(1 > 2) * p(1 > 3)
      #      num_list  = c("1-2", "1-3")
      num_list <- c()

      for (numlen in seq_len(probmaster)) {
        num      <-  apply(X = resp,
                           MARGIN = 1,
                           FUN = function(x){
                             join_pair(df   = perm_list,
                                       resp = x,
                                       col = c(n_item - probmaster,
                                               n_item - (probmaster - 1 * numlen)))
                           })
        num_list <- append(num_list, num)
      } # END for numlen LOOP

      # num_list to matrix, each row is numerator list for each person
      num_list <- matrix(num_list, nrow = n_person, byrow = FALSE)

      # create a list of all items present in the numerators
      # each column is one person
      item_list <- t(apply(X = num_list, MARGIN = 1, FUN = function(x){
        unique(as.numeric(split_pair(x)))
      }))

      # create a list of all denominators that correspond to a numerator
      #   each element in den_list is a matrix with n_row = n_person
      #   e.g. probmaster   = p(1 > 2, 3)
      #        numerator    = p(1 > 2) * p(1 > 3)
      #        denominator  = [p(1 > 2) * p(1 > 3)]
      #                       + [p(2 > 1) * p(2 > 3)]
      #                       + [p(3 > 1) * p(3 > 2)]
      #        den_list     = [[1]] c("1-2", "1-3")
      #                       [[2]] c("2-1", "2-3")
      #                       [[3]] c("3-1", "3-2")
      den_list  <- vector("list", length = ncol(item_list))
      for (item in seq(length(den_list))) {
        den_list[[item]] <- matrix(apply(X = item_list,
                                         MARGIN = 1,
                                         FUN = function(x) {
                                           paste0(x[item], "-", x[-item])
                                         }),
                                   nrow = n_person,
                                   byrow = T)
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

        # logical matrix to select different den_list per person
        logmat <- apply(X = den_list[[den]],
                        MARGIN = 1,
                        FUN = function(x) colnames(prob_pair) %in% x)
        pickedmat <- t(logmat)*log(prob_pair)
        step1 <- exp(rowSums(pickedmat))
        step1 <- matrix(step1, ncol = 1, byrow = T)

        # step 2: sum across all vectors within den_list
        #         e.g. [p(1 > 2) * p(1 > 3)] + [p(2 > 1) * p(2 > 3)] + ...
        step2 <- step2 + step1
      }

      # step 3: multiply elements in num_list
      #         e.g. p(1 > 2) * p(1 > 3)

      # logical matrix to select different num_list per person
      logmat <- apply(X = num_list,
                      MARGIN = 1,
                      FUN = function(x) colnames(prob_pair) %in% x)
      pickedmat <- t(logmat)*log(prob_pair)
      step3   <- exp(rowSums(pickedmat))
      step3 <- matrix(step3, nrow = n_person, byrow = T)

      # step 4: divide (3) by (2)
      step4   <- step3 / step2

      # step 5: multiply (4) across all probmaster
      #         e.g. p(1 > 2, 3) * p(2 > 3)
      probability <- probability * step4
    }  # END for probmaster LOOP
  } # END if picked_order else STATEMENT

  return(probability)
}

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

#' Calculate probability
#'
#' Calculate the probability of all potential response patterns for each respondent using C++
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
#' @param picked_order a data.frame of length `[person x block]` with four variables:
#'             variable `person` of the format `p` for person number `p`,
#'             variable `block` of the format `b` for block number `b`,
#'             variable `resp` of the format `r` for response order number `r`
#'                      which follows mupp::find_all_permutation orders,
#'             variable `seq` which includes the items in ranked order by each person.
#'             data.frame similar to output in `simulate_thirt_resp()$resp`
#'
#' @return a list of length `[block]` of matrices with dimension `[person X permutation]`
#'         of probabilities for each response pattern per block.
#' @export
p_thirtC <- function(gamma, items, persons, picked_order = NULL) {

  ##################
  ## test designs ##
  ##################

  # number of blocks, dimensions, people, items per block
  n_block  <- length(unique(items$block))
  n_dim    <- length(unique(items$dim))
  n_person <- nrow(persons)
  n_item   <- as.data.frame(table(items$block))[ , 2]

  # item parameters as individual data frames
  lambda   <- data.frame(block  = items$block,
                         lambda = items$lambda)
  psisq    <- data.frame(block  = items$block,
                         psisq  = items$psisq)
  dict     <- data.frame(item   = items$item,
                         block  = items$block,
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
                            init = 1)
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
    })

  #############################
  ## probability calculation ##
  #############################

  probability <- vector(mode = "list", length = n_block)

  # probability matrix of dim [person X block_size]

  for(block in seq_len(n_block)) {
    perm_list            <- permutation_list[[block]]

    if (is.null(picked_order)) {
      probability[[block]] <- p_thirt_blockC(n_person = n_person,
                                             n_dim = n_dim,
                                             block = block,
                                             n_perm = nrow(perm_list),
                                             n_item = n_item[block],
                                             pair_names = colnames(tail(perm_list, c(0, -(n_item[block] + 1)))),
                                             perm_id = perm_list$id,
                                             perm_item_vector = as.vector(t(t(perm_list[, 1:n_item[block]]))),
                                             params_gamma = gamma[which(gamma$block == block), ]$gamma,
                                             params_lambda = lambda[which(lambda$block == block), ]$lambda,
                                             params_psisq = psisq[which(psisq$block == block), ]$psisq,
                                             vector_theta = as.vector(t(t(theta))),
                                             dict_item = dict[which(dict$block == block), ]$item,
                                             dict_dim = dict[which(dict$block == block), ]$dim,
                                             picked_order = c(NA))
    } else {
      probability[[block]] <- p_thirt_blockC(n_person = n_person,
                                             n_dim = n_dim,
                                             block = block,
                                             n_perm = nrow(perm_list),
                                             n_item = n_item[block],
                                             pair_names = colnames(tail(perm_list, c(0, -(n_item[block] + 1)))),
                                             perm_id = perm_list$id,
                                             perm_item_vector = as.vector(t(t(perm_list[, 1:n_item[block]]))),
                                             params_gamma = gamma[which(gamma$block == block), ]$gamma,
                                             params_lambda = lambda[which(lambda$block == block), ]$lambda,
                                             params_psisq = psisq[which(psisq$block == block), ]$psisq,
                                             vector_theta = as.vector(t(t(theta))),
                                             dict_item = dict[which(dict$block == block), ]$item,
                                             dict_dim = dict[which(dict$block == block), ]$dim,
                                             picked_order = picked_order[which(picked_order$block == block),]$resp)
    } # END if else STATEMENT
  } # END for block LOOP

  return(probability)
} # END p_thirtC FUNCTION
