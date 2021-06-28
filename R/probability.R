set.seed(202106)

#fixed parameters
n_item    = 4
n_block   = 2
n_dim     = 4
n_person  = 200
block_size <- n_item * (n_item - 1) / 2

gamma   <- round(runif(n = block_size * n_block, min = -1,  max = 1),   digits = 3)  #thresholds
lambda  <- round(runif(n = n_item * n_block,     min = .65, max = .95), digits = 3)  #loadings
psisq   <- round(1 - lambda^2, digits = 3)                                           #uniqueness

#theta
theta   <- as.data.frame(matrix(nrow = n_person, ncol = n_dim))
for (dim in 1:n_dim) {
  theta[dim] <- round(rnorm(n = n_person, mean = 0, sd = 1), digits = 3)
} # END for dim LOOP

#data in wide format with all pairs per row
quads <- as.data.frame(
  matrix(rbinom(n = n_person * block_size * n_block, size = 1, prob = 0.5),
         nrow = n_person))
colnames(quads) <- paste0("i", c(1,1,1,2,2,3,
                                 5,5,5,6,6,7),
                          "i", c(2,3,4,3,4,4,
                                 6,7,8,7,8,8))

#data in long format with one pair per row
quads_long <- data.frame(
  id    = rep(1:n_person, each = block_size * n_block),
  block = rep(1:n_block, each = block_size, times = n_person),
  item1 = rep(c(1,1,1,2,2,3,5,5,5,6,6,7), times = n_person),
  item2 = rep(c(2,3,4,3,4,4,6,7,8,7,8,8), times = n_person),
  resp  = rbinom(n = n_person * block_size * n_block, size = 1, prob = 0.5))

#merge gamma, lambda, theta to data frame
quads_long <- quads_long %>%
  mutate(gamma   = rep(gamma, times = n_person),
         lambda1 = lambda[item1],
         lambda2 = lambda[item2],
         psisq1  = psisq[item1],
         psisq2  = psisq[item2])
for (row in 1:nrow(quads_long)){
  quads_long[row,'theta1'] = theta[dict[which(dict$item == quads_long[row,]$item1),]$dim][quads_long[row,]$id,]
  quads_long[row,'theta2'] = theta[dict[which(dict$item == quads_long[row,]$item2),]$dim][quads_long[row,]$id,]
}

data <- quads_long

#create a dictionary with a "dim" column identifying traits
#   and an "item" column identifying item indicators for different traits
dict <- data.frame(
  dim = c(1:n_dim),
  item = c(1:(n_item * n_block)))

###############################
## Function Def: Probability ##
###############################

#' Title
#'
#' @param data
#' @param n_item
#' @param n_block
#' @param dict
#' @param gamma
#' @param lambda
#' @param theta
#' @param psisq
#'
#' @importFrom dplyr
#'
#' @return
#' @export
#'
#' @examples
calc_probability <- function(data, n_item, n_block, dict,
                             gamma, lambda, theta, psisq){

  #calculate block size
  block_size <- n_item * (n_item - 1) / 2

  ######################
  ## all permutations ##
  ######################

  #find all permutations for one block
  permutation_list <- as.data.frame(mupp::find_all_permutations(n = n_item, init = 1))

  #################
  ## probability ##
  #################

  #individual pair-wise probability
  data <- data %>%
    mutate(prob = pnorm((-gamma + lambda1 * theta1 - lambda2 * theta2)/
                          sqrt(psisq1^2 + psisq2^2)))

  #generate all pairwise combinations
  combo <- t(combn(n_item, 2))
  permutation_list








} # END calc_probability FUNCTION
