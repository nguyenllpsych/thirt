#fixed parameters
n_item    = 4
n_block   = 3
n_dim     = 4
n_person  = 200
block_size <- n_item * (n_item - 1) / 2

gamma   <- runif(n = block_size * n_block, min = -1,  max = 1)    #thresholds
lambda  <- runif(n = n_item * n_block,     min = .65, max = .95)  #loadings
psisq   <- 1 - lambda^2                                           #uniqueness

#theta
theta   <- as.data.frame(matrix(nrow = n_person, ncol = n_dim))
for (dim in 1:n_dim) {
  theta[dim] <- round(rnorm(n = n_person, mean = 0, sd = 1), digits = 3)
} # END for dim LOOP

quads <- as.data.frame(
  matrix(rbinom(n = n_person * block_size * n_block, size = 1, prob = 0.5),
         nrow = n_person))
colnames(quads) <- paste0("i", c(1,1,1,2,2,3,
                                 5,5,5,6,6,7,
                                 9,9,9,10,10,11),
                          "i", c(2,3,4,3,4,4,
                                 6,7,8,7,8,8,
                                 10,11,12,11,12,12))
data <- quads

#create a dictionary with a "dim" column identifying traits
#   and an "item" column identifying item indicators for different traits
dict <- data.frame(
  dim = c("A","B","C","D"),
  item = c(1:12))

###############################
## Function Def: Probability ##
###############################

calc_probability <- function(data, n_item, n_block, dict){

  #calculate block size
  block_size <- n_item * (n_item - 1) / 2

  #check data format
  if (ncol(data) != block_size * n_block){
    stop("Wrong data format",
         call. FALSE)
  } # END if STATEMENT

  ######################
  ## all permutations ##
  ######################

  #empty permutation list object with correct nrow
  permutation_list <- as.data.frame(matrix(nrow = nrow(mupp::find_all_permutations(n = n_item, init = 1))))

  #find all permutations per block then join to one data frame
  for (block in 1:n_block){
    permutation_list[(1 + (block - 1) * n_item):(n_item * block)] <- as.data.frame(mupp::find_all_permutations(n    = n_item,
                                                                                                               init = 1 + (block - 1) * n_item))
  } # END for block LOOP

  #TO DO: different block combinations

  #################
  ## probability ##
  #################

  #individual pair-wise probability
  icf <- data.frame(pair = colnames(data))
  for pair in icf)



  #TO DO: generate combo with combn(n, k)








} # END calc_probability FUNCTION
