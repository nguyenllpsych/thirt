set.seed(202106)

#####################
## Test Conditions ##
#####################

n_item    = 4
n_block   = 1
n_dim     = 4
n_person  = 200
block_size <- n_item * (n_item - 1) / 2

################
## Parameters ##
################

#thresholds - gamma
#vector of length [total binary outcomes]
#row names should be of the format "i-j" for item pair ij
gamma   <- data.frame(gamma = round(runif(n = block_size * n_block, min = -1,  max = 1), digits = 3))
row.names(gamma) <- c("1-2", "1-3", "1-4", "2-3", "2-4", "3-4")

#loadings - lambda
#vector of length [total items]
#row names should be of the format "i" for item i
lambda  <- data.frame(lambda = round(runif(n = n_item * n_block, min = .65, max = .95), digits = 3))

#uniqueness - psi^2
#vector of length [total items]
#row names should be of the format "i" for item i
psisq   <- data.frame(psisq = round(1 - lambda^2, digits = 3))

#theta - trait scores
#matrix of dimension [number of people X number of dimensions]
#row names should be of the format "p" for person p
theta   <- as.data.frame(matrix(nrow = n_person, ncol = n_dim))
for (dim in 1:n_dim) {
  theta[dim] <- round(rnorm(n = n_person, mean = 0, sd = 1), digits = 3)
} # END for dim LOOP

#create a dictionary with a "dim" column identifying traits
#   and an "item" column identifying item indicators for different traits
dict <- data.frame(
  dim = c(1:n_dim),
  item = c(1:(n_item * n_block)))

###############
## Test Data ##
###############

#data in wide format with all pairs per row
quads <- as.data.frame(
  matrix(rbinom(n = n_person * block_size * n_block, size = 1, prob = 0.5),
         nrow = n_person))
colnames(quads) <- paste0("i", c(1,1,1,2,2,3),
                          "i", c(2,3,4,3,4,4))

#data in long format with one pair per row
quads_long <- data.frame(
  id    = rep(1:n_person, each = block_size * n_block),
  block = rep(1:n_block, each = block_size, times = n_person),
  item1 = rep(c(1,1,1,2,2,3), times = n_person),
  item2 = rep(c(2,3,4,3,4,4), times = n_person),
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
} #END for row LOOP

data <- quads_long


###############################
## Function Def: Probability ##
###############################

p_thirt <- function(n_item, n_block, n_person,
                    gamma, lambda, theta, psisq) {

  #calculate block size
  block_size <- n_item * (n_item - 1) / 2

  ######################
  ## all permutations ##
  ######################

  #find all permutations for one block
  permutation_list <- as.data.frame(mupp::find_all_permutations(n = n_item, init = 1))

  #find all pair combinations
  combo <- t(combn(n_item, 2))

  #find all pair combinations specific to each permutation
  for (rowcombo in 1:nrow(combo)) {
    for (rowpermu in 1:nrow(permutation_list)) {

      #add a new variable in permutation_list for each pair in combo
      permutation_list[rowpermu,paste0(combo[rowcombo,][1], "-", combo[rowcombo,][2])] =

        #if item i in pair ij is ranked higher than item j (with lower grep value)
        #   -> variable i-j has value 1, else 0
        ifelse(grep(paste0(combo[rowcombo,][1]), permutation_list[rowpermu,1:n_item])
                  < grep(paste0(combo[rowcombo,][2]), permutation_list[rowpermu,1:n_item]),
               1, 0)
    } #END for rowpermu LOOP
  } #END for rowcombo LOOP

  #################
  ## probability ##
  #################

  #individual pair-wise probability
  p_thirt_one <- function(gamma, lambda1, lambda2, theta1, theta2, psisq1, psisq2) {
    prob = pnorm((-gamma + lambda1 * theta1 - lambda2 * theta2)/
                   sqrt(psisq1^2 + psisq2^2))
    return(prob)
  } #END p_thirt_one FUNCTION

  #joint probability for each permutation
  probability <- as.data.frame(matrix(1,
                                      nrow = n_person,
                                      ncol = nrow(permutation_list)))
  for (person in 1:nrow(probability)) {
    for (resp in 1:ncol(probability)) {
      for (pair in colnames(permutation_list[(n_item + 1):ncol(permutation_list)])){
        probability[person,resp] = ifelse(
          #p_thirt_one if pair == 1
          permutation_list[resp,pair] == 1,
            probability[person,resp] * p_thirt_one(gamma   = gamma[paste0(pair),],
                                                   lambda1 = lambda[paste0(unlist(strsplit(pair, split = "-"))[1]),],
                                                   lambda2 = lambda[paste0(unlist(strsplit(pair, split = "-"))[2]),],
                                                   theta1  = theta[#select row in theta for the person
                                                                   person,
                                                                   #select column in theta for the correct trait/dim
                                                                   dict[#select the trait/dim from the dictionary for the item
                                                                        which(dict$item == paste0(unlist(strsplit(pair, split = "-"))[1])),]$dim],
                                                   theta2  = theta[#select row in theta for the person
                                                                   person,
                                                                   #select column in theta for the correct trait/dim
                                                                   dict[#select the trait/dim from the dictionary for the item
                                                                        which(dict$item == paste0(unlist(strsplit(pair, split = "-"))[2])),]$dim],
                                                   psisq1  = psisq[paste0(unlist(strsplit(pair, split = "-"))[1]),],
                                                   psisq2  = psisq[paste0(unlist(strsplit(pair, split = "-"))[1]),]),
            #p_thirt_one if pair == 0
            probability[person,resp] * (1 -
                                        p_thirt_one(gamma   = gamma[paste0(pair),],
                                                    lambda1 = lambda[paste0(unlist(strsplit(pair, split = "-"))[1]),],
                                                    lambda2 = lambda[paste0(unlist(strsplit(pair, split = "-"))[2]),],
                                                    theta1  = theta[#select row in theta for the person
                                                                    person,
                                                                    #select column in theta for the correct trait/dim
                                                                    dict[#select the trait/dim from the dictionary for the item
                                                                         which(dict$item == paste0(unlist(strsplit(pair, split = "-"))[1])),]$dim],
                                                    theta2  = theta[#select row in theta for the person
                                                                    person,
                                                                    #select column in theta for the correct trait/dim
                                                                    dict[#select the trait/dim from the dictionary for the item
                                                                         which(dict$item == paste0(unlist(strsplit(pair, split = "-"))[2])),]$dim],
                                                    psisq1  = psisq[paste0(unlist(strsplit(pair, split = "-"))[1]),],
                                                    psisq2  = psisq[paste0(unlist(strsplit(pair, split = "-"))[1]),])))
      } #END for pair LOOP
    } #END for resp LOOP
  } #END for person LOOP

  #return list of permutation and probability
  #permutation_list: a matrix of dimension [number of permutations X number of items]
  #probability: a matrix of dimension [person X permutation] of probability for each response
  return_list <- list("permutation_list" = permutation_list[,1:n_item],
                      "probability"      = probability)
  return(return_list)

  #TO DO: multiple blocks
} # END p_thirt FUNCTION


###################
## Test Function ##
###################

#test function with fixed parameters
prob <- p_thirt(n_item, n_block, n_person,
                gamma, lambda, theta, psisq)

prob$permutation_list
summary(prob$probability)
