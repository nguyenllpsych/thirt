test_that("p_thirt() returns correct elements", {
   set.seed(202106)

   n_item    = 4
   n_block   = 1
   n_dim     = 4
   n_person  = 200
   block_size <- n_item * (n_item - 1) / 2

   gamma   <- data.frame(gamma = round(runif(n = block_size * n_block, min = -1,  max = 1), digits = 3))
   row.names(gamma) <- c("1-2", "1-3", "1-4", "2-3", "2-4", "3-4")
   lambda  <- data.frame(lambda = round(runif(n = n_item * n_block, min = .65, max = .95), digits = 3))
   psisq   <- data.frame(psisq = round(1 - lambda^2, digits = 3))
   theta   <- as.data.frame(matrix(nrow = n_person, ncol = n_dim))
   for (dim in 1:n_dim) {
     theta[dim] <- round(rnorm(n = n_person, mean = 0, sd = 1), digits = 3)
   }

   dict <- data.frame(
     dim = c(1:n_dim),
     item = c(1:(n_item * n_block)))

   prob <- p_thirt(n_item, n_block, n_person, dict,
                   gamma, lambda, theta, psisq)

   permutation_list <- as.data.frame(matrix(
     mupp::find_all_permutations(n_item, 1),
     nrow = 24))

   probability_means <- as.data.frame(matrix(c(0.021, 0.004, 0.004, 0.009, 0.016, 0.004,
                                               0.018, 0.01, 0.058, 0.079, 0.017, 0.034,
                                               0.009, 0.024, 0.032, 0.026, 0.028, 0.093,
                                               0.018, 0.006, 0.042, 0.117, 0.01, 0.034),
                                             nrow = 1))

   expect_identical(probability_means, round(as.data.frame(lapply(prob$probability, mean)), digits = 3))
   expect_identical(permutation_list, prob$permutation_list)
})
