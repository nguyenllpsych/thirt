test_that("p_thirt() returns correct elements", {
   set.seed(202106)

   n_item    <- 3
   n_block   <- 2
   n_dim     <- 3
   n_person  <- 200
   block_size <- n_item * (n_item - 1) / 2

   gamma   <- data.frame(gamma = round(runif(n = block_size * n_block, min = -1,  max = 1), digits = 3))
   row.names(gamma) <- c("1-2", "1-3", "2-3", "4-5", "4-6", "5-6")
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

   probability_means <- as.data.frame(matrix(c(0.034, 0.035, 0.041, 0.026, 0.012, 0.038,
                                               0.002, 0.024, 0.001, 0.002, 0.025, 0.014,
                                               0,     0,     0.022, 0.026, 0,     0.008,
                                               0,     0,     0.01,  0.084, 0.001, 0.115,
                                               0,     0.007, 0.001, 0.006, 0.063, 0.147,
                                               0,     0,     0,     0.004, 0,     0.036),
                                             nrow = 1))

   expect_identical(probability_means, round(as.data.frame(lapply(prob, mean)), digits = 3))
})
