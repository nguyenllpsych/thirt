test_that("all probabilities sum up to one", {
   set.seed(202106)

   params <- simulate_thirt_params(n_person = 200,
                                   n_item = c(2, 4),
                                   n_block = 2,
                                   n_dim = 4)

   prob <- do.call(p_thirt, params)

   for(block in seq(length(prob))){
      expect_equal(as.matrix(rowSums(prob[[block]])),
                   matrix(1,
                          nrow = nrow(prob[[block]]),
                          ncol = 1))
   }
})

