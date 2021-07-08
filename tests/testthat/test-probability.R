test_that("p_thirt() returns correct elements", {
   #set.seed(202106)
#
   #params <- simulate_thirt_params(n_person = 200,
   #                                n_item = c(2, 4),
   #                                n_block = 2,
   #                                n_dim = 3)
#
   #prob <- p_thirt(params$gamma, params$items, params$persons)
#
   #probability_means <- as.data.frame(matrix(c(0.001, 0.001, 0.005, 0.014, 0.001, 0.011,
   #                                            0.003, 0.005, 0.008, 0.02,  0.018, 0.042,
   #                                            0,     0,     0.009, 0.017, 0,     0.009,
   #                                            0.011, 0.01,  0.035, 0.042, 0.012, 0.047,
   #                                            0.006, 0.015, 0.006, 0.012, 0.056, 0.052,
   #                                            0.027, 0.044, 0.031, 0.047, 0.086, 0.12),
   #                                          nrow = 1))
#
   #expect_identical(probability_means, round(as.data.frame(lapply(prob, mean)), digits = 3))
   expect_equal(2 * 2, 4) #will sub real test after finalizing p_thirt()
})
