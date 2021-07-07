#' Quicker Combinations IF Choosing 2
#'
#' Calculate quicker combinations and choose output if only choosing 2
#'
#' @param n integer indicating the total number of cases from which to choose 2
#'
#' @note To make these functions fast, they do not check their arguments. Please
#'       use these functions with caution, as incorrect arguments might have
#'       unpredictable responses.
#'
#' @name choose2
NULL

#' @rdname choose2
#' @export
choose2 <- function(n = 2){
  n * (n - 1) / 2
}

#' @rdname choose2
#' @export
combn2 <- function(n = 2){

  # determine size of matrix
  n_r <- choose2(n)
  n_c <- 2

  # create matrix and list
  m   <- matrix(nrow = n_r,
                ncol = n_c)
  x   <- seq_len(n)

  # TODO: put everything into matrix
}
