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

  if(length(n) > 1){
    x <- n
    n <- length(x)
  } else{
    x <- seq_len(n)
  }

  # put everything into an example matrix
  cx   <- matrix(data = x,
                 nrow = n,
                 ncol = n)
  rx   <- t(cx)

  # pull out the lower triangle flag
  flag <- lower.tri(rx)

  # pull out the row/col number
  v1  <- rx[flag]
  v2  <- cx[flag]

  # put together
  rbind(v1, v2)
}
