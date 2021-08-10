#' Prior distributions
#'
#' @param x vector of quantiles
#' @param n number of observations for the simulation
#' @param ... additional arguments
#'
#' @importFrom truncnorm
#'             dtruncnorm rtruncnorm
#'
#' @name prior
NULL

#' @rdname prior
#' @export
d_thetas_prior <- function(x, ...) {
  dnorm(x, ...)
}

#' @rdname prior
#' @export
d_lambda_prior <- function(x, direction = 1, ...) {
  dtruncnorm(direction * x, a = 0, b = 1.2, ...)
}

#' @rdname prior
#' @export
d_psisq_prior <- function(x, ...) {
  dtruncnorm(x, a = 0, b = 1, ...)
}

#' @rdname prior
#' @export
d_gamma_prior <- function(x, ...) {
  dtruncnorm(x, a = -1, b = 1, ...)
}


#' @rdname prior
#' @export
r_thetas_prior <- function(n, ...) {
  rnorm(n, ...)
}

#' @rdname prior
#' @export
r_lambda_prior <- function(n, ...) {
  rtruncnorm(n, a = 0, b = 1.2, ...)
}

#' @rdname prior
#' @export
r_psisq_prior <- function(n, ...) {
  rtruncnorm(n, a = 0, b = 1, ...)
}

#' @rdname prior
#' @export
r_gamma_prior <- function(n, ...) {
  rtruncnorm(n, a = -1, b = 1, ...)
}
