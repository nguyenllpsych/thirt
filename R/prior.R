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
} #END d_thetas_prior FUNCTION

#' @rdname prior
#' @export
d_lambda_prior <- function(x, ...) {
  dtruncnorm(x, a = -1, b = 1, ...)
} #END d_lambda_prior FUNCTION

#' @rdname prior
#' @export
d_psisq_prior <- function(x, ...) {
  dtruncnorm(x, a = 0, b = 1, ...)
} #END d_psisq_prior FUNCTION

#' @rdname prior
#' @export
d_gamma_prior <- function(x, ...) {
  dtruncnorm(x, a = -1, b = 1, ...)
} #END d_gamma_prior FUNCTION


#' @rdname prior
#' @export
r_thetas_prior <- function(n, ...) {
  rnorm(n, ...)
} #END r_thetas_prior FUNCTION

#' @rdname prior
#' @export
r_lambda_prior <- function(n, ...) {
  rtruncnorm(n, a = -1, b = 1, ...)
} #END r_lambda_prior FUNCTION

#' @rdname prior
#' @export
r_psisq_prior <- function(n, ...) {
  rtruncnorm(n, a = 0, b = 1, ...)
} #END r_psisq_prior FUNCTION

#' @rdname prior
#' @export
r_gamma_prior <- function(n, ...) {
  rtruncnorm(n, a = -1, b = 1, ...)
} #END r_gamma_prior FUNCTION
