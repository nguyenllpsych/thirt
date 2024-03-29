% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation.R
\name{count_accept}
\alias{count_accept}
\title{Calculate acceptance rate of MCMC chain}
\usage{
count_accept(all_iters, resp)
}
\arguments{
\item{all_iters}{the output object from estimate_thirt_params_mcmc function}

\item{resp}{a data.frame of length \verb{[n_person x n_block]} with at least three first variables:
variable \code{person} of the format \code{p} for person number \code{p},
variable \code{block} of the format \code{b} for block number \code{b},
variable \code{resp} of the format \code{r} for response number \code{r}
which corresponds to mupp::find_permutation_index().}
}
\description{
Calculate acceptance rate of MCMC chain
}
