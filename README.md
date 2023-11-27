
<!-- README.md is generated from README.Rmd. Please edit that file -->

# thirt

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/nguyenllpsych/thirt/branch/main/graph/badge.svg)](https://codecov.io/gh/nguyenllpsych/thirt?branch=main)
[![R-CMD-check](https://github.com/nguyenllpsych/thirt/workflows/R-CMD-check/badge.svg)](https://github.com/nguyenllpsych/thirt/actions)

<!-- badges: end -->

### An R Packages for the Thurstonian Item Response Theory model

The `thirt` package contains functions designed to simulation data that
for forced-choice questionnaires under the Thurstonian Item Response
Theory model and to estimate person- and item-parameters using an MCMC
algorithm.  

The package homepage is currently hosted
[here](https://nguyenllpsych.github.io/thirt/index.html).

## Installation

The package is hosted on
[GitHub](https://www.github.com/nguyenllpsych/thirt) and the latest
development version can be installed by running this code within an R
session:

``` r
devtools::install_github("nguyenllpsych/thirt")
```

## Help

Take a look at the help pages for `simulate_thirt_params`,
`simulate_thirt_resp`, and `estimate_thirt_params_mcmc` for more
information.

## Example

### Parameter and Response Simulation

We can first simulate the person-parameter $\theta$ and item-parameters
$\lambda, \gamma, \psi^2$ given a specific test design. The test
conditions that can be adjusted are: the number of respondents, the
number of items per question block, the number of negatively keyed items
per block (i.e., items with negative loadings $\lambda$), the number of
question blocks, and the number of dimensions or traits that are being
measured.

``` r
# load the package
library(thirt)

# set seed for reproducibility
set.seed(2022)
options(digits = 2)

# specify test conditions
params <- simulate_thirt_params(
  n_person = 2,  # number of respondents
  n_item   = 3,  # number of items per block
  n_neg    = 1,  # number of negatively keyed items per block
  n_block  = 2,  # number of question blocks
  n_dim    = 3)  # number of dimensions or traits being measured
```

Due to the differing structures across parameters, the
`simulate_thirt_params()` function generates three separate dataframes:

- `gamma` dataframe for $\gamma$: each row corresponds to an item pair
  within a block. For instance row 4 below shows the item threshold
  parameter between the first two items of the second question block.

- `items` dataframe for $\lambda$ and $\psi^2$: each row corresponds to
  an individual item within a block. For instance, row 1 below shows the
  item loading and unique variance for the first item in the first
  block, which measures the second dimension.

- `persons` dataframe for $\theta$: each row corresponds to an
  individual respondent. For instance, row 1 below shows the first
  respondent’s latent trait scores across the three traits measured

``` r
# three output dataframes for parameters
params
#> $gamma
#>   pair block gamma
#> 1  1-2     1  0.63
#> 2  1-3     1 -0.76
#> 3  2-3     1 -0.63
#> 4  1-2     2 -0.85
#> 5  1-3     2 -0.26
#> 6  2-3     2 -1.00
#> 
#> $items
#>   item block dim lambda psisq
#> 1    1     1   2  1.197  0.44
#> 2    2     1   3  0.928  0.52
#> 3    3     1   1 -0.765  0.10
#> 4    1     2   1 -0.486  0.63
#> 5    2     2   2  0.082  0.85
#> 6    3     2   3  0.918  0.29
#> 
#> $persons
#>   person theta_1 theta_2 theta_3
#> 1      1   -0.33   -0.19   0.093
#> 2      2    0.19   -1.07   0.916
```

After we have simulated the person- and item-parameters, we can now
simulate a random response pattern based on these values. The
`simulate_thirt_resp()` function takes in three input values: the three
dataframes in the same structure as `gamma, items, persons`.

``` r
# simulate response patterns based on previously generated parameters
resp <- do.call(simulate_thirt_resp, params)
```

This will generate two separate dataframes to represent the response
patterns:

- `items` dataframe: each row represents one item within a block. For
  instance, row 1 shows that the first item in the first block is used
  to measure the second dimension and that it is positively keyed (i.e.,
  with a positive item loading $\lambda$

- `resp` dataframe: each row represents one respondent within a block.
  For instance, row 1 shows that, for the first question block, the
  first respondent ranks the second item the highest, then the third
  item, then the first item. This response sequence is internally coded
  as the fourth possible response pattern for a three-item question
  block. This numbering system is for internal data storing purposes and
  has no substantive implications.

``` r
# two output dataframes for response patterns
resp
#> $items
#>   item block dim key
#> 1    1     1   2   1
#> 2    2     1   3   1
#> 3    3     1   1  -1
#> 4    1     2   1  -1
#> 5    2     2   2   1
#> 6    3     2   3   1
#> 
#> $resp
#>   person block resp     seq
#> 1      1     1    4 2, 3, 1
#> 2      2     1    4 2, 3, 1
#> 3      1     2    1 1, 2, 3
#> 4      2     2    3 2, 1, 3
```

In order to generate the indices for the `resp` column, please run
`mupp::find_all_permutations(n = <number_of_items_per_block>, init = 1)`.
For user convenience, a list of indices for a 3-item and 4-item block is
shown below:

``` r
# 3-item block
triplets <- as.data.frame(mupp::find_all_permutations(n = 3, init = 1))
triplets$seq <- paste0(triplets$V1, ", ", triplets$V2, ", ", triplets$V3)
triplets <- data.frame(
  resp = 1:nrow(triplets),
  seq  = triplets$seq)
knitr::kable(triplets)
```

| resp | seq     |
|-----:|:--------|
|    1 | 1, 2, 3 |
|    2 | 1, 3, 2 |
|    3 | 2, 1, 3 |
|    4 | 2, 3, 1 |
|    5 | 3, 1, 2 |
|    6 | 3, 2, 1 |

``` r

# 4-item block
quads <- as.data.frame(mupp::find_all_permutations(n = 4, init = 1))
quads$seq <- paste0(quads$V1, ", ", quads$V2, ", ", quads$V3, ", ", quads$V4)
quads <- data.frame(
  resp = 1:nrow(quads),
  seq  = quads$seq)
knitr::kable(quads)
```

| resp | seq        |
|-----:|:-----------|
|    1 | 1, 2, 3, 4 |
|    2 | 1, 2, 4, 3 |
|    3 | 1, 3, 2, 4 |
|    4 | 1, 3, 4, 2 |
|    5 | 1, 4, 2, 3 |
|    6 | 1, 4, 3, 2 |
|    7 | 2, 1, 3, 4 |
|    8 | 2, 1, 4, 3 |
|    9 | 2, 3, 1, 4 |
|   10 | 2, 3, 4, 1 |
|   11 | 2, 4, 1, 3 |
|   12 | 2, 4, 3, 1 |
|   13 | 3, 1, 2, 4 |
|   14 | 3, 1, 4, 2 |
|   15 | 3, 2, 1, 4 |
|   16 | 3, 2, 4, 1 |
|   17 | 3, 4, 1, 2 |
|   18 | 3, 4, 2, 1 |
|   19 | 4, 1, 2, 3 |
|   20 | 4, 1, 3, 2 |
|   21 | 4, 2, 1, 3 |
|   22 | 4, 2, 3, 1 |
|   23 | 4, 3, 1, 2 |
|   24 | 4, 3, 2, 1 |

### Parameter Estimation

Using the previously simulated response patterns, we can now apply the
main estimation function to estimate all person- and item-parameters. In
order to apply this function on real data, researchers must structure
their data to follow the template of the two `resp` and `items`
dataframes.

In addition to these two required inputs, we may change the `control`
argument to adjust the MCMC algorithm:

- `n_iter`: the number of MCMC iterations, exclude for default of 10000

- `n_burnin`: the number of burn-in MCMC iterations, exclude for default
  of 1000

- `step_size_sd`: the step size standard deviation for the random-walk
  Metropolis step, exclude for default of 0.1 across parameters

``` r
# estimate person- and item-parameters based on previously generated response patterns
estimations <- estimate_thirt_params_mcmc(
  resp = resp$resp,
  items = resp$items,
  control = list(n_iter = 100, # number of iterations
                 n_burnin = 20)) # number of burn-ins
```

The estimated parameter values are computed as the mean across all
iterations after the burn-in period, and they may be accessed by calling
the `mean_mcmc` list object stored within the output of
`estimate_thirt_params_mcmc()`. There are four separate dataframes for
the different person- and item-parameters. These parameter estimates can
then be directly compared to the true simulated parameters that
generated the model. We should note, however, that this illustrative
example used a very simple data-generating model (with few people,
items, and blocks) as well as a very short MCMC chain with only 100
iterations.

``` r
# view the estimated parameter values
estimations$mean_mcmc
#> $gamma
#>        [,1]
#> [1,]  0.044
#> [2,]  0.028
#> [3,] -0.065
#> [4,]  0.095
#> [5,]  0.083
#> [6,] -0.050
#> 
#> $lambda
#>       [,1]
#> [1,]  1.03
#> [2,]  0.97
#> [3,] -1.04
#> [4,] -1.02
#> [5,]  1.04
#> [6,]  0.96
#> 
#> $psisq
#>      [,1]
#> [1,] 0.79
#> [2,] 0.87
#> [3,] 0.62
#> [4,] 0.63
#> [5,] 0.68
#> [6,] 0.77
#> 
#> $theta
#>       [,1]  [,2]  [,3]
#> [1,] -0.52 -0.32 -0.38
#> [2,] -0.25  0.33  0.34
```

### Fixing Operational Items Parameters

Often, test developers may want to fix item parameters $\gamma$,
$\lambda$, and $\psi^2$ for the operational items and calibrate only
field test items. Pre-determined parameters for the operational items
can be specified with the `op_params` argument, which defaults to
`NULL`. This argument needs to be a list containing matrices named
`gamma`, `lambda`, and `psisq`. Parameter values for the fixed
operational items should be provided whereas those for field test items
should be left as `NA`. The order of parameters should be the same as
shown in `simulate_thirt_params()` outputs in the `gamma` and `items`
data frame for an equivalent design. In the example below, we consider
items 1 and 2 to be operational items. This means we would fix $\lambda$
and $\psi^2$ for these individual items and $\gamma$ for the pair 1-2.

``` r
# matrix for each params
op_gamma <- matrix(
  # first pair 1-2 is fixed for each of 2 blocks
  c(0.6, NA, NA,
    -0.8, NA, NA)
)
op_lambda <- matrix(
  # first 2 items are fixed for each of 2 blocks
  c(0.9, 0.8, NA,
    -0.5, 0.1, NA)
)
op_psisq <- matrix(
  # first 2 items are fixed for each of 2 blocks
  c(0.4, 0.5, NA,
    0.6, 0.8, NA)
)

# rerun the estimation algorithm providing operational params
estimations_op <- estimate_thirt_params_mcmc(
  # all arguments same as before
  resp = resp$resp,
  items = resp$items,
  control = list(n_iter = 100, 
                 n_burnin = 20),
  # add operational items params
  op_params = list(gamma = op_gamma,
                   lambda = op_lambda,
                   psisq = op_psisq))
#>   |                                                                            |                                                                    |   0%  |                                                                            |                                                                    |   1%  |                                                                            |                                                                    |   2%  |                                                                            |mcmc                                                                |   3%  |                                                                            |mcmc                                                                |   4%  |                                                                            |mcmc                                                                |   5%  |                                                                            |mcmc                                                                |   6%  |                                                                            |mcmc                                                                |   7%  |                                                                            |mcmc                                                                |   8%  |                                                                            |mcmcmcmc                                                            |   9%  |                                                                            |mcmcmcmc                                                            |  10%  |                                                                            |mcmcmcmc                                                            |  11%  |                                                                            |mcmcmcmc                                                            |  12%  |                                                                            |mcmcmcmc                                                            |  13%  |                                                                            |mcmcmcmc                                                            |  14%  |                                                                            |mcmcmcmcmcmc                                                        |  15%  |                                                                            |mcmcmcmcmcmc                                                        |  16%  |                                                                            |mcmcmcmcmcmc                                                        |  17%  |                                                                            |mcmcmcmcmcmc                                                        |  18%  |                                                                            |mcmcmcmcmcmc                                                        |  19%  |                                                                            |mcmcmcmcmcmc                                                        |  20%  |                                                                            |mcmcmcmcmcmcmcmc                                                    |  21%  |                                                                            |mcmcmcmcmcmcmcmc                                                    |  22%  |                                                                            |mcmcmcmcmcmcmcmc                                                    |  23%  |                                                                            |mcmcmcmcmcmcmcmc                                                    |  24%  |                                                                            |mcmcmcmcmcmcmcmc                                                    |  25%  |                                                                            |mcmcmcmcmcmcmcmc                                                    |  26%  |                                                                            |mcmcmcmcmcmcmcmcmcmc                                                |  27%  |                                                                            |mcmcmcmcmcmcmcmcmcmc                                                |  28%  |                                                                            |mcmcmcmcmcmcmcmcmcmc                                                |  29%  |                                                                            |mcmcmcmcmcmcmcmcmcmc                                                |  30%  |                                                                            |mcmcmcmcmcmcmcmcmcmc                                                |  31%  |                                                                            |mcmcmcmcmcmcmcmcmcmc                                                |  32%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmc                                            |  33%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmc                                            |  34%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmc                                            |  35%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmc                                            |  36%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmc                                            |  37%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmc                                            |  38%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmc                                        |  39%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmc                                        |  40%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmc                                        |  41%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmc                                        |  42%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmc                                        |  43%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmc                                        |  44%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                    |  45%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                    |  46%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                    |  47%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                    |  48%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                    |  49%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                    |  50%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                |  51%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                |  52%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                |  53%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                |  54%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                                |  55%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                            |  56%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                            |  57%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                            |  58%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                            |  59%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                            |  60%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                            |  61%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                        |  62%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                        |  63%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                        |  64%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                        |  65%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                        |  66%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                        |  67%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                    |  68%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                    |  69%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                    |  70%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                    |  71%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                    |  72%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                    |  73%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                |  74%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                |  75%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                |  76%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                |  77%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                |  78%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc                |  79%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc            |  80%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc            |  81%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc            |  82%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc            |  83%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc            |  84%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc            |  85%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc        |  86%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc        |  87%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc        |  88%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc        |  89%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc        |  90%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc        |  91%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc    |  92%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc    |  93%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc    |  94%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc    |  95%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc    |  96%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc    |  97%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc|  98%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc|  99%  |                                                                            |mcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmcmc| 100%

# view the estimated parameter values
estimations_op$mean_mcmc
#> $gamma
#>         [,1]
#> [1,]  0.6000
#> [2,]  0.0123
#> [3,]  0.0409
#> [4,] -0.8000
#> [5,] -0.0766
#> [6,]  0.0095
#> 
#> $lambda
#>       [,1]
#> [1,]  0.90
#> [2,]  0.80
#> [3,] -0.99
#> [4,] -0.50
#> [5,]  0.10
#> [6,]  1.01
#> 
#> $psisq
#>      [,1]
#> [1,] 0.40
#> [2,] 0.50
#> [3,] 0.78
#> [4,] 0.60
#> [5,] 0.80
#> [6,] 0.70
#> 
#> $theta
#>        [,1]  [,2]  [,3]
#> [1,]  0.113 -0.59 -0.52
#> [2,] -0.096 -0.62 -0.21
```
