
<!-- README.md is generated from README.Rmd. Please edit that file -->

# thirt

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/nguyenllpsych/thirt/branch/main/graph/badge.svg)](https://codecov.io/gh/nguyenllpsych/thirt?branch=main)
[![R-CMD-check](https://github.com/nguyenllpsych/thirt/actions/workflows/check-release.yaml/badge.svg)](https://github.com/nguyenllpsych/thirt/actions)

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
#> [1,]  0.047
#> [2,]  0.034
#> [3,] -0.076
#> [4,]  0.112
#> [5,]  0.099
#> [6,] -0.063
#> 
#> $lambda
#>       [,1]
#> [1,]  1.04
#> [2,]  0.97
#> [3,] -1.05
#> [4,] -1.04
#> [5,]  1.04
#> [6,]  0.96
#> 
#> $psisq
#>      [,1]
#> [1,] 0.75
#> [2,] 0.86
#> [3,] 0.55
#> [4,] 0.57
#> [5,] 0.64
#> [6,] 0.73
#> 
#> $theta
#>      [,1]  [,2]  [,3]
#> [1,] -0.6 -0.40 -0.39
#> [2,] -0.3  0.25  0.36
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
data frame for an equivalent design.

When operational items are specified, they alone are used for trait
scoring. Test items do not affect trait scores.

In the example below, we consider items 1 and 2 to be operational items.
This means we would fix $\lambda$ and $\psi^2$ for these individual
items and $\gamma$ for the pair 1-2.

Trait scores can be obtained in `mean_mcmc$theta` whereas item
calibration results can be obtained in `mean_mcmc_test$gamma`,
`mean_mcmc_test$lambda`, and `mean_mcmc_test$psisq`. Note that the
operational item parameters are fixed.

``` r
# matrix for each params
# operational parameters
op_gamma <- matrix(
  # first pair 1-2 is fixed for each of 2 blocks
  c(0.4, NA, NA,
   -0.9, NA, NA)
  )
op_lambda <- matrix(
  # first 2 items are fixed for each of 2 blocks
  c(-0.65, 0.55, NA,
    0.57, -0.40, NA)
  )
op_psisq <- matrix(
  # first 2 items are fixed for each of 2 blocks
  c(0.89, 0.72, NA,
    0.02, 0.41, NA)
)

# rerun the estimation algorithm providing operational params
start_mcmc <- Sys.time()
estimations_op <- estimate_thirt_params_mcmc(
  resp  = resp$resp,
  items = resp$items,
  control = list(n_iter   = 500,
                 n_burnin = 20,
                 step_size_sd = c(0.5,     # theta
                                  0.01,    # gamma
                                  0.01,    # lambda
                                  0.075)), # psisq
  op_params = list(gamma  = op_gamma,
                   lambda = op_lambda,
                   psisq  = op_psisq)
)
#>   |                                                                          |                                                                  |   0%  |                                                                          |                                                                  |   1%  |                                                                          |                                                                  |   2%  |                                                                          |                                                                  |   3%  |                                                                          |                                                                  |   4%  |                                                                          |score-                                                            |   5%  |                                                                          |score-                                                            |   6%  |                                                                          |score-                                                            |   7%  |                                                                          |score-                                                            |   8%  |                                                                          |score-                                                            |   9%  |                                                                          |score-                                                            |  10%  |                                                                          |score-                                                            |  11%  |                                                                          |score-                                                            |  12%  |                                                                          |score-                                                            |  13%  |                                                                          |score-                                                            |  14%  |                                                                          |score-score-                                                      |  14%  |                                                                          |score-score-                                                      |  15%  |                                                                          |score-score-                                                      |  16%  |                                                                          |score-score-                                                      |  17%  |                                                                          |score-score-                                                      |  18%  |                                                                          |score-score-                                                      |  19%  |                                                                          |score-score-                                                      |  20%  |                                                                          |score-score-                                                      |  21%  |                                                                          |score-score-                                                      |  22%  |                                                                          |score-score-                                                      |  23%  |                                                                          |score-score-score-                                                |  23%  |                                                                          |score-score-score-                                                |  24%  |                                                                          |score-score-score-                                                |  25%  |                                                                          |score-score-score-                                                |  26%  |                                                                          |score-score-score-                                                |  27%  |                                                                          |score-score-score-                                                |  28%  |                                                                          |score-score-score-                                                |  29%  |                                                                          |score-score-score-                                                |  30%  |                                                                          |score-score-score-                                                |  31%  |                                                                          |score-score-score-                                                |  32%  |                                                                          |score-score-score-score-                                          |  32%  |                                                                          |score-score-score-score-                                          |  33%  |                                                                          |score-score-score-score-                                          |  34%  |                                                                          |score-score-score-score-                                          |  35%  |                                                                          |score-score-score-score-                                          |  36%  |                                                                          |score-score-score-score-                                          |  37%  |                                                                          |score-score-score-score-                                          |  38%  |                                                                          |score-score-score-score-                                          |  39%  |                                                                          |score-score-score-score-                                          |  40%  |                                                                          |score-score-score-score-                                          |  41%  |                                                                          |score-score-score-score-score-                                    |  41%  |                                                                          |score-score-score-score-score-                                    |  42%  |                                                                          |score-score-score-score-score-                                    |  43%  |                                                                          |score-score-score-score-score-                                    |  44%  |                                                                          |score-score-score-score-score-                                    |  45%  |                                                                          |score-score-score-score-score-                                    |  46%  |                                                                          |score-score-score-score-score-                                    |  47%  |                                                                          |score-score-score-score-score-                                    |  48%  |                                                                          |score-score-score-score-score-                                    |  49%  |                                                                          |score-score-score-score-score-                                    |  50%  |                                                                          |score-score-score-score-score-score-                              |  50%  |                                                                          |score-score-score-score-score-score-                              |  51%  |                                                                          |score-score-score-score-score-score-                              |  52%  |                                                                          |score-score-score-score-score-score-                              |  53%  |                                                                          |score-score-score-score-score-score-                              |  54%  |                                                                          |score-score-score-score-score-score-                              |  55%  |                                                                          |score-score-score-score-score-score-                              |  56%  |                                                                          |score-score-score-score-score-score-                              |  57%  |                                                                          |score-score-score-score-score-score-                              |  58%  |                                                                          |score-score-score-score-score-score-                              |  59%  |                                                                          |score-score-score-score-score-score-score-                        |  59%  |                                                                          |score-score-score-score-score-score-score-                        |  60%  |                                                                          |score-score-score-score-score-score-score-                        |  61%  |                                                                          |score-score-score-score-score-score-score-                        |  62%  |                                                                          |score-score-score-score-score-score-score-                        |  63%  |                                                                          |score-score-score-score-score-score-score-                        |  64%  |                                                                          |score-score-score-score-score-score-score-                        |  65%  |                                                                          |score-score-score-score-score-score-score-                        |  66%  |                                                                          |score-score-score-score-score-score-score-                        |  67%  |                                                                          |score-score-score-score-score-score-score-                        |  68%  |                                                                          |score-score-score-score-score-score-score-score-                  |  68%  |                                                                          |score-score-score-score-score-score-score-score-                  |  69%  |                                                                          |score-score-score-score-score-score-score-score-                  |  70%  |                                                                          |score-score-score-score-score-score-score-score-                  |  71%  |                                                                          |score-score-score-score-score-score-score-score-                  |  72%  |                                                                          |score-score-score-score-score-score-score-score-                  |  73%  |                                                                          |score-score-score-score-score-score-score-score-                  |  74%  |                                                                          |score-score-score-score-score-score-score-score-                  |  75%  |                                                                          |score-score-score-score-score-score-score-score-                  |  76%  |                                                                          |score-score-score-score-score-score-score-score-                  |  77%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  77%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  78%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  79%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  80%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  81%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  82%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  83%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  84%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  85%  |                                                                          |score-score-score-score-score-score-score-score-score-            |  86%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  86%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  87%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  88%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  89%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  90%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  91%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  92%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  93%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  94%  |                                                                          |score-score-score-score-score-score-score-score-score-score-      |  95%  |                                                                          |score-score-score-score-score-score-score-score-score-score-score-|  96%  |                                                                          |score-score-score-score-score-score-score-score-score-score-score-|  97%  |                                                                          |score-score-score-score-score-score-score-score-score-score-score-|  98%  |                                                                          |score-score-score-score-score-score-score-score-score-score-score-|  99%  |                                                                          |score-score-score-score-score-score-score-score-score-score-score-| 100%  |                                                                          |                                                                  |   0%  |                                                                          |                                                                  |   1%  |                                                                          |                                                                  |   2%  |                                                                          |                                                                  |   3%  |                                                                          |                                                                  |   4%  |                                                                          |items-                                                            |   5%  |                                                                          |items-                                                            |   6%  |                                                                          |items-                                                            |   7%  |                                                                          |items-                                                            |   8%  |                                                                          |items-                                                            |   9%  |                                                                          |items-                                                            |  10%  |                                                                          |items-                                                            |  11%  |                                                                          |items-                                                            |  12%  |                                                                          |items-                                                            |  13%  |                                                                          |items-                                                            |  14%  |                                                                          |items-items-                                                      |  14%  |                                                                          |items-items-                                                      |  15%  |                                                                          |items-items-                                                      |  16%  |                                                                          |items-items-                                                      |  17%  |                                                                          |items-items-                                                      |  18%  |                                                                          |items-items-                                                      |  19%  |                                                                          |items-items-                                                      |  20%  |                                                                          |items-items-                                                      |  21%  |                                                                          |items-items-                                                      |  22%  |                                                                          |items-items-                                                      |  23%  |                                                                          |items-items-items-                                                |  23%  |                                                                          |items-items-items-                                                |  24%  |                                                                          |items-items-items-                                                |  25%  |                                                                          |items-items-items-                                                |  26%  |                                                                          |items-items-items-                                                |  27%  |                                                                          |items-items-items-                                                |  28%  |                                                                          |items-items-items-                                                |  29%  |                                                                          |items-items-items-                                                |  30%  |                                                                          |items-items-items-                                                |  31%  |                                                                          |items-items-items-                                                |  32%  |                                                                          |items-items-items-items-                                          |  32%  |                                                                          |items-items-items-items-                                          |  33%  |                                                                          |items-items-items-items-                                          |  34%  |                                                                          |items-items-items-items-                                          |  35%  |                                                                          |items-items-items-items-                                          |  36%  |                                                                          |items-items-items-items-                                          |  37%  |                                                                          |items-items-items-items-                                          |  38%  |                                                                          |items-items-items-items-                                          |  39%  |                                                                          |items-items-items-items-                                          |  40%  |                                                                          |items-items-items-items-                                          |  41%  |                                                                          |items-items-items-items-items-                                    |  41%  |                                                                          |items-items-items-items-items-                                    |  42%  |                                                                          |items-items-items-items-items-                                    |  43%  |                                                                          |items-items-items-items-items-                                    |  44%  |                                                                          |items-items-items-items-items-                                    |  45%  |                                                                          |items-items-items-items-items-                                    |  46%  |                                                                          |items-items-items-items-items-                                    |  47%  |                                                                          |items-items-items-items-items-                                    |  48%  |                                                                          |items-items-items-items-items-                                    |  49%  |                                                                          |items-items-items-items-items-                                    |  50%  |                                                                          |items-items-items-items-items-items-                              |  50%  |                                                                          |items-items-items-items-items-items-                              |  51%  |                                                                          |items-items-items-items-items-items-                              |  52%  |                                                                          |items-items-items-items-items-items-                              |  53%  |                                                                          |items-items-items-items-items-items-                              |  54%  |                                                                          |items-items-items-items-items-items-                              |  55%  |                                                                          |items-items-items-items-items-items-                              |  56%  |                                                                          |items-items-items-items-items-items-                              |  57%  |                                                                          |items-items-items-items-items-items-                              |  58%  |                                                                          |items-items-items-items-items-items-                              |  59%  |                                                                          |items-items-items-items-items-items-items-                        |  59%  |                                                                          |items-items-items-items-items-items-items-                        |  60%  |                                                                          |items-items-items-items-items-items-items-                        |  61%  |                                                                          |items-items-items-items-items-items-items-                        |  62%  |                                                                          |items-items-items-items-items-items-items-                        |  63%  |                                                                          |items-items-items-items-items-items-items-                        |  64%  |                                                                          |items-items-items-items-items-items-items-                        |  65%  |                                                                          |items-items-items-items-items-items-items-                        |  66%  |                                                                          |items-items-items-items-items-items-items-                        |  67%  |                                                                          |items-items-items-items-items-items-items-                        |  68%  |                                                                          |items-items-items-items-items-items-items-items-                  |  68%  |                                                                          |items-items-items-items-items-items-items-items-                  |  69%  |                                                                          |items-items-items-items-items-items-items-items-                  |  70%  |                                                                          |items-items-items-items-items-items-items-items-                  |  71%  |                                                                          |items-items-items-items-items-items-items-items-                  |  72%  |                                                                          |items-items-items-items-items-items-items-items-                  |  73%  |                                                                          |items-items-items-items-items-items-items-items-                  |  74%  |                                                                          |items-items-items-items-items-items-items-items-                  |  75%  |                                                                          |items-items-items-items-items-items-items-items-                  |  76%  |                                                                          |items-items-items-items-items-items-items-items-                  |  77%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  77%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  78%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  79%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  80%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  81%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  82%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  83%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  84%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  85%  |                                                                          |items-items-items-items-items-items-items-items-items-            |  86%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  86%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  87%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  88%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  89%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  90%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  91%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  92%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  93%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  94%  |                                                                          |items-items-items-items-items-items-items-items-items-items-      |  95%  |                                                                          |items-items-items-items-items-items-items-items-items-items-items-|  96%  |                                                                          |items-items-items-items-items-items-items-items-items-items-items-|  97%  |                                                                          |items-items-items-items-items-items-items-items-items-items-items-|  98%  |                                                                          |items-items-items-items-items-items-items-items-items-items-items-|  99%  |                                                                          |items-items-items-items-items-items-items-items-items-items-items-| 100%
end_mcmc   <- Sys.time()

# view the estimated parameter values - person scores
estimations_op$mean_mcmc$theta
#>       [,1]  [,2]  [,3]
#> [1,]  0.31  0.48 0.036
#> [2,] -1.41 -0.40 0.456

# view the estimated parameter values - item parameters
estimations_op$mean_mcmc_test$gamma
#>        [,1]
#> [1,]  0.400
#> [2,]  0.042
#> [3,]  0.169
#> [4,] -0.900
#> [5,]  0.149
#> [6,]  0.048
estimations_op$mean_mcmc_test$lambda
#>       [,1]
#> [1,] -0.65
#> [2,]  0.55
#> [3,] -1.00
#> [4,]  0.57
#> [5,] -0.40
#> [6,]  1.00
estimations_op$mean_mcmc_test$psisq
#>      [,1]
#> [1,] 0.89
#> [2,] 0.72
#> [3,] 0.45
#> [4,] 0.02
#> [5,] 0.41
#> [6,] 0.65
```
