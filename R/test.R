#' Simulation tests
#'
#' Examine parameter recovery and performance using simulated data through parallelization
#'
#' @param icondition integer indicating the iteration of simulation condition
#'
#' @param tirt boolean indicating whether estimation results should be compared with the thurstonianIRT package
#' @param seed random seed
#'
#' @return save a timestamped RData file in the current working directory
#'
#' @examples
#' \dontrun{
#'
#' # load parallel package
#' library(parallel)
#'
#' # Simulation conditions
#' n_person      <- 2
#' n_item        <- 4
#' n_neg         <- 1
#' n_block       <- 5
#' n_dim         <- c(4, 6)
#' n_iter        <- 100
#' n_burnin      <- 20
#' step_size_sd  <- 0.1
#'
#' condition_mat <- expand.grid(n_person = n_person,
#'                              n_item   = n_item,
#'                              n_neg    = n_neg,
#'                              n_block  = n_block,
#'                              n_dim    = n_dim,
#'                              n_iter   = n_iter,
#'                              n_burnin = n_burnin,
#'                              step_size_sd = step_size_sd)
#'
#' n_condition   <- nrow(condition_mat)
#'
#' # create clusters
#' n_core <- detectCores()
#' cl     <- makeCluster(n_core)
#' RNGkind("L'Ecuyer-CMRG")
#'
#' # load libraries in clusters
#' clusterEvalQ(cl, library(thirt))
#' clusterExport(cl, condition_mat)
#'
#' # simulation tests
#' parLapply(cl, 1:n_condition, sim_test))
#'
#' # stop cluster
#' stopCluster(cl)
#' }
#'
#'
#' @importFrom thurstonianIRT
#'             fit_TIRT_stan
#'             fit_TIRT_lavaan
#'
#' @export
sim_test <- function(icondition,
                     tirt = TRUE,
                     seed = 20220415){

  # set seed for reproducibility
  set.seed(seed)

  # pull important parameters
  n_person     <- condition_mat[icondition, "n_person"]
  n_neg        <- condition_mat[icondition, "n_neg"]
  n_block      <- condition_mat[icondition, "n_block"]
  n_item       <- rep(condition_mat[icondition, "n_item"], n_block)
  n_dim        <- condition_mat[icondition, "n_dim"]
  n_iter       <- condition_mat[icondition, "n_iter"]
  n_burnin     <- condition_mat[icondition, "n_burnin"]
  step_size_sd <- condition_mat[icondition, "step_size_sd"]

  # simulate parameters and responses
  params <- simulate_thirt_params(n_person = n_person,
                                  n_item   = n_item,
                                  n_neg    = n_neg,
                                  n_block  = n_block,
                                  n_dim    = n_dim)
  resp   <- do.call(simulate_thirt_resp, params)

  # estimation output
  start_mcmc <- Sys.time()
  output     <- estimate_thirt_params_mcmc(
    resp  = resp$resp,
    items = resp$items,
    control = list(n_iter   = n_iter,
                   n_burnin = n_burnin,
                   step_size_sd = step_size_sd)
  )
  end_mcmc   <- Sys.time()
  time_mcmc  <- end_mcmc - start_mcmc

  # acceptance count
  count_accept <- count_accept(all_iters = output$all_iters,
                               resp      = resp)

  # compare with thurstonianIRT
  if(tirt) {

    # restructure data for thurstonianIRT
    TIRT_data <- convert_TIRT(resp)

    # thurstonianIRT fit stan
    start_stan <- Sys.time()
    TIRT_stan  <- fit_TIRT_stan(TIRT_data, chains = 1,
                                iter = n_iter,
                                warmup = n_burnin)
    end_stan   <- Sys.time()
    time_stan  <- end_stan - start_stan

    # thurstonianIRT fit lavaan
    start_lavaan <- Sys.time()
    try <- try(fit_TIRT_lavaan(TIRT_data))
    if(class(try) != "try-error") {
      TIRT_lavaan <- try
    } else {
      TIRT_lavaan <- "error"
    }
    end_lavaan   <- Sys.time()
    time_lavaan  <- end_lavaan - start_lavaan

    # save info
    save(n_person, n_item, n_neg, n_block, n_dim,
         n_iter, n_burnin, step_size_sd,
         params, output, count_accept, time_mcmc,
         time_stan, TIRT_stan,
         time_lavaan, TIRT_lavaan,
         file = paste0("sim_", as.numeric(Sys.time()),".RData"))
  } else {
    # save thirt-only info
    save(n_person, n_item, n_neg, n_block, n_dim,
         n_iter, n_burnin, step_size_sd,
         params, output, count_accept, time_mcmc,
         file = paste0("sim_", as.numeric(Sys.time()),".RData"))
  }
}

#' Convert to thurstonianIRT quads
#'
#' Convert thirt responses with 4 items per block to thurstonianIRT data structure
#'
#' @param resp a list with:
#'              (1) data.frame `item` indicating items, blocks, and dimensions, and
#'              (2) data.frame `resp` indicating people, blocks, response numbers, and response sequences
#' @param n_item an integer indicating number of items per block.
#'               currently only supports 4 items per block
#'
#' @return a data.frame following the structure of thurstonianIRT::triplets_long but with 4 items per block
#'
#' @examples
#' \dontrun{
#' set.seed(202106)
#'
#' params <- simulate_thirt_params(n_person = 20,
#'                                 n_item   = 4,
#'                                 n_block  = 2,
#'                                 n_dim    = 4)
#'
#' resp   <- do.call(simulate_thirt_resp, params)
#'
#' quads_long <- convert_TIRT(resp, n_item = 4)
#'
#' }
#'
#' @importFrom mupp
#'             find_all_permutations
#'
#' @importFrom thurstonianIRT
#'             set_block
#'             make_TIRT_data
#'
#' @export
convert_TIRT <- function(resp, n_item = 4) {

  # argument check
  # this function currently only works for quads design: 4 items per block
  if (n_item != 4) {
    stop("only quads design with 4 items per block is currently supported")
  } # END n_item arg check

  # pull out important info
  n_person <- length(unique(resp$resp$person))
  n_dim    <- length(unique(resp$item$dim))
  n_block  <- length(unique(resp$resp$block))

  # create data frame of all possible permutations for 4 items
  perms <- as.data.frame(
    find_all_permutations(n    = n_item,
                          init = 1)
  )

  # the combinations and names of those combinations
  combo <- combn2(seq(min(perms$V1), max(perms$V1)))
  nms   <- paste0(combo[1, ], "-", combo[2, ])
  names <- c()
  for(block in seq(n_block)){
    names <- append(x = names,
                    values = paste0("i", combo[1, ]+n_item*(block-1),
                                    "i", combo[2, ]+n_item*(block-1)))
  }

  # the values and the column that each preference appears
  vals  <- as.matrix(perms)
  cols  <- col(vals)

  # determining whether combination is higher
  for(col in seq_len(ncol(combo))){
    col1           <- rowSums(cols * (vals == combo[1, col]))
    col2           <- rowSums(cols * (vals == combo[2, col]))
    perms[ , nms[col]] <- as.numeric(col2 > col1)
  }
  perms <- perms[, -c(1:n_item)]

  # create quads data
  quads <- matrix(nrow = n_person,

                # number of pairs across blocks
                ncol = 6*n_block) %>%
  as.data.frame()
  names(quads) <- names

  # populate quads with pairwise binary response for each person
  # same format as thurstonianIRT::triplets
  for (block in seq(n_block)) {
    for(person in seq(n_person)){
      r <- resp$resp[which(resp$resp$person == person &
                           resp$resp$block == block), "resp"]
      quads[person, c((1+(block-1)*6):(6+(block-1)*6))] <- perms[r, ]
    }
  }

  # create blocks
  blocks <- set_block(items = "i1", traits = "t1", signs = 1)
  blocks[[1]] <- NULL
  for (block in seq(n_block)) {
    current_items <- resp$items[which(resp$items$block == block), ]
    blocks <- blocks +
      set_block(items = paste0("i", current_items$item+n_item*(block-1)),
                traits = paste0("t", current_items$dim),
                signs = current_items$key)
  }

  # make_TIRT_data long format
  # similar to thurstonianIRT::triplets_long
  quads_long <- make_TIRT_data(
    data = quads, blocks = blocks, direction = "larger",
    format = "pairwise", family = "bernoulli", range = c(0,1)
  )

  quads_long
}

