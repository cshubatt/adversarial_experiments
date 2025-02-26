# -----------------------------------------------------------------------------
# Restrictiveness for theories of choice under risk
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To run: Rscript --vanilla --verbose 01_calc_restrictions.R k=10 nsims=200 test_k=5 >log/01_calc_restrictions.log 2>&1 # nolint
# -----------------------------------------------------------------------------

# Load libraries
renv::load(here::here())

library(dplyr) # %>%
library(purrr) # map2
library(here)
library(testit) # assert()
library(glue)
library(magrittr)
library(revpref)
library(readr)
library(optimx)
library(hitandrun)
library(doParallel)
library(foreach)
library(parallel)

global_seed <- 214
set.seed(global_seed)

h <- modules::use(here("lib", "value_functions.R"))

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
k <- as.numeric(sub("k=", "", args[grep("k=", args)]))[1]
nsims <- as.numeric(sub("nsims=", "", args[grep("nsims=", args)]))
test_k <- as.numeric(sub("test_k=", "", args[grep("test_k=", args)]))
theory <- as.character(
  sub("theory=", "", args[grep("theory=", args)])
)

k <- 25
nsims <- 200
test_k <- 10
theory <- "cpt"

message(
  "k = ", k, ", nsims = ", nsims, ", test_k = ", test_k, ", theory = ", theory
)
odir <- glue("data_k{k}_nsim{nsims}_{theory}")

# Functions -------------------------------------------------------------------
choice_mse <- function(data, params, theory = "cpt") {
  # Get MSE between v (in data) and v_hat (from params + theory)
  # data <- data %>%
  #   mutate(
  #     p_li = map(p, ~ c(.x, 1 - .x)),
  #     x_li = map2(w, l, ~ c(.x, .y)),
  #   )

  # Calculate v_hat
  if (theory == "cpt") {
    alpha <- ifelse(
      is.null(params$alpha), 1, params$alpha[[1]]
    )
    beta <- ifelse(
      is.null(params$beta), alpha, params$beta[[1]]
    )
    lambda <- ifelse(
      is.null(params$lambda), 1, params$lambda[[1]]
    )
    chi <- ifelse(
      is.null(params$chi), 1, params$chi[[1]]
    )
    gamma <- ifelse(
      is.null(params$gamma), 1, params$gamma[[1]]
    )

    v_hat <- pmap(
      .l = list(data$p, data$w, data$l),
      value_function,
      alpha = alpha, chi = chi, gamma = gamma
    ) %>% unlist()
  } else {
    stop("Invalid theory parameterization.")
  }

  mse <- mean((data$v - v_hat)^2)

  return(mse)
}

weight_p <- function(p, chi, gamma) {
  w_p <- chi * p^gamma / (chi * p^gamma + (1 - p)^gamma)
  return(w_p)
}

value_function <- function(p, w, l, alpha, chi, gamma) {
  w_p <- weight_p(p, chi, gamma)
  v <- w_p * w^alpha + (1 - w_p) * l^alpha
  return(v)
}

get_eligibility <- function(data) {
  eligibility_list <- list()
  pair_count <- 0

  for (i in seq_len(nrow(data))) {
    for (j in seq_len(nrow(data))[-(1:i)]) {
      w_comp_g <- data$w[i] >= data$w[j]
      l_comp_g <- data$l[i] >= data$l[j]
      p_comp_g <- data$p[i] >= data$p[j]
      w_comp_l <- data$w[i] <= data$w[j]
      l_comp_l <- data$l[i] <= data$l[j]
      p_comp_l <- data$p[i] <= data$p[j]

      weak_dominance <- (w_comp_g & l_comp_g & p_comp_g) |
        (w_comp_l & l_comp_l & p_comp_l)
      any_strict <- (data$w[i] != data$w[j]) ||
        (data$l[i] != data$l[j]) ||
        (data$p[i] != data$p[j])

      if (weak_dominance && any_strict) {
        i_dominates <- w_comp_g & l_comp_g & p_comp_g
        pair_count <- pair_count + 1
        eligibility_list[[pair_count]] <- c(
          ind_fosd = ifelse(i_dominates, i, j),
          ind_other = ifelse(i_dominates, j, i)
        )
      }
    }
  }

  eligibility_matrix <- do.call(rbind, eligibility_list)
  eligibility_pairs <- as.data.frame(eligibility_matrix)
  colnames(eligibility_pairs) <- c("ind_fosd", "ind_other")

  return(eligibility_pairs)
}

get_constraints <- function(data, eligibility_pairs) {
  cons_fosd <- matrix(0, nrow = nrow(eligibility_pairs), ncol = nrow(data))
  for (i in seq_len(nrow(eligibility_pairs))) {
    cons_fosd[i, eligibility_pairs$ind_fosd[i]] <- -1
    cons_fosd[i, eligibility_pairs$ind_other[i]] <- 1
  }
  rhs_fosd <- rep(0, nrow(eligibility_pairs))

  cons <- rbind(
    diag(1, nrow = nrow(data)),
    diag(-1, nrow = nrow(data)),
    cons_fosd
  )
  rhs <- c(data$w, -data$l, rhs_fosd)
  dir <- rep("<=", nrow(cons))

  har_cons <- list(constr = cons, rhs = rhs, dir = dir)

  return(har_cons)
}

calc_restrictiveness <- function(
    data = bruhin, nsim = nsims, theory = "cpt", quietly = FALSE) {
  # Get eligibility pairs
  eligibility_pairs <- get_eligibility(data)

  # Given a set of problems data, calculate restrictiveness via simulation
  data <- data %>%
    mutate(v_base = p * w + (1 - p) * l)

  har_cons <- get_constraints(data, eligibility_pairs)

  sim_vs <- hitandrun(har_cons, n.samples = nsim)

  # Set up parallel processing
  num_cores <- detectCores() - 1 # Use all but one core
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  # # Ensures cluster stops even if error occurs
  # on.exit(stopCluster(cl), add = TRUE)

  clusterExport(
    cl,
    varlist = c("choice_mse", "value_function", "weight_p"),
    envir = environment()
  )

  # Parallelize optimization over nsim
  results <- foreach(
    i = seq_len(nsim), .combine = "rbind", .packages = c("optimx", "dplyr")
  ) %dopar% {
    library(purrr)

    sim_data <- data %>%
      mutate(v = sim_vs[i, ])

    disc_base <- mean((sim_data$v - sim_data$v_base)^2)

    optimizer <- function(p) {
      params <- list(
        alpha = p[1],
        beta = 1,
        lambda = 1,
        chi = p[2],
        gamma = p[3]
      )
      mse <- choice_mse(sim_data, params, theory)
      if (is.nan(mse) | is.infinite(mse)) {
        message("NaN or infinite MSE found.")
        print(p)
      }
      return(mse)
    }

    starting_params <- c(alpha = 1, chi = 1, gamma = 1)
    optimum <- optim(
      fn = optimizer, par = starting_params,
      # Restrictiveness changes a lot with bounds on CPT parameters!
      lower = c(0.05, 0.05, 0.05),
      upper = c(1, Inf, 1),
      method = "L-BFGS-B"
    )

    disc_theory <- ifelse(optimum$value > disc_base, disc_base, optimum$value)

    return(c(disc_theory, disc_base))
  }

  stopCluster(cl)

  disc_theory <- results[, 1]
  disc_base <- results[, 2]

  restr <- mean(disc_theory) / mean(disc_base)

  if (!quietly) {
    message("Mean discrepancy of theory: ", mean(disc_theory))
    message("Mean discrepancy of base: ", mean(disc_base))
    message("Mean restrictiveness: ", restr)
  }

  restr_df <- data.frame(
    disc_theory = disc_theory, disc_base = disc_base, restrictiveness = restr
  )

  return(restr_df)
}

# Load data -------------------------------------------------------------------
message("Loading Bruhin et al. problems...")
bruhin <- read_csv(here("data", "risk", "bruhin_lotteries.csv"))

message("Calculating restrictiveness of bruhin...")
start <- Sys.time()
bruhin_restr <- calc_restrictiveness(bruhin, nsim = nsims, theory = theory)
end <- Sys.time()
message("Time elapsed: ", end - start)

# Alternative dataset ---------------------------------------------------------
message("Constructing alternative sets of problems, restr for each...")
splits <- list(
  train = list(split_k = k, split_n = 2^10),
  valid = list(split_k = k, split_n = 2^7),
  test = list(split_k = test_k, split_n = 2^7)
)
# splits <- list(
#   train = list(split_k = k, split_n = 2),
#   valid = list(split_k = k, split_n = 2),
#   test = list(split_k = test_k, split_n = 2)
# )

for (split in names(splits)) {
  message("Generating ", split, " data...")
  split_seed <- which(names(splits) == split)
  split_k <- splits[[split]]$split_k
  split_n <- splits[[split]]$split_n

  fp <- file.path(odir, glue("{split}.rds"))
  fp_csv <- file.path(odir, glue("{split}.csv"))

  bruhin_split <- bruhin[1:split_k, ]

  if (file.exists(fp)) {
    restrictiveness <- read_rds(fp)
  } else {
    restrictiveness <- as.data.frame(
      matrix(0, ncol = split_k * 3 + 1, nrow = 1)
    )
    names(restrictiveness) <- c(
      glue("p_{1:split_k}"), glue("w_{1:split_k}"), glue("l_{1:split_k}"),
      "mean_restrictiveness"
    )

    # Row 1 of dataset = Bruhin data
    restrictiveness[1, ] <- c(
      bruhin$p, bruhin$w, bruhin$l, bruhin_restr$restrictiveness[1]
    )
  }

  add_seed <- 0

  for (i in seq_len(split_n)) {
    seed_i <- global_seed + add_seed
    add_seed <- add_seed + 1
    set.seed(seed_i)

    if (nrow(restrictiveness) >= i + 1) {
      next
    }
    if (i %% 10 == 0) {
      message("Generating alternative dataset ", i, "...")
    }

    alt_data <- bruhin_split %>%
      mutate(
        p = runif(n(), 0, 1) %>% round(digits = 2),
        w = runif(n(), 0, 150) %>% round(digits = 0),
        l = runif(n(), 0, w) %>% round(digits = 0)
      )
    # debug(calc_restrictiveness)
    restr_alt <- calc_restrictiveness(
      alt_data,
      nsim = nsims, quietly = TRUE
    )

    restrictiveness <- rbind(
      restrictiveness,
      c(
        alt_data$p, alt_data$w, alt_data$l,
        restr_alt$restrictiveness[1]
      )
    )
    assert(nrow(restrictiveness) == i + 1)

    write_rds(restrictiveness, fp)
    write_csv(restrictiveness, fp_csv)
  }
}

message("Done.")

# check_autocorrelation <- function(samples, max_lag = 50, plot_dims = 5) {
#   # Load necessary package
#   if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
#   if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)

#   library(ggplot2)
#   library(gridExtra)

#   num_dims <- ncol(samples) # Number of dimensions
#   num_samples <- nrow(samples) # Number of samples

#   acf_results <- list()
#   plots <- list()

#   for (dim in 1:num_dims) {
#     # Compute autocorrelation for each dimension
#     acf_result <- acf(samples[, dim], lag.max = max_lag, plot = FALSE)
#     acf_results[[dim]] <- data.frame(
#       Lag = acf_result$lag,
#       ACF = acf_result$acf,
#       Dimension = paste("Dim", dim)
#     )

#     # Plot only first 'plot_dims' dimensions
#     if (dim <= plot_dims) {
#       p <- ggplot(acf_results[[dim]], aes(x = Lag, y = ACF)) +
#         geom_line() +
#         geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#         labs(title = paste("ACF - Dimension", dim), x = "Lag", y = "Autocorrelation") +
#         theme_minimal()
#       plots[[dim]] <- p
#     }
#   }

#   # Display plots (up to `plot_dims` dimensions)
#   grid.arrange(grobs = plots, ncol = min(2, plot_dims))

#   return(acf_results)
# }

# # Example usage with your samples (assuming 'samples' is a matrix of size N x d)
# # Replace 'samples' with your actual matrix
# # acf_results <- check_autocorrelation(samples)

# compute_ess <- function(samples, max_lag = 50) {
#   # Load necessary package
#   if (!require("coda")) install.packages("coda", dependencies = TRUE)
#   library(coda)

#   num_dims <- ncol(samples) # Number of dimensions
#   ess_values <- numeric(num_dims) # Store ESS for each dimension

#   for (dim in 1:num_dims) {
#     # Convert to MCMC object for ESS calculation
#     ess_values[dim] <- effectiveSize(samples[, dim])
#   }

#   # Print summary
#   print(summary(ess_values))

#   return(ess_values)
# }
