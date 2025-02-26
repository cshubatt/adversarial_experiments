# -----------------------------------------------------------------------------
# Bronars test for GARP violations given a dataset
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To run: Rscript --vanilla --verbose generate_violations.R k=10 nsims=200 test_k=5 >log/generate_violations.log 2>&1 # nolint
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
library(doParallel)
library(foreach)
library(parallel)

global_seed <- 214
set.seed(global_seed)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
k <- as.numeric(sub("k=", "", args[grep("k=", args)]))[1]
nsims <- as.numeric(sub("nsims=", "", args[grep("nsims=", args)]))
test_k <- as.numeric(sub("test_k=", "", args[grep("test_k=", args)]))
y_var <- as.character(
  sub("y_var=", "", args[grep("y_var=", args)])
)

# k <- 8
# nsims <- 200
# test_k <- 5
# y_var <- "frac_viol"

r <- 100 # Example value, replace with the actual value of r
e <- 0.95 # allowable e for GARP(e)

message("k = ", k, ", nsims = ", nsims, ", test_k = ", test_k, ", r = ", r)

# Functions -------------------------------------------------------------------
run_simulation <- function(data = base, nsim = 200) {
  p <- data %>%
    select(x_price, y_price) %>%
    as.matrix()

  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)

  # Use foreach with %dopar% to parallelize the loop
  cceis <- foreach(
    i = seq_len(nsim), .combine = c,
    .packages = c("dplyr", "revpref", "purrr")
  ) %dopar% {
    sample_bundle <- function(x_intercept, x_price, y_price) {
      x_units <- runif(1, 0, 1) * x_intercept
      y_units <- (100 - x_units * x_price) / y_price

      return(list(x_units = x_units, y_units = y_units))
    }

    sim_q <- data %>%
      mutate(
        result = pmap(.l = list(x_intercept, x_price, y_price), sample_bundle),
        x_unit = map(result, ~ .$x_units) %>% unlist(),
        y_unit = map(result, ~ .$y_units) %>% unlist()
      )

    q <- sim_q %>%
      select(x_unit, y_unit) %>%
      as.matrix()

    ccei(p, q) # Compute the CCEI for this simulation
  }

  # Shut down the cluster after the computation is done
  stopCluster(cl)

  return(cceis)
}

# Load data -------------------------------------------------------------------
message("Loading as-if problems...")
base_all <- read_csv(here("data", "am_budgets.csv"))
base <- base_all[1:k, ]

message("Base CCEI (all):")
ccei_all <- run_simulation(base_all, nsim = nsims)
print(summary(ccei_all))

message("Base CCEI (k):")
ccei_k <- run_simulation(base, nsim = nsims)
print(summary(ccei_k))
message("Frac expected CCEI > e:")
print(mean(ccei_k > e))

# Alternative dataset ---------------------------------------------------------
message("Constructing alternative sets of problems, Bronars test...")
splits <- list(
  train = list(split_k = k, split_n = 2^11),
  valid = list(split_k = k, split_n = 256),
  test = list(split_k = test_k, split_n = 256)
)
# splits <- list(
#   train = list(split_k = k, split_n = 2),
#   valid = list(split_k = k, split_n = 2),
#   test = list(split_k = test_k, split_n = 2)
# )

for (split in names(splits)) {
  message("Generating ", split, " data...")
  split_k <- splits[[split]]$split_k
  split_n <- splits[[split]]$split_n

  odir <- glue("data_k{k}_nsim{nsims}_{y_var}")
  fp <- file.path(odir, glue("{split}.rds"))
  fp_csv <- file.path(odir, glue("{split}.csv"))

  base_split <- base[1:split_k, ]

  if (file.exists(fp)) {
    bronars_data <- read_rds(fp)
  } else {
    bronars_data <- as.data.frame(matrix(0, ncol = split_k * 2 + 3, nrow = 1))
    names(bronars_data) <- c(
      glue("x_{1:split_k}"), glue("y_{1:split_k}"), "mean_ccei",
      "ccei_e", "frac_nonviol"
    )

    cceis <- run_simulation(base_split, nsim = nsims)

    # Row 1 of dataset = As If data
    bronars_data[1, ] <- c(
      base_split$x_intercept, base_split$y_intercept,
      mean(cceis), mean(cceis > e), mean(cceis == 1)
    )
  }

  for (i in seq_len(split_n)) {
    if (nrow(bronars_data) >= i + 1) {
      next
    }
    if (i %% 10 == 0) {
      message("Generating alternative dataset ", i, "...")
    }

    alt_data <- base_split %>%
      mutate(
        x_intercept = runif(n(), 1, r),
        y_intercept = runif(n(), 1, r),
        x_price = 100 / x_intercept,
        y_price = 100 / y_intercept
      )

    cceis_alt <- run_simulation(alt_data, nsim = nsims)

    bronars_data <- rbind(
      bronars_data,
      c(
        alt_data$x_intercept, alt_data$y_intercept,
        mean(cceis_alt), mean(cceis_alt > 0.95),
        mean(cceis_alt == 1)
      )
    )
    assert(nrow(bronars_data) == i + 1)

    write_rds(bronars_data, fp)
    write_csv(bronars_data, fp_csv)
  }
}

message("Done.")
