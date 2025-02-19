# -----------------------------------------------------------------------------
# Bronars test for GARP violations given a dataset
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To run: Rscript --vanilla --verbose garp_violations.R >log/garp_violations.log 2>&1 # nolint
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

set.seed(214)

# Functions -------------------------------------------------------------------
sample_bundle <- function(x_intercept, x_price, y_price) {
  x_units <- runif(1, 0, 1) * x_intercept
  y_units <- (100 - x_units * x_price) / y_price

  return(list(x_units = x_units, y_units = y_units))
}

# Load data -------------------------------------------------------------------
message("Loading as-if problems...")
asif <- read_csv(here("data", "as_if_budgets.csv"))[1:10, ]
k <- nrow(asif)

# Run Simulations -------------------------------------------------------------
message("Simulating choice bundles...")
run_simulation <- function(data = asif, nsim = 1000) {
  # message("Num sims: ", nsim)
  p <- data %>%
    select(x_price, y_price) %>%
    as.matrix()
  cceis <- c()

  for (i in seq_len(nsim)) {
    sim_q <- data %>%
      mutate(
        result = pmap(.l = list(x_intercept, x_price, y_price), sample_bundle),
        x_unit = map(result, ~ .$x_units) %>% unlist(),
        y_unit = map(result, ~ .$y_units) %>% unlist()
      )

    q <- sim_q %>%
      select(x_unit, y_unit) %>%
      as.matrix()
    cceis <- c(cceis, ccei(p, q))
  }

  return(cceis)
}

# Create dataset to learn mapping from set -> bronar test ----------------------
message("Creating dataset for Bronars test...")
nsims <- 200
bronars_fp <- file.path("output", glue("bronars_data_k{k}_nsim{nsims}.rds"))

if (file.exists(bronars_fp)) {
  bronars_data <- read_rds(bronars_fp)
} else {
  bronars_data <- as.data.frame(matrix(0, ncol = k * 2 + 2, nrow = 1))
  names(bronars_data) <- c(
    glue("x_{1:k}"), glue("y_{1:k}"), "mean_ccei", "ccei_95"
  )

  cceis <- run_simulation(asif, nsim = nsims)

  print(summary(cceis))
  message("95th percentile CCEI: ", quantile(cceis, 0.95))
  write_rds(cceis, file.path("output", "cceis_asif.rds"))

  bronars_data[1, ] <- c(
    asif$x_intercept, asif$y_intercept, mean(cceis), quantile(cceis, 0.95)
  )
}

# Alternative dataset ---------------------------------------------------------
for (i in 1:2000) {
  if (nrow(bronars_data) >= i + 1) {
    next
  }
  message("Generating alternative dataset ", i, "...")
  alt <- asif %>%
    mutate(
      x_intercept = runif(n(), 1, 100) %>% as.integer(),
      y_intercept = runif(n(), 1, 100) %>% as.integer(),
      x_price = 100 / x_intercept,
      y_price = 100 / y_intercept
    )

  cceis_alt <- run_simulation(alt, nsim = nsims)
  new_row <- c(
    alt$x_intercept, alt$y_intercept, mean(cceis_alt), quantile(cceis_alt, 0.95)
  ) %>% as.matrix(nrow = 1, ncol = 62)
  # FIX THIS!!!!!
  bronars_data[i + 1, ] <- new_row

  assert(nrow(bronars_data) == i + 1)

  write_rds(bronars_data, bronars_fp)
  write_csv(bronars_data, file.path("output", "bronars_data.csv"))
}

message("Done.")
