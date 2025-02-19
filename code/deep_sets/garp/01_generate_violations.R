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

set.seed(214)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
k <- as.numeric(sub("k=", "", args[grep("k=", args)]))[1]
nsims <- as.numeric(sub("nsims=", "", args[grep("nsims=", args)]))
test_k <- as.numeric(sub("test_k=", "", args[grep("test_k=", args)]))
y_var <- as.character(
  sub("y_var=", "", args[grep("y_var=", args)])
)

# k <- 10
# nsims <- 200
# test_k <- 5
# y_var <- "frac_viol"

r <- 100 # Example value, replace with the actual value of r

message("k = ", k, ", nsims = ", nsims, ", test_k = ", test_k, ", r = ", r)

# Functions -------------------------------------------------------------------
sample_bundle <- function(x_intercept, x_price, y_price) {
  x_units <- runif(1, 0, 1) * x_intercept
  y_units <- (100 - x_units * x_price) / y_price

  return(list(x_units = x_units, y_units = y_units))
}

run_simulation <- function(data = asif, nsim = nsims) {
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

# Load data -------------------------------------------------------------------
message("Loading as-if problems...")
asif_all <- read_csv(here("data", "as_if_budgets.csv"))
asif <- asif_all[1:k, ]

message("As If CCEI (all):")
ccei_all <- run_simulation(asif_all, nsim = nsims)
print(summary(ccei_all))

message("As If CCEI (k):")
ccei_k <- run_simulation(asif, nsim = nsims)
print(summary(ccei_k))

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

  asif_split <- asif[1:split_k, ]

  if (file.exists(fp)) {
    bronars_data <- read_rds(fp)
  } else {
    bronars_data <- as.data.frame(matrix(0, ncol = split_k * 2 + 3, nrow = 1))
    names(bronars_data) <- c(
      glue("x_{1:split_k}"), glue("y_{1:split_k}"), "mean_ccei",
      "ccei_95", "frac_nonviol"
    )

    cceis <- run_simulation(asif_split, nsim = nsims)
    # message("As If CCEI distribution:")
    # print(summary(cceis))

    # Row 1 of dataset = As If data
    bronars_data[1, ] <- c(
      asif_split$x_intercept, asif_split$y_intercept,
      mean(cceis), quantile(cceis, 0.95), mean(cceis == 1)
    )
  }

  for (i in seq_len(split_n)) {
    if (nrow(bronars_data) >= i + 1) {
      next
    }
    if (i %% 10 == 0) {
      message("Generating alternative dataset ", i, "...")
    }

    alt_data <- asif_split %>%
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
        mean(cceis_alt), quantile(cceis_alt, 0.95),
        mean(cceis_alt < 1)
      )
    )
    assert(nrow(bronars_data) == i + 1)

    write_rds(bronars_data, fp)
    write_csv(bronars_data, fp_csv)
  }

  # Normalize columns x_1, ..., x_k and y_1, ..., y_k for deep net
  # if ((mean(abs(bronars_data$x_1 - mean(bronars_data$x_1))) < 2)) {
  #   message("Transforming data...")
  #   norm_data <- bronars_data %>%
  #     mutate_at(
  #       vars(starts_with("x_"), starts_with("y_")),
  #       ~(. - r / 2) / sqrt(r^2 / 12)
  #     )
  #   write_rds(norm_data, fp)
  #   write_csv(norm_data, fp_csv)
  # } else {
  #   message("Data already transformed.")
  # }
}

message("Done.")
