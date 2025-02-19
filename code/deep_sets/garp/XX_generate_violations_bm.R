# -----------------------------------------------------------------------------
# Bronars test for GARP violations given a dataset
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To run: Rscript --vanilla --verbose XX_generate_violations_bm.R >log/XX_generate_violations_bm.log 2>&1 # nolint
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

k <- 10
test_k <- 5
y_var <- "mean_ccei"

nsims <- 100

message("k = ", k, ", nsims = ", nsims, ", test_k = ", test_k, ", r = ", r)

# Functions -------------------------------------------------------------------
sample_bundle <- function(x_intercept, x_price, y_price, budget = 100) {
  x_units <- runif(1, 0, 1) * x_intercept
  y_units <- (budget - x_units * x_price) / y_price

  return(list(x_units = x_units, y_units = y_units))
}

run_simulation <- function(data = asif, nsim = nsims, budget = 100) {
  p <- data %>%
    select(x_price, y_price) %>%
    as.matrix()
  cceis <- c()

  for (i in seq_len(nsim)) {
    sim_q <- data %>%
      mutate(
        result = pmap(
          .l = list(x_intercept, x_price, y_price),
          sample_bundle, budget = budget
        ),
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
asif <- read_csv(here("data", "as_if_budgets.csv"))[1:k, ]

# Alternative dataset ---------------------------------------------------------
message("Constructing alternative sets of problems, Bronars test...")
budget <- 10
max_unit <- 10

asif_split_b10_m10 <- asif %>%
  mutate(
    x_intercept = runif(n(), 1, max_unit),
    y_intercept = runif(n(), 1, max_unit),
    x_price = budget / x_intercept,
    y_price = budget / y_intercept
  )

asif_split_b10_m100 <- asif_split_b10_m10 %>%
  mutate(
    x_intercept = 10 * x_intercept,
    y_intercept = 10 * y_intercept,
    x_price = budget / x_intercept,
    y_price = budget / y_intercept
  )

asif_split_b100_m10 <- asif_split_b10_m10 %>%
  mutate(
    x_price = 100 / x_intercept,
    y_price = 100 / y_intercept
  )

asif_split_b100_m100 <- asif_split_b10_m10 %>%
  mutate(
    x_intercept = 10 * x_intercept,
    y_intercept = 10 * y_intercept,
    x_price = 100 / x_intercept,
    y_price = 100 / y_intercept
  )

# Run simulations ------------------------------------------------------------
message("Running simulations...")
cceis_b10_m10 <- run_simulation(asif_split_b10_m10, nsim = nsims, budget = 10)
cceis_b10_m100 <- run_simulation(asif_split_b10_m100, nsim = nsims, budget = 10)
cceis_b100_m10 <- run_simulation(
  asif_split_b100_m10,
  nsim = nsims, budget = 100
)
cceis_b100_m100 <- run_simulation(
  asif_split_b100_m100,
  nsim = nsims, budget = 100
)

message("Summary of CCEIs, budget 10 max units 10:")
print(summary(cceis_b10_m10))
message("Summary of CCEIs, budget 10 max units 100:")
print(summary(cceis_b10_m100))
message("Summary of CCEIs, budget 100 max units 10:")
print(summary(cceis_b100_m10))
message("Summary of CCEIs, budget 100 max units 100:")
print(summary(cceis_b100_m100))

# data <- asif_split_b10_m10

# data <- asif_split_b100_m10


# sum_tab <- data.frame(sim = 1:nsims, temp = rep(0, nsims))
# for (budget in budgets) {
#   for (max_unit in max_units) {
#     # message("Running sims for budget = ", budget, ", max_unit = ", max_unit)
#     cceis_bm <- c()
#     for (i in seq_len(nsims)) {
#       # if (i %% 10 == 0) {
#       #   message("Simulation ", i)
#       # }
#       asif_split <- asif %>%
#         mutate(
#           x_intercept = runif(n(), 1, max_unit),
#           y_intercept = runif(n(), 1, max_unit),
#           x_price = budget / x_intercept,
#           y_price = budget / y_intercept
#         )

#       cceis <- run_simulation(asif_split, nsim = nsims)

#       cceis_bm <- c(cceis_bm, mean(cceis))
#     }
#     message("Budget = ", budget, ", Max Units = ", max_unit, ":")
#     print(summary(cceis_bm))
#     sum_tab[[glue("mean_ccei_b{budget}_m{max_unit}")]] <- cceis_bm
#   }
# }

# write_csv(sum_tab, file.path("temp", "cceis_bm.csv"))

message("Done.")
