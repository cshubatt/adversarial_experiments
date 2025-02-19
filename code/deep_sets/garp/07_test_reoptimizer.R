# -----------------------------------------------------------------------------
# Bronars test for GARP violations given a dataset
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# Rscript --vanilla --verbose 04_test_optimizer_result.R k=10 nsims=200 >data_k$(1)_nsims$(2)/outfiles/04_test_optimizer_result.log 2>&1
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

# k <- 10
# nsims <- 200
# optset_size <- 10

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
k <- as.numeric(sub("k=", "", args[grep("k=", args)]))[1]
nsims <- as.numeric(sub("nsims=", "", args[grep("nsims=", args)]))
optset_size <- as.numeric(
  sub("optset_size=", "", args[grep("optset_size=", args)])
)

r <- 100 # Example value, replace with the actual value of r

message(
  "k = ", k, ", nsims = ", nsims,
  "optset_size = ", optset_size, ", r = ", r
)

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
message("Loading optimized problems...")
mean_cceis <- c()
in_dir <- file.path(
  glue("data_k{k}_nsim{nsims}"),
  glue("optimized_points_{optset_size}"),
  "finetune"
)
save_fp <- file.path(in_dir, "opt_mean_cceis.csv")
problems_df <- as.data.frame(matrix(0, ncol = k * 2 + 2, nrow = 1))
names(problems_df) <- c(
  glue("x_{1:k}"), glue("y_{1:k}"), "mean_ccei", "ccei_95"
)

if (file.exists(save_fp)) {
  message("Mean CCEIs already calculated, skipping.")
  mean_cceis <- read_csv(save_fp)$mean_cceis
} else {
  message("Calculating mean CCEIs.")
  # Do this in a loop
  for (i in seq(0, 49)) {
    if (i %% 10 == 0) {
      message(glue("Optimized problem {i}"))
    }

    fn <- file.path(in_dir, glue("optimized_points_{i}.csv"))
    opt_set <- read_csv(fn, show_col_types = FALSE)
    opt_set <- opt_set %>%
      mutate(
        x_intercept = x,
        y_intercept = y,
        x_price = 100 / x_intercept,
        y_price = 100 / y_intercept
      )

    # Run simulation
    opt_cceis <- run_simulation(data = opt_set, nsim = nsims)
    mean_cceis <- c(mean_cceis, mean(opt_cceis))

    if (i == 1) {
      problems_df[1, ] <- c(
        opt_set$x_intercept, opt_set$y_intercept,
        mean(opt_cceis), quantile(opt_cceis, 0.95)
      )
    } else {
      problems_df <- rbind(
        problems_df,
        c(
          opt_set$x_intercept, opt_set$y_intercept,
          mean(opt_cceis), quantile(opt_cceis, 0.95)
        )
      )
    }
  }
}

write_csv(data.frame(mean_cceis), save_fp)
write_csv(problems_df, file.path(in_dir, "opt_sets_cceis.csv"))

message("Mean CCEIs for optimized problem collections:")
print(summary(mean_cceis))
predictions <- read_csv(
  file.path(in_dir, "predictions.csv"),
  show_col_types = FALSE
)
# High correlation indicates that the DeepSet fits well on the space
# discovered by the optimizer
message(
  "Correlation between predictions, mean CCEIs: ",
  cor(predictions$predictions, mean_cceis)
)

# For comparison, print summary stats on mean CCEI for randomly drawn sets
# If this distribution is close to optimized, indicates
# optimizer does not find sets much better than random
message("Mean CCEIS for random problem collections size 10:")
train_dir <- file.path(
  glue("data_k{k}_nsim{nsims}")
)
train <- read_csv(file.path(train_dir, "train.csv"), show_col_types = FALSE)
print(summary(train$mean_ccei[1:50]))

message("Mean CCEIs for random problem collections size 5:")
test <- read_csv(file.path(train_dir, "test.csv"), show_col_types = FALSE)
print(summary(test$mean_ccei[1:50]))

message("Done.")
