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
library(ggplot2)
library(doParallel)
library(foreach)
library(parallel)

set.seed(214)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
print(args)
k <- as.numeric(sub("k=", "", args[grep("k=", args)]))[1]
nsims <- as.numeric(sub("nsims=", "", args[grep("nsims=", args)]))
optset_size <- as.numeric(
  sub("optset_size=", "", args[grep("optset_size=", args)])
)
# y_var command line args
y_var <- as.character(
  sub("y_var=", "", args[grep("y_var=", args)])
)

# k <- 8
# nsims <- 200
# optset_size <- 8
# y_var <- "ccei_e"

r <- 100 # Example value, replace with the actual value of r
e <- 0.95 # allowable e for GARP(e)

message(
  "k = ", k, ", nsims = ", nsims,
  "optset_size = ", optset_size,
  ", y_var = ", y_var,
  ", r = ", r,
  ", e = ", e
)

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

plot_budgets <- function(x_values, y_values) {
  # Create a data frame with the x and y values
  data <- data.frame(x = x_values, y = y_values)

  # Create a ggplot object
  p <- ggplot(data, aes(x = x, y = y)) +
    labs(
      x = "payment to self", y = "payment to other"
    ) +
    theme_bw()

  # Add lines for each budget
  for (i in 1:nrow(data)) {
    p <- p + geom_segment(
      x = 0, y = data$y[i], xend = data$x[i], yend = 0,
      color = "black"
    )
  }


  p <- p + scale_x_continuous(expand = c(0, 0), limits = c(0, r * 1.1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, r * 1.1))


  return(p)
}

# Load data -------------------------------------------------------------------
message("Loading optimized problems...")
mean_cceis <- c()
ccei_e <- c()
frac_nonviol <- c()

in_dir <- file.path(
  glue("data_k{k}_nsim{nsims}_{y_var}"),
  glue("optimized_points_{optset_size}")
)
save_fp <- file.path(in_dir, "opt_sets_cceis.csv")
message("SAVE FP: ")
print(save_fp)
problems_df <- as.data.frame(matrix(0, ncol = 2 * optset_size + 3, nrow = 1))
names(problems_df) <- c(
  glue("x_{1:optset_size}"), glue("y_{1:optset_size}"),
  "mean_ccei", "ccei_e", "frac_nonviol"
)

if (file.exists(save_fp)) {
  message("Mean CCEIs already calculated, skipping.")
  problems_df <- read_csv(save_fp, show_col_types = FALSE)
  pred_variable <- problems_df[[y_var]]
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
    ccei_e <- c(ccei_e, mean(opt_cceis > e))
    frac_nonviol <- c(frac_nonviol, mean(opt_cceis == 1))

    # Row 1 of dataset = base data
    if (i == 1) {
      problems_df[1, ] <- c(
        opt_set$x_intercept, opt_set$y_intercept,
        mean(opt_cceis), mean(opt_cceis > e), mean(opt_cceis == 1)
      )
    } else {
      problems_df <- rbind(
        problems_df,
        c(
          opt_set$x_intercept, opt_set$y_intercept,
          mean(opt_cceis), mean(opt_cceis > e), mean(opt_cceis == 1)
        )
      )
    }
  }
  write_csv(problems_df, save_fp)
}

message("Mean CCEIs for optimized problem collections:")
print(summary(problems_df$mean_ccei))
message("Frac nonviol for optimized problem collections:")
print(summary(problems_df$frac_nonviol))
message("Frac CCEI > e:")
print(summary(problems_df$ccei_e))

predictions <- read_csv(
  file.path(in_dir, "predictions.csv"),
  show_col_types = FALSE
)
# High correlation indicates that the DeepSet fits well on the space
# discovered by the optimizer
message(
  "Correlation between predictions, mean CCEIs: ",
  cor(predictions$predictions, problems_df$mean_ccei)
)
message(
  "Correlation between predictions, frac nonviol: ",
  cor(predictions$predictions, problems_df$frac_nonviol)
)
message(
  "Correlation between predictions, frac CCEI > e: ",
  cor(predictions$predictions, problems_df$ccei_e)
)

# For comparison, print summary stats on mean CCEI for randomly drawn sets
# If this distribution is close to optimized, indicates
# optimizer does not find sets much better than random
message(glue("{y_var} for random problem collections size 10:"))
train_dir <- file.path(
  glue("data_k{k}_nsim{nsims}_{y_var}")
)
train <- read_csv(file.path(train_dir, "train.csv"), show_col_types = FALSE)
print(summary(train[[y_var]][1:50]))

message(glue("{y_var} for random problem collections size 5:"))
test <- read_csv(file.path(train_dir, "test.csv"), show_col_types = FALSE)
print(summary(test[[y_var]][1:50]))

# Plot best designed budget sets -----------------------------------------------
message("Plotting best designed budget sets...")
best_row <- which(problems_df[[y_var]] == min(problems_df[[y_var]]))
best_design <- problems_df[best_row, ]
# Extract x and y values
x_values <- best_design %>%
  select(starts_with("x_")) %>%
  unlist()
y_values <- best_design %>%
  select(starts_with("y_")) %>%
  unlist()

# Plot the budget lines
gg <- plot_budgets(x_values, y_values)
ggsave(
  file.path(in_dir, "budget_lines.png"),
  gg + labs(title = "Selection Procedure"),
  width = 7, height = 7, units = "in", dpi = 300
)

base_row <- train[1, ]
x_values_base <- base_row %>%
  select(starts_with("x_")) %>%
  unlist()
y_values_base <- base_row %>%
  select(starts_with("y_")) %>%
  unlist()
gg_base <- plot_budgets(x_values_base, y_values_base)
ggsave(
  file.path(in_dir, "budget_lines_base.png"),
  gg_base + labs(title = "Andreoni and Miller (2003)"),
  width = 7, height = 7, units = "in", dpi = 300
)

message("Done.")
