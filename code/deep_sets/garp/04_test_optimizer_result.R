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

k <- 8
nsims <- 200
optset_size <- 8
y_var <- "mean_ccei"

r <- 100 # Example value, replace with the actual value of r

message(
  "k = ", k, ", nsims = ", nsims,
  "optset_size = ", optset_size,
  ", y_var = ", y_var,
  ", r = ", r
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
ccei_95 <- c()
frac_nonviol <- c()
ccei_under_95 <- c()

in_dir <- file.path(
  glue("data_k{k}_nsim{nsims}_{y_var}"),
  glue("optimized_points_{optset_size}")
)
save_fp <- file.path(in_dir, "opt_sets_cceis.csv")
message("SAVE FP: ")
print(save_fp)
problems_df <- as.data.frame(matrix(0, ncol = 2 * optset_size + 4, nrow = 1))
names(problems_df) <- c(
  glue("x_{1:optset_size}"), glue("y_{1:optset_size}"),
  "mean_ccei", "ccei_95", "frac_nonviol", "ccei_under_95"
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
        # x_intercept = (x * sqrt(r^2 / 12)) + r / 2,
        # y_intercept = (y * sqrt(r^2 / 12)) + r / 2,
        x_intercept = x,
        y_intercept = y,
        x_price = 100 / x_intercept,
        y_price = 100 / y_intercept
      )

    # Run simulation
    opt_cceis <- run_simulation(data = opt_set, nsim = nsims)
    mean_cceis <- c(mean_cceis, mean(opt_cceis))
    ccei_95 <- c(ccei_95, quantile(opt_cceis, 0.95))
    frac_nonviol <- c(frac_nonviol, mean(opt_cceis == 1))
    ccei_under_95 <- c(ccei_under_95, mean(opt_cceis < 0.95))

    # Row 1 of dataset = As If data
    if (i == 1) {
      problems_df[1, ] <- c(
        opt_set$x_intercept, opt_set$y_intercept,
        mean(opt_cceis), quantile(opt_cceis, 0.95), mean(opt_cceis == 1),
        mean(opt_cceis < 0.95)
      )
    } else {
      problems_df <- rbind(
        problems_df,
        c(
          opt_set$x_intercept, opt_set$y_intercept,
          mean(opt_cceis), quantile(opt_cceis, 0.95), mean(opt_cceis == 1),
          mean(opt_cceis < 0.95)
        )
      )
    }
  }
  write_csv(problems_df, save_fp)
}


# train_n <- 128
# train <- problems_df[1:train_n, ]
# valid <- problems_df[(train_n + 1):nrow(problems_df), ]

# # save train, valid
# write_csv(train, file.path(in_dir, "finetune_train.csv"))
# write_csv(valid, file.path(in_dir, "finetune_valid.csv"))

message("Mean CCEIs for optimized problem collections:")
print(summary(problems_df$mean_ccei))
message("Frac nonviol for optimized problem collections:")
print(summary(problems_df$frac_nonviol))
message("Frac CCEI < 0.95:")
print(summary(problems_df$ccei_under_95))

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

# For comparison, print summary stats on mean CCEI for randomly drawn sets
# If this distribution is close to optimized, indicates
# optimizer does not find sets much better than random
message("Mean CCEIS for random problem collections size 10:")
train_dir <- file.path(
  glue("data_k{k}_nsim{nsims}_{y_var}")
)
train <- read_csv(file.path(train_dir, "train.csv"), show_col_types = FALSE)
print(summary(train$mean_ccei[1:50]))

message("Mean CCEIs for random problem collections size 5:")
test <- read_csv(file.path(train_dir, "test.csv"), show_col_types = FALSE)
print(summary(test$mean_ccei[1:50]))

# Plot best designed budget sets -----------------------------------------------
message("Plotting best designed budget sets...")
best_row <- which(problems_df$mean_ccei == min(problems_df$mean_ccei))
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

x_values_AM <- c(120, 40, 120, 60, 150, 75, 60, 100) / 1.5
y_values_AM <- c(40, 120, 60, 120, 75, 150, 60, 100) / 1.5
gg_AM <- plot_budgets(x_values_AM, y_values_AM)
ggsave(
  file.path(in_dir, "budget_lines_AM.png"),
  gg_AM + labs(title = "Andreoni and Miller (2003)"),
  width = 7, height = 7, units = "in", dpi = 300
)

message("Done.")
