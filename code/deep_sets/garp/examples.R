# -----------------------------------------------------------------------------
# Examples of discrete choice functions and power-maximizing designs
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To run: Rscript --vanilla --verbose examples.R >log/examples.log 2>&1 # nolint
# -----------------------------------------------------------------------------

# Load libraries
renv::load(here::here())

library(dplyr) # %>%
library(purrr) # map2
library(here)
library(testit) # assert()
library(glue)
library(readr)
library(rje) # powerSet
library(gtools) # permutations
library(tidyr) # unnest

set.seed(214)

relax_ratio <- 0.95

# analytical function for getting viol prob with HA uniform, e = relax_ratio
get_viol_prob <- function(p) {
  p_xa <- p[1]
  # p_y / p_ya
  p_y_ratio <- p[2]
  # assert
  stopifnot(p_xa - 1 * 1 / p_y_ratio > 0)
  # p_x = 1
  prob_viol_0 <- (relax_ratio - 1 / p_y_ratio) * (p_xa - 1 / p_y_ratio)^(-1)
  prob_viol_a <- (
    1 / p_xa - (relax_ratio - p_y_ratio) * (1 - p_xa * p_y_ratio)^(-1)
  ) * p_xa
  prob_viol <- prob_viol_0 * prob_viol_a

  return(-prob_viol)
}

starting <- c(p_xa = 2, p_y_ratio = 2)
opt <- optimx(starting, get_viol_prob)
print(opt)
