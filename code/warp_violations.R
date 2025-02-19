# -----------------------------------------------------------------------------
# Generate preferences with warp violations, try to find violation
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# -----------------------------------------------------------------------------

# Load libraries
renv::load(here::here())

library(dplyr) # %>%
library(purrr) # map2
library(here)
library(testit) # assert()
library(glue)
library(rje)
library(magrittr)

set.seed(214)

# Function ---------------------------------------------------------------------


# Items x_1 through x_n, with ~approximate~ preferences x_1 > x_2 > ... > x_n

# Generate choice from every sub-menu, with some mistakes
# Method 1: Correct with probability 1 - e, random with probability e
e <- 0.2
n <- 4
options <- 1:n
menus_raw <- powerSet(options, n)

menus <- list()
for (i in 1:length(menus_raw)) {
  menu <- menus_raw[[i]]
  if (length(menu) > 1) {
    menus <- c(menus, list(menu))
  }
}

# probability 
choice_structure_template <- data.frame(
  menus = I(menus),
  preferred = sapply(menus, min),
  rand = sapply(menus, function(x) sample(x, 1)),
  go_preferred = rbinom(length(menus), 1, 1 - e)
) %>%
  mutate(
    choice = ifelse(go_preferred == 1, preferred, rand),
    correct = ifelse(choice == preferred, 1, 0)
  )

# Strategies to search for WARP violation
# Goal: Find shortest path of menus to discover a WARP violation
# Assume do not inherently know anything about the true preference ordering;
# only known information is learned from previously observed menus

# If at first we do not consider adaptive search strategies, then here is what we could do.
# [2^n - (n+1)]! ways to order menus, but actually less than this once we treat all labelings as the same.
# to do this exhaustively, consider all possible orderings and get "avg length to WARP violation" for each ordering
# how to do this more efficiently?