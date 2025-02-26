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

# Functions -------------------------------------------------------------------
choice_significance <- function(theory_df, num_menus) {
  designs <- powerSet(theory_df$menu_id) %>%
    purrr::keep(~ length(.x) == num_menus)
  menu_sizes <- list()
  for (i in seq_len(length(designs))) {
    choice_possibilities <- c()
    for (j in seq_len(num_menus)) {
      choice_possibilities[j] <- theory_df$menu_size[[designs[[i]][j]]]
    }
    menu_sizes[[i]] <- choice_possibilities
  }
  signif_df <- data.frame(
    design_id = seq_len(length(designs))
  )
  signif_df$design <- designs
  signif_df$menu_sizes <- menu_sizes
  signif_df$num_choices <- sapply(signif_df$menu_sizes, prod)

  # For each design, get all possible choice combos
  possible_choices <- list()
  for (i in seq_len(nrow(signif_df))) {
    design_i <- signif_df$design[[i]]
    menus <- theory_df %>%
      filter(menu_id %in% design_i)

    possible_i_grid <- expand.grid(menus$menu, stringsAsFactors = FALSE)
    possible_i_li <- purrr::pmap(possible_i_grid, ~ c(...))
    possible_choices[[i]] <- possible_i_li
  }

  signif_df$possible_choices <- possible_choices
  signif_expanded <- signif_df %>%
    unnest(cols = possible_choices) %>%
    mutate(significance = NA)

  # Calculate significance of each choice combo
  for (i in seq_len(nrow(signif_expanded))) {
    design_i <- signif_expanded$design_id[i]
    choice_i <- signif_expanded$possible_choices[[i]] %>%
      as.character()

    theory_choices <- theory_df %>%
      filter(menu_id %in% signif_df$design[[design_i]]) %>%
      select(starts_with("f_")) %>%
      as.vector()

    matches <- map_lgl(theory_choices, ~ identical(.x, choice_i))
    signif_expanded$significance[i] <- mean(matches)
  }

  return(signif_expanded)
}

# Items -----------------------------------------------------------------------
k <- 3
options <- glue("x_{1:k}")
menus <- powerSet(options)

filtered_menus <- purrr::keep(menus, ~ length(.x) > 1)
message("Number of menus: ", length(filtered_menus))

menu_df <- data.frame(
  menu_id = seq_len(length(filtered_menus)), menu = NA
)
menu_df$menu <- filtered_menus
menu_df$menu_size <- sapply(menu_df$menu, length)

# Prediction rule f = choice from each menu; theory = set of prediction rules

# WARP ------------------------------------------------------------------------
# Possible preference orderings
preferences <- permutations(length(options), length(options), options)
warp_menus <- menu_df
for (i in seq_len(dim(preferences)[1])) {
  choices_i <- c()
  preferences_i <- preferences[i, ]
  for (j in seq_len(nrow(warp_menus))) {
    menu_j <- warp_menus$menu[[j]]
    # choose most preferred item in menu_j, i.e. first in preferences_i
    choice_j <- preferences_i[preferences_i %in% menu_j][1]
    choices_i <- c(choices_i, choice_j)
  }
  warp_menus[[glue("f_0{i}")]] <- choices_i
}

# Alternative theories --------------------------------------------------------
# Rational preferences binary, always choose middle option in 3-menu
compromise_menus <- menu_df
for (i in seq_len(dim(preferences)[1])) {
  choices_i <- c()
  preferences_i <- preferences[i, ]
  for (j in seq_len(nrow(compromise_menus))) {
    menu_j <- compromise_menus$menu[[j]]
    # for 2-menus choose most preferred item in menu_j
    choice_j <- preferences_i[preferences_i %in% menu_j][1]
    # for 3-menus choose second-most preferred item in menu_j
    if (length(menu_j) == 3) {
      choice_j <- preferences_i[preferences_i %in% menu_j][2]
    }
    choices_i <- c(choices_i, choice_j)
  }
  compromise_menus[[glue("f_a{i}")]] <- choices_i
}

# Significance tests ----------------------------------------------------------
# Given a theory, need to calculate the significance of each possible choice
choice_significance <- function(theory_df) {
  signif_df <- theory_df %>%
    select(menu_id, menu, menu_size)

  expanded_df <- signif_df %>%
    mutate(
      choice = purrr::map(menu, ~.x),
      significance = NA
    ) %>%
    unnest(cols = choice) %>%
    arrange(menu_id)

  for (i in seq_len(nrow(expanded_df))) {
    menu_i <- expanded_df$menu_id[i]
    choice_i <- expanded_df$choice[i]

    theory_choices <- theory_df %>%
      filter(menu_id == menu_i) %>%
      select(starts_with("f_")) %>%
      as.vector()

    expanded_df$significance[i] <- mean(theory_choices == choice_i)
  }

  return(expanded_df)
}

expanded_df <- choice_significance(warp_menus)

# Function to generate the desired dataset for num_menus
generate_combinations <- function(expanded_df, num_menus) {
  # Select the first `num_menus` menus from the dataset
  selected_menus <- expanded_df %>%
    filter(menu_id %in% 1:num_menus)

  # Generate combinations of choices from the selected menus
  choice_combinations <- expand.grid(
    lapply(1:num_menus, function(i) {
      selected_menus %>%
        filter(menu_id == i) %>%
        pull(choice)
    })
  )

  # Add menu_id information for each combination
  choice_combinations <- choice_combinations %>%
    mutate(menu_id_comb = purrr::map2(1:num_menus, .[1:num_menus], ~ paste0("menu_", .x, ": ", .y)))

  # Return the resulting dataset with all combinations
  return(choice_combinations)
}

# Example usage for num_menus = 2
num_menus <- 2
result_df <- generate_combinations(expanded_df, num_menus)
