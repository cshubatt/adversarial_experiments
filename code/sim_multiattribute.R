# -----------------------------------------------------------------------------
# Generate multi-attribute problems + model predictions
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To Run: Rscript --vanilla --verbose sim_multiattribute.R >log/sim_multiattribute.log 2>&1 # nolint
# -----------------------------------------------------------------------------

# Load libraries
renv::load(here::here())

library(dplyr) # %>%
library(purrr) # map2
library(here)
library(testit) # assert()
library(readr) # read_rds()
library(glue)
library(ggplot2)
library(data.table) # setnames()

h <- modules::use(here("lib", "value_functions.R"))

# Generate multi-attribute problems -------------------------------------------
gen_option <- function(min = 0, max = 20) {
  option <- c(
    "a" = runif(1, min, max) %>% round(2),
    "b" = runif(1, min, max) %>% round(2)
  )

  return(option)
}

# Simulate models -------------------------------------------------------------
# For each model, generate choice rates for x and y from the 2-menu {x, y}
# and choice rates for x, y, and z from the 3-menu {x, y, z}
message("Generating problems...")
nprobs <- 5000
set.seed(123)

df <- data.frame(
  menu = 1:nprobs,
  drop = TRUE
)
df$x <- map(1:nprobs, ~ gen_option())
df$y <- map(1:nprobs, ~ gen_option())
df$z <- map(1:nprobs, ~ gen_option())

df <- df %>%
  select(-drop) %>%
  mutate(
    disadvantage__xy = map2(x, y, ~ h$disadvantage(unlist(.x), unlist(.y))) %>%
      unlist(),
    disadvantage__yx = map2(y, x, ~ h$disadvantage(unlist(.x), unlist(.y))) %>%
      unlist()
  ) %>%
  filter(disadvantage__xy > 0 & disadvantage__yx > 0)

# True value function ---------------------------------------------------------
message("Calculating true value...")
v_x_2 <- map(df$x, ~h$val_option(.x)) %>% unlist()
v_y_2 <- map(df$y, ~h$val_option(.x)) %>% unlist()
v_z_3 <- map(df$z, ~h$val_option(.x)) %>% unlist()

df$choose_x_2 <- v_x_2 > v_y_2
df$v_x_2 <- v_x_2
df$v_y_2 <- v_y_2
df$v_z_3 <- v_z_3

# Tversky Simonson (1993) -----------------------------------------------------
message("Simulating Tversky and Simonson...")
thetas <- c(200, 100, 20)
for (theta in thetas) {
  v_x_2 <- map2(df$x, df$y, ~ h$val_option_ts(.x, .y, theta = theta)) %>% unlist()
  v_y_2 <- map2(df$y, df$x, ~ h$val_option_ts(.x, .y)) %>% unlist()
  v_x_3 <- pmap(
    .l = list(x = df$x, y = df$y, z = df$z), .f = h$val_option_ts, theta = theta
    ) %>%
    unlist()
  v_y_3 <- pmap(
    .l = list(x = df$y, y = df$x, z = df$z), .f = h$val_option_ts, theta = theta
    ) %>%
    unlist()
  v_z_3 <- pmap(
    .l = list(x = df$z, y = df$x, z = df$y), .f = h$val_option_ts, theta = theta
    ) %>%
    unlist()

  df$choose_x_2__TS <- v_x_2 > v_y_2
  df$choose_x_3__TS <- v_x_3 > v_y_3
  df <- df %>%
    mutate(
      consistent_2__TS = choose_x_2 == choose_x_2__TS,
      consistent_3__TS = choose_x_2 == choose_x_3__TS,
      reversal__TS = choose_x_2__TS != choose_x_3__TS
    )

  message("Frac 2-menu correct: ", mean(df$consistent_2__TS))
  message("Frac 3-menu correct: ", mean(df$consistent_3__TS))
  message("Frac reversal: ", mean(df$reversal__TS))

  df <- df %>%
    setnames(
      c(
        "choose_x_2__TS", "choose_x_3__TS", "consistent_2__TS", "consistent_3__TS",
        "reversal__TS"
      ),
      c(
        glue("choose_x_2__TS_{theta}"), glue("choose_x_3__TS_{theta}"),
        glue("consistent_2__TS_{theta}"), glue("consistent_3__TS_{theta}"),
        glue("reversal__TS_{theta}")
      )
    )
}

# reversal_problems <- filter(df, reversal__TS)

# BGS Salience (2013) ---------------------------------------------------------
message("Simulating BGS Salience...")
deltas <- c(0.3, 0.5, 0.8, 0.95)
for(delta in deltas) {
  message("Delta: ", delta)
  v_x_2 <- map2(df$x, df$y, ~ h$val_option_bgs(.x, .y, delta = delta)) %>% unlist()
  v_y_2 <- map2(df$y, df$x, ~ h$val_option_bgs(.x, .y)) %>% unlist()
  v_x_3 <- pmap(.l = list(x = df$x, y = df$y, z = df$z, delta = delta), .f = h$val_option_bgs) %>%
    unlist()
  v_y_3 <- pmap(.l = list(x = df$y, y = df$x, z = df$z, delta = delta), .f = h$val_option_bgs) %>%
    unlist()
  v_z_3 <- pmap(.l = list(x = df$z, y = df$x, z = df$y, delta = delta), .f = h$val_option_bgs) %>%
    unlist()

  df$choose_x_2__BGS <- v_x_2 > v_y_2
  df$choose_x_3__BGS <- v_x_3 > v_y_3

  df <- df %>%
    mutate(
      consistent_2__BGS = choose_x_2 == choose_x_2__BGS,
      consistent_3__BGS = choose_x_2 == choose_x_3__BGS,
      reversal__BGS = choose_x_2__BGS != choose_x_3__BGS
    )

  message("Frac 2-menu correct: ", mean(df$consistent_2__BGS))
  message("Frac 3-menu correct: ", mean(df$consistent_3__BGS))
  message(
    "Frac 2-menu and 3-menu incorrect: ", 
    mean((!df$consistent_2__BGS) & (!df$consistent_3__BGS))
    )
  message("Frac reversal: ", mean(df$reversal__BGS))

  df <- df %>%
  setnames(
    c(
      "choose_x_2__BGS", "choose_x_3__BGS", "consistent_2__BGS", "consistent_3__BGS",
      "reversal__BGS"
    ),
    c(
      glue("choose_x_2__BGS_{delta}"), glue("choose_x_3__BGS_{delta}"),
      glue("consistent_2__BGS_{delta}"), glue("consistent_3__BGS_{delta}"),
      glue("reversal__BGS_{delta}")
    )
  )
}

# it seems like bgs_which_2 and bgs_which_3 are strictly increasing sets
# salience get more extreme (delta shrinks), sets strictly gain elements
# bgs_which_2 <- df$menu[df$consistent_2__BGS == FALSE]
# bgs_which_3 <- df$menu[df$consistent_3__BGS == FALSE]
# bgs_both_wrong <- intersect(bgs_which_2, bgs_which_3)
# bgs_reversals <- df$menu[df$reversal__BGS == TRUE]

# Comparison Complexity -------------------------------------------------------
message("Simulating values from comparison complexity model...")
gammas <- c(0.5, 1, 2.5)
for (gamma in gammas){
  message("Gamma: ", gamma)
  cprobs_2 <- map2(
    df$x, df$y, ~ h$choice_prob_cc(.x, .y, gamma = gamma)
  ) %>% unlist(FALSE)
  cprobs_2_df <- as.data.frame(do.call(rbind, cprobs_2)) %>%
    rename(p_x_2__CC = x, p_y_2__CC = y)

  cprobs_3 <- pmap(
    .l = list(x = df$x, y = df$y, z = df$z),
    .f = h$choice_prob_cc, gamma = gamma
    ) %>%
    unlist(FALSE)
  cprobs_3_df <- as.data.frame(do.call(rbind, cprobs_3)) %>%
    rename(p_x_3__CC = x, p_y_3__CC = y, p_z_3__CC = z)

  cprobs_3_phantom <- pmap(
    .l = list(x = df$x, y = df$y, z = df$z),
    .f = h$choice_prob_cc, z_phantom = TRUE, gamma = gamma
    ) %>%
    unlist(FALSE)
  cprobs_3_phantom_df <- as.data.frame(do.call(rbind, cprobs_3_phantom)) %>%
    rename(p_x_3_phantom__CC = x, p_y_3_phantom__CC = y)

  df <- df %>% cbind(cprobs_2_df, cprobs_3_df, cprobs_3_phantom_df) %>%
    mutate(
      choose_x_2__CC = p_x_2__CC > p_y_2__CC,
      # this coding is weird because could also include z -- this is parallel to other models
      choose_x_3__CC = p_x_3__CC > p_y_3__CC,
      choose_x_3_phantom__CC = p_x_3_phantom__CC > p_y_3_phantom__CC,
      consistent_2__CC = choose_x_2 == choose_x_2__CC,
      consistent_3__CC = choose_x_2 == choose_x_3__CC,
      consistent_3_phantom__CC = choose_x_2 == choose_x_3_phantom__CC,
      reversal__CC = choose_x_2__CC != choose_x_3__CC,
      reversal_phantom__CC = choose_x_2__CC != choose_x_3_phantom__CC
    )

  message("Frac 2-menu correct: ", mean(df$consistent_2__CC))
  message("Frac 3-menu correct: ", mean(df$consistent_3__CC))
  message(
    "Frac 2-menu and 3-menu incorrect: ", 
    mean((!df$consistent_2__CC) & (!df$consistent_3__CC))
    )
  message("Frac 3-menu phantom correct: ", mean(df$consistent_3_phantom__CC))
  message("Frac reversal: ", mean(df$reversal__CC))
  message("Frac reversal phantom: ", mean(df$reversal_phantom__CC))

  df <- df %>%
  setnames(
    c(
      "p_y_2__CC", "p_x_2__CC", "p_x_3__CC", "p_y_3__CC", "p_z_3__CC",
      "p_x_3_phantom__CC", "p_y_3_phantom__CC",
      "choose_x_2__CC", "choose_x_3__CC", "choose_x_3_phantom__CC",
      "consistent_2__CC", "consistent_3__CC", "consistent_3_phantom__CC",
      "reversal__CC", "reversal_phantom__CC"
    ),
    c(
      glue("p_y_2__CC_{gamma}"), glue("p_x_2__CC_{gamma}"),
      glue("p_x_3__CC_{gamma}"), glue("p_y_3__CC_{gamma}"), glue("p_z_3__CC_{gamma}"),
      glue("p_x_3_phantom__CC_{gamma}"), glue("p_y_3_phantom__CC_{gamma}"),
      glue("choose_x_2__CC_{gamma}"), glue("choose_x_3__CC_{gamma}"),
      glue("choose_x_3_phantom__CC_{gamma}"),
      glue("consistent_2__CC_{gamma}"), glue("consistent_3__CC_{gamma}"),
      glue("consistent_3_phantom__CC_{gamma}"),
      glue("reversal__CC_{gamma}"), glue("reversal_phantom__CC_{gamma}")
    )
  )
}

# Save data -------------------------------------------------------------------
message("Saving data...")
write_rds(df, file.path("output", "multi_simulations.rds"))

# I think it should be the case that as gamma further from 1, 
# set of inconsistencies in the 2-menu + phantom option strictly increases?
# Might not be quite true depending on sampling since this is simulated...

# cc_which_3 <- df$menu[df$consistent_3_phantom__CC == FALSE]

# Outcomes to look at:
# Does the DM usually choose the correct option in the 2-menu?
# - For ST, BGS, is value ordering correct?
# - For CC, are choice probabilities ordered correctly?
# When z is added as a phantom, does the DM choose the correct option in the 2-menu?
# - For ST, BGS; is the value ordering of x-y correct within x-y-z?
# - For CC, are choice probabilities ordered correclty for x-y with z phantom?

# Compare models --------------------------------------------------------------
message("Comparing models...")
consistent_vars <- c(
  glue("consistent_3__TS_{thetas}"), glue("consistent_3__BGS_{deltas}"),
  glue("consistent_3_phantom__CC_{gammas}")
)
cor(
  df %>%
  select(all_of(consistent_vars))
  )

df <- df %>%
  mutate(
    any_inconsistent = rowSums(select(., all_of(consistent_vars)) == FALSE) > 0,
    any_inconsistent_TS = rowSums(select(., starts_with("consistent_3__TS"))) == 0,
    any_inconsistent_BGS = rowSums(select(., starts_with("consistent_3__BGS"))) == 0,
    any_inconsistent_CC = rowSums(select(., starts_with("consistent_3_phantom__CC"))) == 0
  )

incons_df <- filter(df, any_inconsistent)
cor(incons_df %>% select(any_inconsistent_TS, any_inconsistent_BGS, any_inconsistent_CC))

# print out a problem
print_menus <- function(df_row) {
  message("menu: ", df_row$menu)
  message("correct option: ", ifelse(df_row$choose_x_2, "x", "y"))
  message("value difference: ", df_row$v_x_2 - df_row$v_y_2)
  paste0(c("x:", unlist(df_row$x))) %>% print
  paste(c("y:", unlist(df_row$y))) %>% print
  paste(c("z:", unlist(df_row$z))) %>% print

  # parameter values should be ordered from most to least distortion
  ts_consistent <- df_row %>%
    select(starts_with("consistent_3__TS")) %>%
    unlist()
  paste(c("TS consistent: ", ts_consistent)) %>% print

  bgs_consistent <- df_row %>%
    select(starts_with("consistent_3__BGS")) %>%
    unlist()
  paste(c("BGS consistent: ", bgs_consistent)) %>% print

  cc_consistent <- df_row %>%
    select(starts_with("consistent_3_phantom__CC")) %>%
    unlist()
  paste(c("CC consistent: ", cc_consistent)) %>% print

  return(invisible())
}

for (i in 1:20) {
  print_menus(incons_df[i, ])
}

bgs_cc <- filter(df, xor(any_inconsistent_BGS, any_inconsistent_CC))
bgs_ts <- filter(df, xor(any_inconsistent_BGS, any_inconsistent_TS))
cc_ts <- filter(df, xor(any_inconsistent_CC, any_inconsistent_TS))

message("CC, BGS conflict: ")
for (i in 1:10) {
  print_menus(bgs_cc[i, ])
}

message("BGS, TS conflict: ")
for (i in 1:10) {
  print_menus(bgs_ts[i, ])
}

message("CC, TS conflict: ")
for (i in 1:10) {
  print_menus(cc_ts[i, ])
}
