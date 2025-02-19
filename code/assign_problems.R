# -----------------------------------------------------------------------------
# Learn mapping from problems to modles
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To Run: Rscript --vanilla --verbose assign_problems.R >log/assign_problems.log 2>&1 # nolint
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

# Load data -------------------------------------------------------------------
message("Loading choice data...")
predictions <- read_csv(here("data", "risk", "structural_estimates.csv")) %>%
  rename(problem = prob_id) %>%
  select(problem, rate_a, rate_a_eu, rate_a_cpt, rate_a_cdf_eu, rate_a_cdf_eu)

all_choices <- read_rds(here("data", "risk", "choice_problems.rds")) %>%
  select(-rate_a) %>%
  left_join(predictions) %>%
  filter(!is.na(rate_a)) %>%
  mutate(
    eu_loss = abs(rate_a - rate_a_cpt),
    cdf_loss = abs(rate_a - rate_a_cdf_eu),
    cdf_advantage = eu_loss - cdf_loss,
    # Both models good when this is low
    max_error = pmax(eu_loss, cdf_loss),
    # Both models bad when this is high
    min_error = pmin(eu_loss, cdf_loss)
  )

check <- filter(all_choices, is.na(rate_a_eu))
message("CPT Loss:")
print(summary(all_choices$eu_loss))

message("CDF Loss:")
print(summary(all_choices$cdf_loss))

message("CDF Advantage Over CPT:")
print(summary(all_choices$cdf_advantage))

message("Max Error:")
print(summary(all_choices$max_error))

message("Min Error:")
print(summary(all_choices$min_error))

peterson <- filter(all_choices, problem < 990000)

# Predict CDF advantage -------------------------------------------------------
message("Predicting CDF advantage...")
include_vars <- c(
  "abs_ev_diff__ab", "abs_ev_diff__ab_sq", "evil_probs__a",
  "mixed__a", "ln_range__a", "ln_scale__a", "var__a", "cdf_diff_abs__ab",
  "adb__a", "ln_var__a", "ln_entropy__a", "certain__b", "ln_nstates__a"
)
adv_eqn <- reformulate(include_vars, "cdf_advantage")
max_eqn <- reformulate(include_vars, "max_error")
min_eqn <- reformulate(include_vars, "min_error")
eu_eqn <- reformulate(include_vars, "eu_loss")
cdf_eqn <- reformulate(include_vars, "cdf_loss")

adv_fit <- lm(adv_eqn, data = peterson)
max_fit <- lm(max_eqn, data = peterson)
min_fit <- lm(min_eqn, data = peterson)
eu_fit <- lm(eu_eqn, data = peterson)
cdf_fit <- lm(cdf_eqn, data = peterson)

print(summary(adv_fit))
print(summary(max_fit))
print(summary(min_fit))
print(summary(eu_fit))
print(summary(cdf_fit))

enke <- filter(all_choices, problem >= 990000) %>%
  mutate(
    cdf_advantage_pred = predict(fit, .),
    max_error_pred = predict(max_fit, .),
    min_error_pred = predict(min_fit, .),
    eu_loss_pred = predict(eu_fit, .),
    cdf_loss_pred = predict(cdf_fit, .)
  )
peterson <- peterson %>%
  mutate(
    cdf_advantage_pred = predict(adv_fit, .),
    max_error_pred = predict(max_fit, .),
    min_error_pred = predict(min_fit, .),
    eu_loss_pred = predict(eu_fit, .),
    cdf_loss_pred = predict(cdf_fit, .)
  )

gg <- ggplot(peterson, aes(x = eu_loss_pred, y = cdf_loss_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(
    x = "CPT Loss",
    y = "CDF Loss",
    title = "CPT vs. CDF Loss Predictions"
  ) +
  xlim(0, 0.5) +
  ylim(0, 0.5) +
  theme_bw()
ggsave(file.path("output", "cpt_vs_cdf_loss.png"), gg)

problems_eu <- filter(enke, cdf_advantage_pred < 0)
problems_cdf <- filter(enke, cdf_advantage_pred > 0)
problems_poor <- filter(enke, min_error_pred > quantile(min_error_pred, 0.8))
problems_good <- filter(enke, max_error_pred < quantile(max_error_pred, 0.8))

eu_adv_r2s <- c()
cdf_adv_r2s <- c()
poor_r2s <- c()
good_r2s <- c()

for (mod in c("cpt", "cdf_eu")) {
  reg_eqn <- reformulate(glue("rate_a_{mod}"), "rate_a")
  eu_adv_fit <- lm(reg_eqn, data = problems_eu)
  cdf_adv_fit <- lm(reg_eqn, data = problems_cdf)
  poor_fit <- lm(reg_eqn, data = problems_poor)
  good_fit <- lm(reg_eqn, data = problems_good)

  eu_adv_r2s <- c(eu_adv_r2s, summary(eu_adv_fit)$r.squared)
  cdf_adv_r2s <- c(cdf_adv_r2s, summary(cdf_adv_fit)$r.squared)
  poor_r2s <- c(poor_r2s, summary(poor_fit)$r.squared)
  good_r2s <- c(good_r2s, summary(good_fit)$r.squared)
}

message(
  "CPT, CDF R2s on CPT Advantage Problems (N = ",
  nrow(problems_eu), "): "
)
print(eu_adv_r2s)

message(
  "CPT, CDF R2s on CDF Advantage Problems (N = ",
  nrow(problems_cdf), "): "
)
print(cdf_adv_r2s)

message(
  "CPT, CDF R2s on Poor Problems (N = ",
  nrow(problems_poor), "): "
)

print(poor_r2s)

message(
  "CPT, CDF R2s on Good Problems (N = ",
  nrow(problems_good), "): "
)
print(good_r2s)
