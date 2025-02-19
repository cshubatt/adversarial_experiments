renv::load(here::here())

library(dplyr)
library(readr)
library(haven)
library(purrr) # map2
library(ggplot2)
library(here)
library(stargazer)

# Load data -------------------------------------------------------------------
message("Loading choice data...")
cu <- read_dta(here("data", "risk", "Risky_problems.dta")) %>%
  select(problem, n, certainty, ln_time, compound)
pet <- read_dta(here("data", "risk", "Peterson_problems.dta")) %>%
  select(problem, n, frac_inconsistent)
ratio <- read_rds(here("data", "risk", "cdf_problems.rds")) %>%
  select(problem, cdf_ratio, rate_wrong)
nstates <- read_rds(here("data", "risk", "choice_problems.rds")) %>%
  select(problem, nstates__a, nstates__b)

cu <- cu %>% left_join(ratio) %>% left_join(nstates)
pet <- pet %>% left_join(ratio) %>% left_join(nstates)

# certainty
fit1 <- lm(certainty ~ cdf_ratio, data = cu)
print(summary(fit1))

fit2 <- lm(certainty ~ cdf_ratio + nstates__a + nstates__b, data = cu)
print(summary(fit2))

save_tbl <- stargazer(
  fit1, fit2, title = "Certainty",
  type = "latex", # Specify LaTeX output
  float = FALSE, # Do not float the table
  header = FALSE, # Exclude the default table header
  report = "vcs*", # Use asterisks for significance stars
  star.cutoffs = c(0.05, 0.01, 0.001),
  covariate.labels = c("CDF Ratio", "# States A", "# States B")
)

write(save_tbl, file.path("output", "certainty.tex"))

fit <- lm(certainty ~ cdf_ratio + compound, data = cu)
print(summary(fit))

fit <- lm(certainty ~ cdf_ratio + nstates__a + nstates__b + compound, data = cu)
print(summary(fit))

# response times
fit1 <- lm(ln_time ~ cdf_ratio, data = cu)
print(summary(fit1))

fit2 <- lm(ln_time ~ cdf_ratio + nstates__a + nstates__b, data = cu)
print(summary(fit2))

save_tbl <- stargazer(
  fit1, fit2, title = "Response Times",
  type = "latex", # Specify LaTeX output
  float = FALSE, # Do not float the table
  header = FALSE, # Exclude the default table header
  report = "vcs*", # Use asterisks for significance stars
  star.cutoffs = c(0.05, 0.01, 0.001),
  covariate.labels = c("CDF Ratio", "# States A", "# States B")
)
write(save_tbl, file.path("output", "response_times.tex"))

fit <- lm(ln_time ~ cdf_ratio + compound, data = cu)
print(summary(fit))

fit <- lm(ln_time ~ cdf_ratio + nstates__a + nstates__b + compound, data = cu)
print(summary(fit))

# rate wrong
fit <- lm(rate_wrong ~ cdf_ratio, data = cu)
print(summary(fit))

fit <- lm(rate_wrong ~ cdf_ratio + nstates__a + nstates__b, data = cu)
print(summary(fit))

fit <- lm(rate_wrong ~ certainty * nstates__a, data = cu)
print(summary(fit))

states_summary <- cu %>%
  group_by(nstates__a) %>%
  summarize(
    n = n(),
    rate_wrong = mean(rate_wrong),
    uncertainty = mean(1 - certainty),
    ln_time = mean(ln_time),
    cdf_ratio = mean(cdf_ratio)
  ) %>%
  ungroup

# inconsistent
fit <- lm(frac_inconsistent ~ cdf_ratio, data = pet)
print(summary(fit))

fit <- lm(frac_inconsistent ~ cdf_ratio + nstates__a + nstates__b, data = pet)
print(summary(fit))
