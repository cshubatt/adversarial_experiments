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
library(optimx)
library(hitandrun)

set.seed(214)

x <- runif(1000, 0, 1)
y <- x + 0.1

# e_x <- mean(x)
# e_y <- mean(y)
d <- function(z) {
  return(z^0.1)
}

e_x <- mean(x)
e_y <- mean(y)

e_d_x <- mean(d(x))
e_d_y <- mean(d(y))

restr <- e_x / e_y
message("Restrictiveness: ", restr)

restr_d <- e_d_x / e_d_y
message("Restrictiveness (d): ", restr_d)
