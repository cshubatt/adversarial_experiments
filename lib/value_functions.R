# -----------------------------------------------------------------------------
# MODULE: value functions for multiattribute options
# Author: Cassidy Shubatt <cshubatt@g.harvard.edu
# To Load: modules::use(here("lib", "value_functions.R"))
# -----------------------------------------------------------------------------
import(dplyr)
import(stats) # qnorm
import(gtools) # permutations
import(matrixStats) # colProds
import(testit) # assert

# Basic value function --------------------------------------------------------
val_option <- function(x) {
  x <- unlist(x)
  val <- sum(x)
  return(val)
}

# Simonson-Tversky (1993) -----------------------------------------------------
advantage_i <- function(x, y) {
  adv <- max(0, x - y)
  return(adv)
}
disadvantage_i <- function(x, y, lambda = 1) {
  disadv <- lambda * advantage_i(y, x)
  return(disadv)
}
advantage <- function(x, y) {
  x <- unlist(x)
  y <- unlist(y)

  adv <- 0
  for (i in 1:length(x)) {
    adv <- adv + advantage_i(x[i], y[i])
  }
  return(adv)
}
disadvantage <- function(x, y, lambda = 1) {
  disadv <- 0
  for (i in 1:length(x)) {
    disadv <- disadv + disadvantage_i(x[i], y[i], lambda)
  }
  return(disadv)
}

rel_advantage <- function(x, y, lambda = 1) {
  num <- advantage(x, y)
  denom <- num + disadvantage(x, y, lambda)
  rel_adv <- num / denom
  return(rel_adv)
}

val_option_ts <- function(x, y, z = NA, lambda = 3, theta = 100) {
  x <- unlist(x)
  y <- unlist(y)
  z <- unlist(z)

  val <- 0
  for (i in 1:length(x)) {
    val <- val + x[i]
  }
  rel_adv_y <- rel_advantage(x, y, lambda)
  val <- val + theta * rel_adv_y

  if (any(is.na(z))) {
    return(val)
  }

  rel_adv_z <- rel_advantage(x, z, lambda)

  val <- val + theta * (rel_adv_y + rel_adv_z)

  return(val)
}

# BGS Salience (2013) ---------------------------------------------------------
attr_salience <- function(a, abar) {
  abs(a - abar) / (a + abar)
}

salience_wt <- function(sal_a, sal_b, delta) {
  hi_wt <- 2 / (1 + delta)
  lo_wt <- 2 * delta / (1 + delta)

  if (sal_a > sal_b) {
    return(list(hi_wt, lo_wt))
  } else if (sal_a < sal_b) {
    return(list(lo_wt, hi_wt))
  } else {
    return(list(1, 1))
  }
}

val_option_bgs <- function(x, y, z = NA, delta = 0.3) {
  x <- unlist(x)
  y <- unlist(y)
  z <- unlist(z)

  asum <- x[1] + y[1]
  bsum <- x[2] + y[2]
  abar <- asum / 2
  bbar <- bsum / 2
  if (!any(is.na(z))) {
    asum <- asum + z[1]
    bsum <- bsum + z[2]
    abar <- abar + z[1] / 2
    bbar <- bbar + z[2] / 2
  }

  sal_a <- attr_salience(x[1], abar)
  sal_b <- attr_salience(x[2], bbar)

  wt <- salience_wt(sal_a, sal_b, delta) %>% unlist()
  val <- wt %*% x

  return(val)
}

# Comparison Complexity -------------------------------------------------------
comp_l1_f <- function(x, y, kappa, gamma, psi = 1, type = "g", sigma = 1) {
  # returns tau as a function of the attributes for two-attribute comparisons
  # x,y: vectors of attribute values
  # kappa: tremble parameter
  # gamma: curvature parameter at dominance
  # psi: curvature parameter at indifference

  if (!identical(x, y)) {
    r <- abs(sum(x - y)) / sum(abs(x - y))
  } else {
    r <- 0
  }

  comp <- h_trans(r, kappa, gamma, psi, type, sigma)
  return(comp)
}

h_trans <- function(r, kappa, gamma, psi = 1, type = "g", sigma = 1) {
  # returns tau as a function of the absolute value/similarity ratio
  #   corresponding to main parameterization of G()
  # kappa: tremble parameter
  # gamma: curvature parameter at dominance
  # psi: curvature parameter at indifference

  if (type == "g") {
    comp <- qnorm(struct_cprob(r, kappa, gamma, psi))^2
  } else if (type == "msg") {
    comp <- (sigma / 2) * qnorm(struct_cprob(r, kappa, gamma, psi))
  } else if (type == "bb") {
    comp <- 2 * struct_cprob(r, kappa, gamma, psi) - 1
  }

  return(comp)
}

struct_cprob <- function(ratio_signed, kappa, gamma, psi = 1) {
  ratio <- abs(ratio_signed)

  a_better <- ratio_signed >= 0
  prob_given_a <- (1 - kappa) - (0.5 - kappa) * ((1 - ratio)^gamma) / ((ratio^psi + (1 - ratio)^psi)^(1 / psi))
  prob_given_b <- kappa + (0.5 - kappa) * ((1 - ratio)^gamma) / ((ratio^psi + (1 - ratio)^psi)^(1 / psi))

  prob_choose_a <- ifelse(a_better, prob_given_a, prob_given_b)

  return(prob_choose_a)
}

taus_l1_f <- function(amat, kappa, gamma, psi = 1, type = "g", sigma = 1) {
  # returns vector of taus given matrix of attribute values
  # amat: matrix of attribute values
  #   rows: option
  #   columns: attribute
  # kappa: tremble parameter
  # gamma: curvature parameter at dominance
  # psi: curvature parameter at indifference

  n <- nrow(amat)
  index <- 1
  taus <- array()
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      taus[index] <- comp_l1_f(amat[i, ], amat[j, ], kappa, gamma, psi, type, sigma)
      index <- index + 1
    }
  }
  return(taus)
}

posts_f <- function(v, taus, nsim, sigs = NULL, type = "g", sigma = 1) {
  # simulate posteriors over each option corresponding to signal

  n <- length(v)
  permlist <- permutations(n, n, 1:n) # all possible orderings

  # if signals not supplied, generate them
  if (is.null(sigs)) {
    sigs <- sigs_f(v, taus, nsim, type, sigma)
  }

  problist <- problist_f(v, taus, nsim, sigs, type, sigma) # posterior prob of each ordering

  postlist <- array(0, dim = c(nsim, n)) # posterior value over each option
  for (i in 1:n) {
    # for each option
    for (j in 1:n) {
      # for each option ranking
      # add contribution to posterior from i being ranked jth
      perm_inds <- which(permlist[, i] == j) # orderings where i is ranked jth

      if (!identical(perm_inds, integer(0))) {
        # if there is an ordering where i is ranked jth:
        if (length(perm_inds) > 1) {
          # if there are multiple orderings where i is ranked jth:
          postlist[, i] <- (postlist[, i] + rowSums(problist[, perm_inds]) * (n - j + 1) / (n + 1))
        } else {
          # if there's only one ordering where i is ranked jth
          postlist[, i] <- (postlist[, i] + problist[, perm_inds] * (n - j + 1) / (n + 1))
        }
      }
    }
  }
  return(postlist)
}


problist_f <- function(v, taus, nsim, sigs = NULL, type = "g", sigma = 1) {
  # simulate posteriors over each ranking corresponding to signal

  n <- length(v)
  permlist <- permutations(n, n, 1:n) # all possible orderings
  complist <- array(NA, dim = c(nrow(permlist), choose(n, 2))) # ordering-implied comparisons
  index <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      complist[, index] <- sign(permlist[, j] - permlist[, i])
      index <- index + 1
    }
  }

  # if signals not supplied, generate them
  if (is.null(sigs)) {
    sigs <- sigs_f(v, taus, nsim, type, sigma)
  }

  problist <- array(NA, dim = c(nsim, nrow(permlist))) # posterior prob of each ordering

  if (type == "g") {
    if (length(taus) == 1) {
      # for binary choice, need to flatten arrays
      for (i in 1:nsim) {
        densmat <- dnorm((sigs[i] - t(complist)) * as.numeric(sqrt(taus)))
        densmat[is.na(densmat)] <- 1 # NA values occur due to infinite precision signals matching state; set to any non-zero value
        densmat[sigs[i] == 0 & taus == Inf] <- 1 # Set infinite precision signals equal to 0 to any non-zero value
        problist[i, ] <- colProds(densmat)
      }
    } else {
      for (i in 1:nsim) {
        densmat <- dnorm((sigs[i, ] - t(complist)) * sqrt(taus))
        densmat[is.na(densmat)] <- 1 # NA values occur due to infinite precision signals matching state; set to any non-zero value
        densmat[sigs[i, ] == 0 & taus == Inf, ] <- 1 # Set infinite precision signals equal to 0 to any non-zero value
        problist[i, ] <- colProds(densmat)
      }
    }
    problist <- ((problist) / rowSums(problist))
  } else if (type == "msg" | type == "bb") {
    if (length(taus) == 1) {
      # for binary choice, need to flatten arrays
      for (i in 1:nsim) {
        densmat <- (t(complist) == 1) * sigs[i] + (t(complist) == -1) * (1 - sigs[i])
        problist[i, ] <- colProds(densmat)
      }
    } else {
      for (i in 1:nsim) {
        densmat <- (t(complist) == 1) * sigs[i, ] + (t(complist) == -1) * (1 - sigs[i, ])
        problist[i, ] <- colProds(densmat)
      }
    }
    problist <- ((problist) / rowSums(problist))
  }

  return(problist)
}


sigs_f <- function(v, taus, nsim, type = "g", sigma = 1) {
  # generates signals
  # v: vector of values
  # taus: In order of (tau_{ij})_{i<j},
  #   e.g. (tau_12,tau_13,...,tau_1n, tau_23,tau_24,...,tau_2n,...tau_n-1n)
  # the ordering of signals in output will also use this convention
  # type: type of signal structure
  #   g: gaussian, msg: misspecified gaussian, bb: bang-bang
  # sigma: variance parameter for msg model

  n <- length(v)
  index <- 1
  comps <- array() # the comparisons implied by the values
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      comps[index] <- sign(v[i] - v[j])
      index <- index + 1
    }
  }

  if (type == "g") {
    sigs <- array(rnorm(nsim * length(comps)), dim = c(nsim, length(comps)))
    if (length(taus) == 1) {
      # for binary choice, need to flatten arrays
      sigs <- t(t(sigs) * as.numeric(1 / sqrt(taus)) + as.numeric(comps))
    } else {
      sigs <- t(t(sigs) * (1 / sqrt(taus)) + comps)
    }
    sigs[, taus == 0] <- 0 # set zero precision signals to arbitrary value
  } else if (type == "msg") {
    sigs_u <- array(runif(nsim * length(comps)), dim = c(nsim, length(comps)))
    if (length(taus) == 1) {
      # for binary choice, need to flatten arrays
      sigs <- t(exp(sigma * qnorm(t(sigs_u)) + 2 * as.numeric(taus) * as.numeric(comps)) /
        (1 + exp(sigma * qnorm(t(sigs_u)) + 2 * as.numeric(taus) * as.numeric(comps))))
      sigs[t(exp(sigma * qnorm(t(sigs_u)) + 2 * as.numeric(taus) * as.numeric(comps))) == Inf] <- 1
    } else {
      sigs <- t(exp(sigma * qnorm(t(sigs_u)) + 2 * taus * comps) /
        (1 + exp(sigma * qnorm(t(sigs_u)) + 2 * taus * comps)))
      sigs[t(exp(sigma * qnorm(t(sigs_u)) + 2 * taus * comps)) == Inf] <- 1
    }
  } else if (type == "bb") {
    sigs_u <- array(runif(nsim * length(comps)), dim = c(nsim, length(comps)))
    if (length(taus) == 1) {
      # for binary choice, need to flatten arrays
      sigs <- (t((t(sigs_u) < as.numeric(taus)) * as.numeric(comps)) + 1) / 2
    } else {
      sigs <- (t((t(sigs_u) < taus) * comps) + 1) / 2
    }
  }
  return(sigs)
}

rmax <- function(vec) {
  # returns the maximum index; randomizes in case of ties
  maxes <- which(vec == max(vec))
  if (length(maxes) == 1) {
    return(maxes)
  } else {
    return(sample(maxes, 1))
  }
}

choice_prob_cc <- function(x, y, z = NA, z_phantom = FALSE, nsim = 1000, kappa = 0, gamma = 2.36) {
  x <- unlist(x)
  y <- unlist(y)
  z <- unlist(z)
  set.seed(214)

  amat <- rbind(x, y)
  if (!any(is.na(z))) {
    amat <- rbind(amat, z)
  }
  v <- rowSums(amat)

  taus <- taus_l1_f(amat, kappa, gamma)
  postlist <- posts_f(v, taus, nsim)

  choices <- array()
  for (i in 1:nsim) {
    if (z_phantom) {
      choices[i] <- rmax(postlist[i, 1:2])
    } else {
      choices[i] <- rmax(postlist[i, ])
    }
  }

  choiceprobs <- c(mean(choices == 1), mean(choices == 2))
  names(choiceprobs) <- c("x", "y")
  if (!z_phantom & !any(is.na(z))) {
    choiceprobs <- c(choiceprobs, mean(choices == 3))
    names(choiceprobs) <- c("x", "y", "z")
  }

  return(list(choiceprobs))
}

# CPT functions --------------------------------------------------------------
sample_utility <- function(
    x, alpha = 1, beta = 1, lambda = 1) {
  # Input: x, a payoff
  # Output: utility according to util params alpha, beta, lambda
  utility <- ifelse(
    x > 0, x^alpha, -lambda * (-x)^beta
  )

  return(utility)
}


sample_pwf <- function(p_list, chi = 1, gamma = 1) {
  # Input: p_list, a list of all probabilities p
  # Output: weighted_prob, a vector of weighted probabilities from a fixed probability weighting function

  p <- unlist(p_list)
  weighted_prob <- (chi * p^gamma) / (chi * p^gamma + (1 - p)^gamma)
  return(weighted_prob)
}

lottery_value <- function(
    p_list, x_list, chi = 1, gamma = 1, alpha = 1, beta = 1, lambda = 1) {
  assert(
    "x_list and p_list same length",
    length(x_list) == length(p_list)
  )
  x <- sample_utility(x_list, alpha, beta, lambda)
  p <- sample_pwf(p_list, gamma, chi)
  value <- x %*% p

  return(value)
}

lottery_value_rdeu <- function(
    p_list, x_list, chi = 1, gamma = 1, alpha = 1, beta = 1, lambda = 1,
    type = "crra", reflexive = TRUE) {
  assert(
    "x_list and p_list same length",
    length(x_list) == length(p_list)
  )
  assert(
    "x_list contains same-signed payoffs",
    all(x_list >= 0) | all(x_list <= 0)
  )

  if (length(x_list) > 1) {
    x_ordered <- x_list[order(abs(x_list), decreasing = TRUE)]
    p_ordered_cum <- cumsum(p_list[order(abs(x_list), decreasing = TRUE)]) %>%
      pmin(1)

    ux_ordered <- sample_utility(
      x_ordered, alpha, beta, lambda
    )
    wp_ordered_cum <- sample_pwf(p_ordered_cum, chi, gamma)

    value <- (
      wp_ordered_cum - c(0, wp_ordered_cum[1:(length(wp_ordered_cum) - 1)])
    ) %*% ux_ordered
  } else {
    value <- sample_utility(x_list, alpha, beta, lambda)
  }
  return(value)
}

lottery_value_cpt <- function(
    p_list, x_list, chi = 1, gamma = 1, alpha = 1, beta = 1, lambda = 1,
    type = "crra") {
  assert(
    "x_list and p_list same length",
    length(x_list) == length(p_list)
  )

  if (any(x_list >= 0) & any(x_list < 0)) {
    x_pos <- x_list[x_list >= 0]
    p_pos <- p_list[x_list >= 0]
    x_neg <- x_list[x_list < 0]
    p_neg <- p_list[x_list < 0]

    value <- (
      lottery_value_rdeu(p_pos, x_pos, chi, gamma, alpha, beta, lambda, type) +
        lottery_value_rdeu(p_neg, x_neg, chi, gamma, alpha, beta, lambda, type)
    )
  } else {
    value <- lottery_value_rdeu(
      p_list, x_list, chi, gamma, alpha, beta, lambda, type
    )
  }

  return(value)
}
