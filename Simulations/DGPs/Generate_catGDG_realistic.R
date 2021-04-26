# Data-generating distribution for the categorical example:
library(devtools)
library(data.table)

normalize_rows <- function(x) {
  sweep(x, 1, rowSums(x), "/")
}

vals_from_factor <- function(x) {
  # lev <- levels(as.factor(x)) factor(lev, levels = lev)
  sort(unique(x))
}

Qbar0 <- function(A, W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  W4 <- W[, 4]
  Qbar <- (1 / 2) * (plogis(15 * (A == 1) * (1 * W1 - 0.5) - 3 * (A == 2) *
    (2 * W1 + 0.5) + 3 * (A == 3) * (3 * W1 - 0.5)) + plogis(W2 * W1))
  return(Qbar)
}

g0 <- function(W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  W4 <- W[, 4]

  scale_factor <- 0.8
  A1 <- 0.05 * plogis(scale_factor * W1)
  A2 <- plogis(scale_factor * W1)
  A3 <- plogis(scale_factor * W1)
  A <- cbind(A1, A2, A3)

  A <- normalize_rows(A)
}


gen_data <- function(n = 1000, p = 4) {
  W <- matrix(rnorm(n * p), nrow = n)
  colnames(W) <- paste("W", seq_len(p), sep = "")
  g0W <- g0(W)
  # A <- factor(apply(g0W, 1, function(pAi) which(rmultinom(1, 1, pAi) == 1)))
  A <- as.numeric(apply(g0W, 1, function(pAi) which(rmultinom(1, 1, pAi) == 1)))
  A_vals <- vals_from_factor(A)

  u <- runif(n)
  Y <- as.numeric(u < Qbar0(A, W))
  Q0aW <- sapply(A_vals, Qbar0, W)
  d0 <- max.col(Q0aW)
  Yd0 <- as.numeric(u < Qbar0(d0, W))
  df <- data.frame(W, A, Y, d0, Yd0)

  df$g0W <- g0(W)
  df$Q0aW <- Q0aW

  return(df)
}

set.seed(11)
data_cat_realistic <- gen_data(1000, 4)
data_cat_realistic <- data.table(data_cat_realistic[, 1:6])
rm(vals_from_factor, Qbar0, normalize_rows, gen_data, g0)
devtools::use_data(data_cat_realistic, overwrite = TRUE)
