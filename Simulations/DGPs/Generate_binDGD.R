# Data-generating distribution for the binary example:
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
  Qbar <- (1 / 2) * (plogis(-5 * (A == 1) * (W1 - 0.5) + 5 * (A == 0) * (W1 - 0.5)) + 0.5 * plogis(W2 * W3))
  return(Qbar)
}

g0 <- function(W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]

  scale_factor <- 0.8
  A <- plogis(scale_factor * W1)
}

gen_data <- function(n = 1000, p = 3) {
  W <- matrix(rnorm(n * p), nrow = n)
  colnames(W) <- paste("W", seq_len(p), sep = "")
  A <- rbinom(n, 1, g0(W))

  u <- runif(n)
  Y <- as.numeric(u < Qbar0(A, W))
  Y0 <- as.numeric(u < Qbar0(0, W))
  Y1 <- as.numeric(u < Qbar0(1, W))
  d0 <- as.numeric(Qbar0(1, W) > Qbar0(0, W))
  Yd0 <- as.numeric(u < Qbar0(d0, W))

  data.frame(W, A, Y, Y0, Y1, Yd0, d0, blip = Qbar0(1, W) - Qbar0(0, W))
}

set.seed(11)
data_bin <- gen_data(1000, 3)
data_bin <- data.table(data_bin[, 1:5])
rm(vals_from_factor, Qbar0, normalize_rows, gen_data, g0)
devtools::use_data(data_bin)
