#Data-generating distribution for the categorical example:
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
  Qbar <- (1/2) * (plogis(-5 * (A == 2) * (W1 + 0.5) + 5 * (A == 3) * (W1 - 0.5)) + plogis(W2 * W3))
  return(Qbar)
}

g0 <- function(W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  W4 <- W[, 4]
  
  # rep(0.5, nrow(W))
  scale_factor <- 0.8
  A1 <- plogis(scale_factor * W1)
  A2 <- plogis(scale_factor * W2)
  A3 <- plogis(scale_factor * W3)
  A <- cbind(A1, A2, A3)
  
  # make sure A sums to 1
  A <- normalize_rows(A)
}

gen_data <- function(n = 1000, p = 4) {
  W <- matrix(rnorm(n * p), nrow = n)
  colnames(W) <- paste("W", seq_len(p), sep = "")
  g0W <- g0(W)
  A <- factor(apply(g0W, 1, function(pAi) which(rmultinom(1, 1, pAi) == 1)))
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
testdata <- gen_data(1e+05, 5)
test_vim_cat_data <- gen_data(1000, 5)
test_vim_cat_data<-data.table(test_vim_cat_data[,1:7])
rm(vals_from_factor,Qbar0,normalize_rows,gen_data,g0,testdata)
devtools::use_data(test_vim_cat_data)

