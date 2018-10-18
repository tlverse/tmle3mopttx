##############################
#Simple binary simulation 1b
##############################

library(devtools)
library(data.table)

#Static rule is the optimal rule
Qbar0 <- function(A, W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  W4 <- W[, 4]
  Qbar <- ifelse(W4 > -4, plogis(A/2 * abs(W1)/2 * abs(W2)/2), plogis(-2*A))
  return(Qbar)
}

g0 <- function(W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  W4 <- W[, 4]
  
  plogis(0.25 * W1 - 0.1 * W2)
}

gen_data <- function(n = 1000, p = 4) {
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
data_full <- gen_data(1000000, 4)
data <- gen_data(1000, 4)
data<-data[,1:6]
mean(data_full$Yd0)
mean(data_full$Y1)
mean(data_full$Y0)
table(data_full$d0)

test_static_better<-data

rm(data_full, psi, g0,gen_data,Qbar0,data)
devtools::use_data(test_static_better)
