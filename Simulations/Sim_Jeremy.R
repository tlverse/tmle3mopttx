#Simulation Jeremy: 
#Repeat the simulation performed by Jeremy in his dissertation
library(tmle3)
library(sl3)
library(devtools)
library(here)
load_all()

Qbar0 <- function(A, W) {
  
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  Qbar <- (1/2) * (plogis(-5 * (A == 1) * (W1 - 0.5) + 5 * (A == 0) * (W1 - 0.5)) + 0.5*plogis(W2 * W3))
  return(Qbar)
}

g0 <- function(W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  
  # rep(0.5, nrow(W))
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
data_full <- gen_data(1000000, 3)
data_tda <- gen_data(50000, 3)
data_tda <- data_tda[,1:6]
psi<-mean(data_full$Yd0)
mean(data_full$Y1)
mean(data_full$Y0)
table(data_full$d0)

xgboost_50<-Lrnr_xgboost$new(nrounds = 50)
xgboost_100<-Lrnr_xgboost$new(nrounds = 100)
xgboost_500<-Lrnr_xgboost$new(nrounds = 500)
xgboost_1000<-Lrnr_xgboost$new(nrounds = 1000)
glmnet_0.2<-Lrnr_glmnet$new(alpha = 0.2, lambda = 300)
glmnet_0.4<-Lrnr_glmnet$new(alpha = 0.4, lambda = 300)
glmnet_0.6<-Lrnr_glmnet$new(alpha = 0.6, lambda = 300)
glmnet_0.8<-Lrnr_glmnet$new(alpha = 0.8, lambda = 300)

lrn1 <- Lrnr_mean$new()
lrn2<-Lrnr_glm_fast$new()
lrn3<-Lrnr_hal9001$new()

Q_learner <- Lrnr_sl$new(
  learners = list(xgboost_50,xgboost_100,xgboost_500,
                  lrn1,lrn2,xgboost_1000),
  metalearner = Lrnr_nnls$new()
)

g_learner <- Lrnr_sl$new(
  learners = list(lrn2),
  metalearner = Lrnr_nnls$new()
)

#Make SL of multivariate learners:
learners <- list(xgboost_50,xgboost_100,xgboost_500,lrn1,lrn2)
b_learner <- create_mv_learners(learners = learners)

learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

##################
sim <- function(i,n){
  message(i) 
  # sim_learner_list <- nested_learner_list
  sim_learner_list <- learner_list
  #Generate data:
  data <- gen_data(n, 3)
  
  # Define spec:
  tmle_spec <- tmle3_mopttx_blip_revere(
    V = c("W1","W2","W3"), type = "blip2",
    b_learner = sim_learner_list$B, maximize = TRUE, complex = TRUE
  )
  
  # Define nodes:
  node_list <- list(W = c("W1", "W2", "W3"), A = "A", Y = "Y")
  
  fit <- tmle3(tmle_spec, data=data, node_list=node_list, learner_list=sim_learner_list)
  est <- fit$summary
  
  #True parameter:
  est$psi <- psi
  est$coverage_d0 <- as.numeric(est$lower_transformed<=psi & est$upper_transformed >= psi)
  
  #True data-adaptive parameter:
  est$Edn <- tmle_spec$data_adapt_psi(data_tda, node_list, Qbar0)
  est$coverage_dn <- as.numeric(est$lower_transformed<=est$Edn & est$upper_transformed >= est$Edn)

  est$iteration <- i
  est$n <- n
  results <- est
  
  save(results, file=sprintf("Simulations/simJeremy_results/results%04d.rdata",i))
  return(results)
}

MC=2
n=1000

library(future)
library(future.apply)
library(data.table)
library(foreach)
library(doMC)
registerDoMC(16)

sim_results <- foreach(iter=seq_len(MC),.errorhandling = "remove")%dopar%{
  return(sim(iter,n=n))
}

results <-rbindlist(sim_results)
save(results, file="Simulations/simJeremy_results/simJeremy_results.rdata")
