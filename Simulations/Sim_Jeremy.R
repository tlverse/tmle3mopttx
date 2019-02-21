#Simulation Jeremy: 
#Repeat the simulation performed by Jeremy in his dissertation
library(tmle3)
library(sl3)
library(devtools)
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

#Make multivariate learners for the blip:
#mv_xgboost_100 <- make_learner(Lrnr_multivariate, xgboost_100)
#mv_xgboost_1000 <- make_learner(Lrnr_multivariate, xgboost_1000)
#mv_glm <- make_learner(Lrnr_multivariate, lrn2)

b_learner <- Lrnr_sl$new(
  #learners = list(mv_xgboost_100),
  #learners = list(mv_xgboost_100,mv_glm,mv_xgboost_1000),
  learners = list(xgboost_100,lrn2,xgboost_1000),
  metalearner = Lrnr_nnls$new()
)

mv_b_learner <- make_learner(Lrnr_multivariate, b_learner)

learner_list <- list(Y = Q_learner, A = g_learner, B = mv_b_learner)

##################
n=1000

sim <- function(i){
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
  
  # use validation fit for psi + IC
  cv_estimates <- fit$tmle_params[[1]]$estimates(fit$tmle_task,"validation")
  cv_est <- summary_from_estimates(fit$tmle_task,list(cv_estimates),"cv-tsm","cv_tsm")
  
  # combine ests
  ests <- rbindlist(list(est, cv_est))
  ests$coverage <- as.numeric(ests$lower_transformed<=psi & ests$upper_transformed >= psi)
  
  #True data-adaptive parameter:
  opt<-tmle_spec$return_rule()
  
  tda_task <- tmle_spec$make_tmle_task(data_tda, node_list)
  tda_tx <- opt$rule(tda_task, "full")
  # tda_tx_bn <- sapply(1:10, function(fold_number)opt$rule(tda_task,fold_number))
  tda_W <- tda_task$get_tmle_node("W")
  Edn <- mean(Qbar0(tda_tx,as.matrix(tda_W)))
  # Edn_bn <- mean(apply(tda_tx_bn,2,Qbar0,tda_W))
  ests$Edn <- Edn
  # ests$Edn_bn <- Edn_bn
  ests$coverage_dn <- as.numeric(ests$lower_transformed<=Edn & ests$upper_transformed >= Edn)
  ests$psi <- psi
  ests$iteration <- i
  # ests$coverage_dn_bn <- as.numeric(ests$lower_transformed<=Edn_bn & ests$upper_transformed >= Edn_bn)
  names(ests)<-c("type","param","init_est","tmle_est","se","lower","upper",
                 "psi_transformed","lower_transformed","upper_transformed","coverage", 
                 "true data-adpative", "coverage data-adaptive","psi","iteration")
  
  results <- ests
  
  save(results, file=sprintf("sim1a_results/results%04d.rdata",i))
  return(results)
}


