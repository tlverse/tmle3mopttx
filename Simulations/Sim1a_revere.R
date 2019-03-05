#Simulation with Binary A (Sim1a): Observational Study

#Rule dependent on single covariate
#~50% A=1 and ~50% A=0 allocation is the optimal rule
#When W4>0, optimal A=1
#When W4<0, optimal A=0

library(tmle3mopttx)
library(sl3)
library(tmle3)
library(data.table)

Qbar0 <- function(A, W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  W4 <- W[, 4]
  #When W4>0, optimal A=1
  #When W4<0, optimal A=0
  Qbar <- ifelse(W4 > 0, plogis(7 * A), plogis(-5*A))
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
data_tda <- gen_data(100000, 4)
data_tda <- data_tda[,1:6]
psi<-mean(data_full$Yd0)
mean(data_full$Y1)
mean(data_full$Y0)
table(data_full$d0)

# Define sl3 library and metalearners:

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

chain_fun <- function(learner_fit, task){
  X <- task$X  
  X[,W4_bin:=W4>0]
  X[,W4_bin_int:=W4_bin*A]
  data <- data.table(X,Y=task$Y)
  new_task <- make_sl3_Task(data, covariates=c("W4_bin","A","W4_bin_int"),outcome="Y",folds=task$folds)  
}

true_basis <- make_learner(Custom_chain,make_learner(Lrnr_mean), chain_fun)
true_para <- make_learner(Pipeline,true_basis, make_learner(Lrnr_glm_fast))

Q_learner <- Lrnr_sl$new(
  #learners = list(xgboost_100,xgboost_500,xgboost_1000,lrn2),
  learners = list(xgboost_50,xgboost_100,xgboost_500,
                  lrn1,lrn2,xgboost_1000),
  # learners = list(lrn1,lrn2,true_para),
  metalearner = Lrnr_nnls$new()
)

g_learner <- Lrnr_sl$new(
  learners = list(lrn2),
  #learners = list(xgboost_100,xgboost_500,glmnet_0.2,glmnet_0.8,lrn1,lrn2),
  metalearner = Lrnr_nnls$new()
)

b_learner <- Lrnr_sl$new(
  #learners = list(xgboost_100,xgboost_500,xgboost_1000,lrn2),
  learners = list(xgboost_100,lrn2,xgboost_1000),
  metalearner = Lrnr_nnls$new()
)

learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)
nested_learner_list <- lapply(learner_list, function(learner)make_learner(Lrnr_cv,learner, full_fit=TRUE))

##########################
n=1000

sim <- function(i){
  message(i)  
  # sim_learner_list <- nested_learner_list
  sim_learner_list <- learner_list
  #Generate data:
  data <- gen_data(n, 4)
  
  # Define spec:
  tmle_spec <- tmle3_mopttx_blip_revere(
    V = c("W4"), type = "blip1",
    b_learner = sim_learner_list$B, maximize = TRUE, complex = TRUE
  )
  
  # Define nodes:
  node_list <- list(W = c("W1", "W2", "W3", "W4"), A = "A", Y = "Y")

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

# one_est <- sim(1)
# print(one_est)
MC=500
n=1000

#sl3_debug_mode()
library(future)
library(future.apply)
library(data.table)
library(foreach)
library(doMC)
i<-1
# plan(multiprocess, workers=16)
registerDoMC(16)
# sim_results <- future_lapply(seq_len(MC),sim)
sim_results <- foreach(iter=seq_len(MC),.errorhandling = "remove")%dopar%{
  return(sim(iter))
}
results <-rbindlist(sim_results)
save(results, file="sim1a_results.rdata")
