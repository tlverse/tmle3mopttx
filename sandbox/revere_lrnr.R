#Simulation 1a: 
#Rule dependent on single covariate
#~50% A=1 and ~50% A=0 allocation is the optimal rule
#When W4>0, optimal A=1
#When W4<0, optimal A=0
setwd("~/Dropbox/tlverse/tmle3mopttx/")
library(devtools)
load_all()
library(tmle3mopttx)
library(sl3)
library(tmle3)
library(origami)
library(data.table)
tda_manual= function(opt, tda_task){
  
  #Define new task:
  tmle_task<-tda_task
  
  #Make working with opt easier:
  self<-opt
  private<-opt$.__enclos_env__$private
  
  #Get blip fits:
  blip_fits<-private$.blip_fits
  
  #Get estimated coefficients:
  fit_coef<-data.frame(self$coef_fits)
  
  #Define the blip task:
  blip_tmle_task <- make_sl3_Task(self$V_data(tmle_task),
                                  covariates = self$V,outcome = NULL)
  
  blip_fin <- matrix(nrow = nrow(blip_tmle_task$data), ncol = length(blip_fits[[1]]))
  
  for (j in 1:length(blip_fits[[1]])) {
    
    temp <- lapply(1:length(tmle_task$folds), function(v) {
      pred<-blip_fits[[v]][[j]]$fit_object$cv_fit$predict_fold(blip_tmle_task, fold_number = 1)
      pred[tmle_task$folds[[v]]$validation_set, ]
    })
    
    # Average over split-specific fits per algorithm
    blip_pred <- do.call(rbind, temp)
    pred <- data.frame(pred = as.matrix(blip_pred) %*% t(fit_coef))
    row.names(pred) <- unlist(lapply(1:length(tmle_task$folds), function(i) tmle_task$folds[[i]]$validation_set))
    pred <- pred[order(as.numeric(row.names(pred))), ]
    pred <- data.frame(pred)
    
  }
  
  if (length(blip_fits[[1]]) == 1) {
    rule <- as.numeric(pred$pred > 0)
  } else {
    # Combine all the sl validation samples:
    if (private$.maximize) {
      if (blip_type == "blip1") {
        rule <- max.col(blip_fin) + 1
      } else {
        rule <- max.col(blip_fin)
      }
    } else {
      if (blip_type == "blip1") {
        rule <- max.col(-1 * blip_fin) + 1
      } else {
        rule <- max.col(-1 * blip_fin)
      }
    }
  }
  return(rule)
}

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
data_tda <- gen_data(500000, 4)
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

Q_learner <- Lrnr_sl$new(
  #learners = list(xgboost_100,xgboost_500,xgboost_1000,lrn2),
  learners = list(xgboost_50,xgboost_100,xgboost_500,
                  lrn1,lrn2,xgboost_1000),
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

# Define spec:
tmle_spec <- tmle3_mopttx_blip(
  V = c("W4"), type = "blip1",
  b_learner = learner_list$B, maximize = TRUE, complex = TRUE
)

set.seed(1111)
# for(i in 1:MC){

#Generate data:
n <- 1000
data <- gen_data(n, 4)
node_list <- list(W = c("W1", "W2", "W3", "W4"), A = "A", Y = "Y")
tmle_task <- tmle_spec$make_tmle_task(data, node_list)
tda_task <- tmle_spec$make_tmle_task(data_tda, node_list)
likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)

revere_blip_task <- function(fold_number, tmle_task){
  # A_vals <- as.factor(tmle_task$npsem$A$variable_type$levels)
  A_vals <- tmle_task$npsem$A$variable_type$levels
  
  # Generate counterfactual tasks for each value of A:
  cf_tasks <- lapply(A_vals, function(A_val) {
    newdata <- data.table(A = A_val)
    cf_task <- tmle_task$generate_counterfactual_task(UUIDgenerate(), new_data = newdata)
    return(cf_task)
  })
  
  # DR A-IPW mapping of blip
  A <- tmle_task$get_tmle_node("A")
  Y <- tmle_task$get_tmle_node("Y")
  A_vals <- tmle_task$npsem$A$variable_type$levels
  A_ind <- Optimal_Rule$public_methods$factor_to_indicators(A,A_vals)
  Y_mat <- replicate(length(A_vals), Y)
  
  Q_vals <- sapply(cf_tasks, likelihood$get_likelihood, "Y", fold_number)
  g_vals <- sapply(cf_tasks, likelihood$get_likelihood, "A", fold_number)
  DR <- (A_ind / g_vals) * (Y_mat - Q_vals) + Q_vals
  
  # todo: port other blip code
  # todo: support multivariate outcome
  blip <- DR[,2] - DR[,1]
  V <- tmle_task$data[,tmle_spec$options$V,with=FALSE]
  data <- data.table(V,blip=blip)
  revere_task <- make_sl3_Task(data, outcome="blip",covariates=tmle_spec$options$V, folds=tmle_task$folds)
  
  return(revere_task)
}

revere_task_function <- revere_blip_task
blip_sl <- learner_list$B
blip_sl_stack <- make_learner(Stack,blip_sl$params$learners)
learner <- blip_sl_stack
fold <- tmle_task$folds[[1]]
train_revere_fit <- function(fold, revere_task_function, learner){
  fold_idx <- fold_index()
  #todo: generalize arguements
  revere_task <- revere_task_function(fold_idx, tmle_task, likelihood)
  
  revere_training_task <- training(revere_task)
  revere_fit <- learner$train(revere_training_task)
}

revere_fits <- lapply(tmle_task$folds,train_revere_fit,revere_task_function,learner)

train_revere_predict <- function(fold, revere_task_function, revere_fits){
  fold_idx <- fold_index()
  revere_fit <- revere_fits[[fold_idx]]
  revere_task <- revere_task_function(fold_idx, tmle_task, likelihood)
  preds <- revere_fit$predict(revere_task)
}

# do multinomial SL
# put this in tmle3 probably
# function factory for opttx revere to pass in likelihood
# data adaptive target parameter / revere
# what we need is essentially a revere_sl from a revere_cv
# scrub folds on subset in learner cv
# refactor cv_risk code

# revere sl
# based on previous sl's
# do something split specific

# new idea: set to lowest intervention level (population optimal treatment (where's sl?))
# generally the _parameter definition_ is split specific
# maybe you need an sl to define the parameter, but often you don't

# idea: simple c-tmle style screening - choose W* based on Q(A,W) in a split-specific way
# this would look like revere for g based on Q SL (or hal!)
# problem: revere learner needs tmle3_task (not SL task) use expects_tmle3_task
# how to pass in a likelihood? (or various other things)
# function factory???

# construct likelihood and t
revere_preds <- lapply(tmle_task$folds,train_revere_predict,revere_task_function,revere_fits)
revere_validation_preds <- lapply(tmle_task$folds,function(fold){validation(revere_preds[[fold_index()]])})
revere_pred_dt <- rbindlist(revere_validation_preds)
validation_index <- unlist(lapply(tmle_task$folds,function(fold)validation()))
revere_pred_dt <- revere_pred_dt[order(validation_index)]
revere_full_task <- revere_task_function(-1,tmle_task,likelihood)
revere_meta_data <- data.table(revere_pred_dt, Y__=revere_full_task$Y)
setnames(revere_meta_data,"Y__",revere_full_task$nodes$outcome)
cv_meta_task <- make_sl3_Task(revere_meta_data, outcome=revere_full_task$nodes$outcome, covariates=names(revere_pred_dt))
metalearner <- blip_sl$params$metalearner
metalearner_fit <- metalearner$train(cv_meta_task)
blip_preds <- metalearner_fit$predict()
tx <- as.numeric(blip_preds>0)
outcome <- Qbar0(tx,tmle_task$get_tmle_node("W"))
mean(outcome)

revere_full_fit <- learner$train(revere_full_task)
full_meta_task <- revere_full_fit$chain()
full_blip_preds <- metalearner_fit$predict(full_meta_task)
tx <- as.numeric(full_blip_preds>0)
outcome <- Qbar0(tx,tmle_task$get_tmle_node("W"))
mean(outcome)

revere_tda_task <- revere_task_function(-1,tda_task,likelihood)
tda_meta_task <- revere_full_fit$chain(revere_tda_task)
tda_blip_preds <- metalearner_fit$predict(tda_meta_task)
tx <- as.numeric(tda_blip_preds>0)
outcome <- Qbar0(tx,tda_task$get_tmle_node("W"))
mean(outcome)

revere_tda_task <- revere_task_function(1,tda_task,likelihood)
tda_meta_task <- revere_fits[[1]]$chain(revere_tda_task)
tda_blip_preds <- metalearner_fit$predict(tda_meta_task)
tx <- as.numeric(tda_blip_preds>0)
outcome <- Qbar0(tx,tda_task$get_tmle_node("W"))
mean(outcome)

#lrnr_revere_cv
revere_preds <- lapply()
lapply()
