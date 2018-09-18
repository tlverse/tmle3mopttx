library(sl3)
library(origami)
library(tmle3)
library(data.table)
library(R6)
library(devtools)
library(uuid)

load_all()
MC<-500
n<-1000

#Functions necessary for the simulation:
normalize_rows <- function(x) {
  sweep(x, 1, rowSums(x), "/")
}

vals_from_factor <- function(x) {
  sort(unique(x))
}

Qbar0 <- function(A, W) {
  
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  
  Qbar <- 2/3 *(plogis(5 * (A == 2) + 2 * (A == 3) * (W1-0.2)) )
  return(Qbar)
}

g0 <- function(W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]

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

#Generate truth:
testdata <- gen_data(n=1e+06, p=3)
truthn<-mean(testdata$Yd0)

###########################
# sl3 specification
###########################

#Define nodes:
node_list <- list(
  W = c("W1","W2","W3","W4","W5"),
  A = "A",
  Y = "Y"
)

qlib <- make_learner_stack(
  "Lrnr_mean",
  "Lrnr_glmnet",
  "Lrnr_randomForest",
  "Lrnr_xgboost",
  "Lrnr_glm_fast",
  "Lrnr_nnls"
)

glib <- make_learner_stack(
  "Lrnr_mean",
  "Lrnr_glmnet",
  "Lrnr_randomForest",
  "Lrnr_xgboost"
)

blib <- make_learner_stack(
  "Lrnr_mean",
  "Lrnr_glmnet",
  "Lrnr_randomForest",
  "Lrnr_xgboost",
  "Lrnr_glm_fast",
  "Lrnr_nnls"
)

metalearner <- make_learner(Lrnr_nnls)
mn_metalearner <- make_learner(Lrnr_solnp, loss_function = loss_loglik_multinomial, learner_function = metalearner_linear_multinomial)

Q_learner <- make_learner(Lrnr_sl, qlib, metalearner)
g_learner <- make_learner(Lrnr_sl, glib, mn_metalearner)
b_learner <- make_learner(Lrnr_sl, blib, metalearner)
learner_list <- list(Y = Q_learner, A = g_learner, B=b_learner)

###################
# Check coverage
###################

psi<-matrix(nrow = MC,ncol=3)
cov<-matrix(nrow = MC,ncol=3)
lower<-matrix(nrow = MC,ncol=3)
upper<-matrix(nrow = MC,ncol=3)

for(i in 1:MC){
  
  #Generate a random draw:
  data_full <- gen_data(n, 5)
  data<-data_full[,1:7]
  data$A<-as.numeric(levels(data$A))[data$A]
  data<-as.data.table(data)
  
  ##############
  #Test blip2:
  ##############
  
  tmle_spec <- tmle3_mopttx(V=c("W1","W2","W3","W4","W5"), type="blip2")
  tmle_task <- tmle_spec$make_tmle_task(data, node_list)
  initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)
  opt_rule <- Optimal_Rule$new(tmle_task, initial_likelihood, "split-specific", blip_library=learner_list$B)
  opt_rule$fit_blip()
  lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
  updater <- tmle3_cv_Update$new()
  targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood, updater)
  tsm_rule <- Param_TSM$new(targeted_likelihood, lf_rule)
  updater$tmle_params <- tsm_rule
  tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, list(tsm_rule), updater)
  
  psi[i,2]<-tmle_fit$summary$tmle_est
  lower[i,2]<-tmle_fit$summary$lower
  upper[i,2]<-tmle_fit$summary$upper
  cov[i,2]<-(tmle_fit$summary$lower<=truthn && tmle_fit$summary$upper>=truthn)
  
  ##############
  #Test blip3:
  ##############
  
  tmle_spec <- tmle3_mopttx(V=c("W1","W2","W3","W4","W5"), type="blip3")
  tmle_task <- tmle_spec$make_tmle_task(data, node_list)
  initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)
  opt_rule <- Optimal_Rule$new(tmle_task, initial_likelihood, "split-specific", blip_library=learner_list$B)
  opt_rule$fit_blip()
  lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
  updater <- tmle3_cv_Update$new()
  targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood, updater)
  tsm_rule <- Param_TSM$new(targeted_likelihood, lf_rule)
  updater$tmle_params <- tsm_rule
  tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, list(tsm_rule), updater)
  
  psi[i,3]<-tmle_fit$summary$tmle_est
  lower[i,3]<-tmle_fit$summary$lower
  upper[i,3]<-tmle_fit$summary$upper
  cov[i,3]<-(tmle_fit$summary$lower<=truthn && tmle_fit$summary$upper>=truthn)
  
}

save.image(here("Simulation/Simulation_1a_res.RData"))
