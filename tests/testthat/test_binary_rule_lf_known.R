context("Test binary rule, known likelihood")

library(sl3)
library(data.table)
library(tmle3mopttx)
library(tmle3)
library(SuperLearner)

set.seed(1234)

data("data_bin")
data <- data_bin

# Define sl3 library and metalearners:
qlib <- make_learner_stack(
  "Lrnr_mean",
  "Lrnr_glm_fast"
)

glib <- make_learner_stack(
  "Lrnr_mean",
  "Lrnr_glmnet",
  "Lrnr_xgboost"
)

blib <- make_learner_stack(
  "Lrnr_glm_fast",
  "Lrnr_xgboost"
)

# Define nodes:
node_list <- list(W = c("W1", "W2", "W3"), A = "A", Y = "Y")

#######################
# Feed in sl3 fits
#######################

metalearner <- make_learner(Lrnr_nnls)
Q_learner <- make_learner(Lrnr_sl, qlib, metalearner)
g_learner <- make_learner(Lrnr_sl, glib, metalearner)
b_learner <- make_learner(Lrnr_sl, blib, metalearner)
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

tmle_spec_working <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3"), type = "blip1",
  learners = learner_list, maximize = TRUE,
  complex = TRUE, realistic = TRUE
)
tmle_task_working <- tmle_spec_working$make_tmle_task(data, node_list)

#Train all learners separately:
task_A <- tmle_task_working$get_regression_task("A")
task_Y <- tmle_task_working$get_regression_task("Y")

Q_fit <- Q_learner$train(task_Y)
g_fit <- g_learner$train(task_A)
learner_list <- list(Y = Q_fit, A = g_fit, B = b_learner)

test_that("Binary rule, already known sl3 fits", {
  tmle_spec <- tmle3_mopttx_blip_revere(
    V = c("W1", "W2", "W3"), type = "blip1",
    learners = learner_list, maximize = TRUE,
    complex = TRUE, realistic = TRUE
  )
  
  #tmle_task <- tmle_spec$make_tmle_task(data, node_list)
  #initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)
  #updater <- tmle_spec$make_updater()
  #targeted_likelihood <- tmle_spec$make_targeted_likelihood(initial_likelihood, updater)
  #tmle_params <- tmle_spec$make_params(tmle_task, likelihood=targeted_likelihood)
  #fit <- fit_tmle3(tmle_task, targeted_likelihood, tmle_params, updater)
  
  fit <- tmle3(tmle_spec, data, node_list, learner_list)
  expect_equal(fit$summary$tmle_est, 0.5658569, tolerance = 0.2)
})

###########################
# Feed in known functions
###########################

#Pretend we know the actual functions:
g_mean <- function(task) {
  W1 <- task$data$W1
  scale_factor <- 0.8
  A <- plogis(scale_factor * W1)
  
  return(A)
}

Q_mean <- function(task) {
  W1 <- task$data$W1
  W2 <- task$data$W2
  W3 <- task$data$W3
  A  <- task$data$A
  
  Qbar <- (1 / 2) * (plogis(-5 * (A == 1) * (W1 - 0.5) + 5 * (A == 0) * (W1 - 0.5)) + 0.5 * plogis(W2 * W3))
  
  return(Qbar)
}

factor_list <- list(
  define_lf(LF_emp, "W"),
  define_lf(LF_known, "A", mean_fun = g_mean, type="mean"),
  define_lf(LF_known, "Y", mean_fun = Q_mean, type = "mean")
)
likelihood_def <- Likelihood$new(factor_list)


test_that("Binary rule, likelihood overwrite", {
  tmle_spec <- tmle3_mopttx_blip_revere(
    V = c("W1", "W2", "W3"), type = "blip1",
    learners = learner_list, maximize = TRUE,
    complex = TRUE, realistic = TRUE, 
    likelihood_override = likelihood_def
  )
  
  #tmle_task <- tmle_spec$make_tmle_task(data, node_list)
  #initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)
  #updater <- tmle_spec$make_updater()
  #targeted_likelihood <- tmle_spec$make_targeted_likelihood(initial_likelihood, updater)
  #tmle_params <- tmle_spec$make_params(tmle_task, likelihood=targeted_likelihood)
  #fit <- fit_tmle3(tmle_task, targeted_likelihood, tmle_params, updater)
  
  fit <- tmle3(tmle_spec, data, node_list, learner_list)
  expect_equal(fit$summary$tmle_est, 0.5739371, tolerance = 0.2)
})


