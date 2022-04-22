context("Test resource constrained binary rule")

library(sl3)
library(data.table)
library(tmle3mopttx)
library(tmle3)
#library(devtools)
#load_all()

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
  "Lrnr_xgboost",
  "Lrnr_mean"
)

metalearner <- make_learner(Lrnr_nnls)
Q_learner <- make_learner(Lrnr_sl, qlib, metalearner)
g_learner <- make_learner(Lrnr_sl, glib, metalearner)
b_learner <- make_learner(Lrnr_sl, blib, metalearner)
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

# Define nodes:
node_list <- list(W = c("W1", "W2", "W3"), A = "A", Y = "Y")

test_that("Binary resource constraint", {
  tmle_spec <- tmle3_mopttx_blip_revere(
    V = c("W1", "W2", "W3"), type = "blip1",
    learners = learner_list,
    maximize = TRUE, complex = TRUE, realistic = FALSE, 
    resource=0.4
  )
  
  #tmle_task <- tmle_spec$make_tmle_task(data, node_list)
  #initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)
  #updater <- tmle_spec$make_updater()
  #targeted_likelihood <- tmle_spec$make_targeted_likelihood(initial_likelihood, updater)
  #tmle_params <- tmle_spec$make_params(tmle_task, likelihood=targeted_likelihood)
  
  #fit <- fit_tmle3(tmle_task, targeted_likelihood, tmle_params, updater)
  
  fit <- tmle3(tmle_spec, data, node_list, learner_list)
  expect_equal(sum(tmle_spec$return_rule), 400, tolerance = 50)
  
})


