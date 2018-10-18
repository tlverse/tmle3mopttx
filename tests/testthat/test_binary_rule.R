context("Test binary rule")

library(testthat)
library(sl3)
library(data.table)
library(tmle3mopttx)
library(tmle3)

set.seed(1234)

data("test_vim_data")

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

metalearner <- make_learner(Lrnr_nnls)

Q_learner <- make_learner(Lrnr_sl, qlib, metalearner)
g_learner <- make_learner(Lrnr_sl, glib, metalearner)
b_learner <- make_learner(Lrnr_sl, blib, metalearner)
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

# Define spec:
tmle_spec <- tmle3_mopttx_blip(
  V = c("W01", "W02", "W03", "W04"), type = "blip1",
  b_learner = learner_list$B, maximize = TRUE
)

# Define nodes:
node_list <- list(W = node_list$W, A = "A_bin01", Y = node_list$Y)

# Define data:
tmle_task <- tmle_spec$make_tmle_task(data, node_list)

# Define likelihood:
initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)

# Shortcut:
# fit <- tmle3(tmle_spec, data, node_list, learner_list)

updater <- tmle_spec$make_updater()
targeted_likelihood <- tmle_spec$make_targeted_likelihood(initial_likelihood, updater)

tmle_params <- tmle_spec$make_params(tmle_task, targeted_likelihood)

fit <- fit_tmle3(tmle_task, targeted_likelihood, tmle_params, updater)

# extract results
tmle3_psi <- fit$summary$tmle_est
tmle3_se <- fit$summary$se
tmle3_epsilon <- updater$epsilons[[1]]$Y

test_that("Mean under the optimal binary rule is correct", {
  expect_equal(tmle3_psi, 0.7489233, tolerance = 0.1)
})
