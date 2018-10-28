context("Test binary rule simulation 1b: Static vs. Estimated Rule")

library(testthat)
library(sl3)
library(data.table)
library(tmle3mopttx)
library(tmle3)

set.seed(1234)

data("test_static_better")
data <- test_static_better

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
  V = c("W4", "W1", "W2", "W3"), type = "blip1",
  b_learner = learner_list$B, maximize = TRUE, complex = FALSE
)

# Define nodes:
node_list <- list(W = c("W1", "W2", "W3", "W4"), A = "A", Y = "Y")

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
fit

# Check the used rule:
table(fit$tmle_params[[1]]$cf_likelihood$intervention_list$A$rule_fun(tmle_task))

# extract results
tmle3_psi <- fit$summary$tmle_est
tmle3_se <- fit$summary$se
tmle3_epsilon <- updater$epsilons[[1]]$Y

test_that("Mean under the optimal binary rule is correct", {
  expect_equal(tmle3_psi, 0.7489233, tolerance = 0.1)
})
