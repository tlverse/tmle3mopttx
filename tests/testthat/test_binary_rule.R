context("Test binary rule")

library(testthat)
library(sl3)
library(data.table)
library(tmle3mopttx)
library(tmle3)

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

metalearner <- make_learner(Lrnr_nnls)

Q_learner <- make_learner(Lrnr_sl, qlib, metalearner)
g_learner <- make_learner(Lrnr_sl, glib, metalearner)
b_learner <- make_learner(Lrnr_sl, blib, metalearner)
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

# Define nodes:
node_list <- list(W = c("W1", "W2", "W3"), A = "A", Y = "Y")

# Test when V is an empty set:

# Define spec:
tmle_spec_noV <- tmle3_mopttx_blip_revere(
  type = "blip1", learners = learner_list, maximize = TRUE,
  complex = TRUE, realistic = TRUE
)

tmle_task <- tmle_spec_noV$make_tmle_task(data, node_list)
initial_likelihood <- tmle_spec_noV$make_initial_likelihood(
  tmle_task,
  learner_list
)
updater <- tmle_spec_noV$make_updater()
targeted_likelihood <- tmle_spec_noV$make_targeted_likelihood(
  initial_likelihood,
  updater
)
tmle_params <- tmle_spec_noV$make_params(tmle_task, likelihood = targeted_likelihood)


# Define spec:
tmle_spec <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3"), type = "blip1",
  learners = learner_list, maximize = TRUE,
  complex = TRUE, realistic = TRUE
)



# Define data:
tmle_task <- tmle_spec$make_tmle_task(data, node_list)
fit <- tmle3(tmle_spec, data, node_list, learner_list)

test_that("Mean under the optimal binary rule is correct", {
  expect_equal(tmle3_psi, 0.7489233, tolerance = 0.1)
})
