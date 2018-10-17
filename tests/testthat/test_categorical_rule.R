context("Test categorical rule")

library(testthat)
library(sl3)
library(tmle3mopttx)
library(tmle3)
library(data.table)
library(here)
library(uuid)

set.seed(1234)

data(test_vim_cat_data)
data<-test_vim_cat_data

# Define nodes:
node_list <- list(
  W = c("W1", "W2", "W3", "W4", "W5"),
  A = "A",
  Y = "Y"
)

# Define sl3 library and metalearners:
qlib <- make_learner_stack(
  "Lrnr_mean",
  "Lrnr_glm_fast"
)

# sl3_list_learners(c("categorical"))
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
mn_metalearner <- make_learner(Lrnr_solnp, loss_function = loss_loglik_multinomial, learner_function = metalearner_linear_multinomial)

Q_learner <- make_learner(Lrnr_sl, qlib, metalearner)
g_learner <- make_learner(Lrnr_sl, glib, mn_metalearner)
b_learner <- make_learner(Lrnr_sl, blib, metalearner)
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

# Define spec:
tmle_spec <- tmle3_mopttx_blip(V = c("W1", "W2", "W3", "W4", "W5"), type = "blip1", 
                               b_learner = learner_list$B, maximize=TRUE)

# Define data:
tmle_task <- tmle_spec$make_tmle_task(data, node_list)

# Define likelihood:
initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)

# Shortcut:
# fit <- tmle3(tmle_spec, data, node_list, learner_list)

#Define an updater and define targeted likelihood:
updater <- tmle_spec$make_updater()
targeted_likelihood <- tmle_spec$make_targeted_likelihood(initial_likelihood, updater)

tmle_params <- tmle_spec$make_params(tmle_task, targeted_likelihood)
updater$tmle_params <- tmle_params

#fit <- fit_tmle3(tmle_task, targeted_likelihood, tmle_params, updater)

# extract results
#tmle3_psi <- fit$summary$tmle_est

#test_that("Mean under the optimal categorical rule is correct", {
#  expect_equal(tmle3_psi, 0.621474, tolerance = 0.1)
#})
