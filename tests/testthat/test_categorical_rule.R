context("Test categorical rule")

library(testthat)
library(sl3)
library(tmle3mopttx)
library(tmle3)
library(data.table)
library(here)

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
                               b_learner = learner_list$B, max)

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









# Learn the rule:
opt_rule <- Optimal_Rule$new(tmle_task, initial_likelihood, "split-specific",
  blip_library = learner_list$B,
  blip_type = tmle_spec$options$type
)
opt_rule$fit_blip()

# Define a dynamic likelihood factor:
lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)

# Define updater and targeted likelihood:
updater <- tmle3_cv_Update$new()
targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood, updater)

tsm_rule <- Param_TSM$new(targeted_likelihood, lf_rule)

updater$tmle_params <- tsm_rule
tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, list(tsm_rule), updater)

# extract results
tmle3_psi <- tmle_fit$summary$tmle_est
#tmle3_se <- tmle_fit$summary$se
#tmle3_epsilon <- updater$epsilons[[1]]$Y

test_that("Mean under the optimal categorical rule is correct", {
  expect_equal(tmle3_psi, 0.621474, tolerance = 0.1)
})
