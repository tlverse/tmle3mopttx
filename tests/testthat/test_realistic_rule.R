context("Test realistic rule")

library(sl3)
library(data.table)
library(tmle3mopttx)
library(tmle3)

set.seed(1234)

data("data_cat_realistic")
data <- data_cat_realistic

xgboost_10 <- Lrnr_xgboost$new(nrounds = 10)
xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn1 <- Lrnr_mean$new()
lrn2 <- Lrnr_glm_fast$new()

Q_learner <- Lrnr_sl$new(
  learners = list(lrn1, lrn2),
  metalearner = Lrnr_nnls$new()
)

mn_metalearner <- make_learner(Lrnr_solnp,
                               eval_function = loss_loglik_multinomial,
                               learner_function = metalearner_linear_multinomial
)
g_learner <- make_learner(Lrnr_sl, list(xgboost_10, xgboost_50, lrn1), mn_metalearner)

# Define the Blip learner, which is a multivariate learner:
learners <- list(lrn1, xgboost_10, xgboost_50)
b_learner <- create_mv_learners(learners = learners)

learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

# Define nodes:
node_list <- list(W = c("W1", "W2", "W3", "W4"), A = "A", Y = "Y")

test_that("Categorical rule, realistic rule", {
  tmle_spec <- tmle3_mopttx_blip_revere(
    V = c("W4", "W3", "W2", "W1"), type = "blip2",
    learners = learner_list, maximize = TRUE,
    complex = TRUE, realistic = TRUE
  )
  # tmle_task <- tmle_spec$make_tmle_task(data, node_list)
  # initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)
  # updater <- tmle_spec$make_updater()
  # targeted_likelihood <- tmle_spec$make_targeted_likelihood(initial_likelihood, updater)
  # tmle_params <- tmle_spec$make_params(tmle_task, likelihood=targeted_likelihood)
  # fit <- fit_tmle3(tmle_task, targeted_likelihood, tmle_params, updater)

  fit <- tmle3(tmle_spec, data, node_list, learner_list)
  expect_equal(fit$summary$tmle_est, 0.4989788, tolerance = 0.2)
})
