context("Test Q learning")

library(testthat)
library(sl3)
library(data.table)
library(tmle3mopttx)
library(tmle3)

set.seed(1234)

data("data_cat_realistic")
data <- data_cat_realistic

xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn1 <- Lrnr_mean$new()
lrn2 <- Lrnr_glm_fast$new()

Q_learner <- Lrnr_sl$new(
  learners = list(xgboost_100, xgboost_500, lrn1, lrn2),
  metalearner = Lrnr_nnls$new()
)

mn_metalearner <- make_learner(Lrnr_solnp,
  loss_function = loss_loglik_multinomial,
  learner_function = metalearner_linear_multinomial
)
g_learner <- make_learner(Lrnr_sl, list(xgboost_100, xgboost_500, lrn1), mn_metalearner)

# Define the Blip learner, which is a multivariate learner:
learners <- list(xgboost_100, xgboost_500, lrn1, lrn2)
b_learner <- create_mv_learners(learners = learners)

learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

# Define nodes:
node_list <- list(W = c("W1", "W2", "W3", "W4"), A = "A", Y = "Y")

test_that("Categorical rule, Q learning", {
  # initialize a tmle specification
  tmle_spec_Q <- tmle3_mopttx_Q(maximize = TRUE)

  # Define data:
  tmle_task <- tmle_spec_Q$make_tmle_task(data, node_list)

  # Define likelihood:
  initial_likelihood <- tmle_spec_Q$make_initial_likelihood(tmle_task, learner_list)

  # Estimate the parameter:
  fit <- Q_learning(tmle_spec_Q, initial_likelihood, tmle_task)

  expect_equal(fit, 0.4765371, tolerance = 0.2)
})