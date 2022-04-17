context("Test categorical rule Variable Importance Measure")

library(sl3)
library(data.table)
library(tmle3mopttx)
library(tmle3)
library(foreach)

set.seed(1234)

data("data_cat_vim")
data <- data_cat_vim
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
node_list <- list(W = c("W2"), A = c("A", "W1"), Y = "Y")
data$A <- as.integer(data$A)

test_that("Categorical rule, Variable Importance", {
  tmle_spec <- tmle3_mopttx_vim(
    V = "W2", learners = learner_list, type = "blip2",
    contrast = "multiplicative",
    maximize = FALSE,
    method = "SL", complex = TRUE, realistic = FALSE
  )
  
  vim_results <- tmle3_vim(tmle_spec, data,
    node_list = node_list, learner_list,
    adjust_for_other_A = TRUE
  )

  expect_equal(vim_results$tmle_est[1], -0.2955736, tolerance = 0.3)
})
