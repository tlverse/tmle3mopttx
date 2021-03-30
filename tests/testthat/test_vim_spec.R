context("Test categorical rule Variable Importance Measure")

library(sl3)
library(data.table)
library(tmle3mopttx)
library(tmle3)
library(foreach)

set.seed(1234)

data("data_cat_vim")
data <- data_cat_vim
xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn1 <- Lrnr_mean$new()
lrn2 <- Lrnr_glm_fast$new()
lrn3 <- Lrnr_glmnet$new()

Q_learner <- Lrnr_sl$new(
  learners = list(lrn1, lrn2, lrn3),
  metalearner = Lrnr_nnls$new()
)

mn_metalearner <- make_learner(Lrnr_solnp,
  loss_function = loss_loglik_multinomial,
  learner_function = metalearner_linear_multinomial
)
g_learner <- make_learner(Lrnr_sl, list(lrn3, xgboost_100, lrn1), mn_metalearner)

# Define the Blip learner, which is a multivariate learner:
learners <- list(lrn1, lrn2)
b_learner <- create_mv_learners(learners = learners)

learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

# Define nodes:
node_list <- list(W = c("W2", "W3", "W4"), A = c("A", "W1"), Y = "Y")
data$A <- as.integer(data$A)

test_that("Categorical rule, Variable Importance", {
  tmle_spec <- tmle3_mopttx_vim(
    V = "W3", learners = learner_list, type = "blip2",
    contrast = "multiplicative",
    maximize = FALSE,
    method = "SL", complex = TRUE, realistic = FALSE
  )

  vim_results <- tmle3_vim(tmle_spec, data,
    node_list = node_list, learner_list,
    adjust_for_other_A = TRUE
  )

  expect_equal(vim_results$tmle_est[1], -0.2955736, tolerance = 0.2)
})

# tmle_task <- tmle_spec_opttx$make_tmle_task(data, node_list)
# g_task <- tmle_task$get_regression_task("A")
# debugonce(metalearner_linear_multinomial)
# debugonce(g_learner2$.__enclos_env__$private$.train_sublearners)
# g_fit <- g_learner2$train(g_task)

# unpack_predictions(g_fit$predict())

# preds <- g_learner$predict(g_task)

# fit tsm spec

# tmle_spec_tsm <- tmle_TSM_all()

# fit_tsm <- tmle3(tmle_spec_tsm, data, node_list, learner_list)

# extract rule
# rule_fun <- fit_opttx$tmle_params[[1]]$cf_likelihood$intervention_list$A$rule_fun
# treatment_assignment <- rule_fun(fit_opttx$tmle_task)
# table(treatment_assignment)

###############
# Q learning:
###############

# Define spec:
# fit opttx spec with Q-learning
# tmle_spec <- tmle3_mopttx_vim(
#  contrast = "multiplicative",
#  maximize = FALSE,
#  method = "Q"
# )

# Fast way of doing it:
# fit_opttx <- tmle3(tmle_spec, data, node_list, learner_list)

# Variable importance:
# vim_results <- tmle3_vim(tmle_spec, data,
#                         node_list = node_list, learner_list,
#                         adjust_for_other_A = FALSE
# )
