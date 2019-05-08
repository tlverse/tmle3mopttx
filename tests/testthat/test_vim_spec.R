context("Test vim spec")

library(testthat)
library(sl3)
library(tmle3mopttx)
library(tmle3)
library(data.table)
library(here)

set.seed(1234)

data("data_cat_vim")
data <- data_cat_vim
data <- data.table(data)
# data[, A := factor(A)]

# Define nodes:
node_list <- list(
  W = c("W3", "W4"),
  A = c("A", "W1", "W2"),
  Y = "Y"
)

xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn1 <- Lrnr_mean$new()
lrn2 <- Lrnr_glm_fast$new()

Q_learner <- Lrnr_sl$new(
  learners = list(xgboost_100, xgboost_500, lrn1, lrn2),
  metalearner = Lrnr_nnls$new()
)

# Define the g learner, which is a multinomial learner:
glib <- list(xgboost_100, lrn1)

mn_metalearner <- make_learner(Lrnr_solnp,
  loss_function = loss_loglik_multinomial,
  learner_function = metalearner_linear_multinomial
)
g_learner <- make_learner(Lrnr_sl, glib, mn_metalearner)

# Define the Blip learner, which is a multivariate learner:
learners <- list(xgboost_100, xgboost_500, lrn1, lrn2)
b_learner <- create_mv_learners(learners = learners)

learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

##################
# Split-specific:
##################

# fit opttx spec with split-specific method:
tmle_spec <- tmle3_mopttx_vim(
  V = node_list$W, b_learner = learner_list$B, type = "blip2",
  contrast = "multiplicative",
  maximize = FALSE,
  method = "SL"
)

# Variable importance:
vim_results <- tmle3_vim(tmle_spec, data,
  node_list = node_list, learner_list,
  adjust_for_other_A = TRUE
)

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
