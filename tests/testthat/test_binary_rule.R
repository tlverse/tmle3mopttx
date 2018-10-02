context("Test binary rule")

if (FALSE) {
  setwd("..")
  setwd("..")
  getwd()
  library("devtools")
  document()
  load_all("./")
  setwd("..")
  install("tstmle", build_vignettes = FALSE, dependencies = FALSE)
}

library(testthat)
library(sl3)
library(tmle3mopttx)

set.seed(1234)

data("test_vim_data")

#Define sl3 library and metalearners:
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
learner_list <- list(Y = Q_learner, A = g_learner, B=b_learner)

#Define spec:
tmle_spec <- tmle3_mopttx(V=c("W01","W02","W03","W04"), type="blip1", b_learner=learner_list$B)

#Define nodes:
node_list <- list(W=node_list$W, A="A_bin01", Y=node_list$Y)

#Define data:
tmle_task <- tmle_spec$make_tmle_task(data, node_list)

#Define likelihood:
initial_likelihood <- tmle_spec$make_initial_likelihood(tmle_task, learner_list)

#Shortcut:
#fit <- tmle3(tmle_spec, data, node_list, learner_list)

#Learn the rule:
opt_rule <- Optimal_Rule$new(tmle_task, initial_likelihood, "split-specific", blip_library=learner_list$B,
                             blip_type=tmle_spec$.__enclos_env__$private$.options$type)
opt_rule$fit_blip()

#Define a dynamic likelihood factor:
lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)

#Define updater and targeted likelihood:
updater <- tmle3_cv_Update$new()
targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood, updater)

tsm_rule <- Param_TSM$new(targeted_likelihood, lf_rule)

updater$tmle_params <- tsm_rule
tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, list(tsm_rule), updater)

# extract results
tmle3_psi <- tmle_fit$summary$tmle_est
tmle3_se <- tmle_fit$summary$se
tmle3_epsilon <- updater$epsilons[[1]]$Y

test_that("Mean under the optimal binary rule is correct", {

  expect_equal(tmle3_psi, 0.7044482, tolerance = 0.1)
  
})





