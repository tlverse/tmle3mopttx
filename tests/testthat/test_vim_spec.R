context("Test vim spec")

library(testthat)
library(sl3)
library(tmle3mopttx)
library(tmle3)
library(data.table)
library(here)

set.seed(1234)

load(here("data/test_vim_cat_data.rda"))
data<-test_vim_cat_data
# data[,A:=factor(A)]

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
# fit opttx spec
tmle_spec_opttx <- tmle3_mopttx_vim(V = c("W1", "W2", "W3", "W4", "W5"),
                              type = "blip2",
                              b_learner = learner_list$B,
                              contrast = "multiplicative",
                              maximize = FALSE)

# Optimal_Rule$debug("V_data")
# debug(tmle_spec_opttx$make_params)
fit_opttx <- tmle3(tmle_spec_opttx, data, node_list, learner_list)

# fit tsm spec

tmle_spec_tsm <- tmle_TSM_all()

fit_tsm <- tmle3(tmle_spec_tsm, data, node_list, learner_list)

# extract rule
rule_fun <- fit_opttx$tmle_params[[1]]$cf_likelihood$intervention_list$A$rule_fun
treatment_assignment <- rule_fun(fit_opttx$tmle_task)
table(treatment_assignment)
