
library(tmle3mopttx)
library(sl3)
library(tmle3)
library(origami)
library(data.table)

Qbar0 <- function(A, W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  W4 <- W[, 4]
  #When W4>0, optimal A=1
  #When W4<0, optimal A=0
  Qbar <- ifelse(W4 > 0, plogis(7 * A), plogis(-5*A))
  return(Qbar)
}

g0 <- function(W) {
  W1 <- W[, 1]
  W2 <- W[, 2]
  W3 <- W[, 3]
  W4 <- W[, 4]
  
  plogis(0.25 * W1 - 0.1 * W2)
}

gen_data <- function(n = 1000, p = 4) {
  W <- matrix(rnorm(n * p), nrow = n)
  colnames(W) <- paste("W", seq_len(p), sep = "")
  A <- rbinom(n, 1, g0(W))
  u <- runif(n)
  Y <- as.numeric(u < Qbar0(A, W))
  Y0 <- as.numeric(u < Qbar0(0, W))
  Y1 <- as.numeric(u < Qbar0(1, W))
  d0 <- as.numeric(Qbar0(1, W) > Qbar0(0, W))
  Yd0 <- as.numeric(u < Qbar0(d0, W))
  data.frame(W, A, Y, Y0, Y1, Yd0, d0, blip = Qbar0(1, W) - Qbar0(0, W))
}

# Define sl3 library and metalearners:

xgboost_50<-Lrnr_xgboost$new(nrounds = 50)
xgboost_100<-Lrnr_xgboost$new(nrounds = 100)
xgboost_500<-Lrnr_xgboost$new(nrounds = 500)
xgboost_1000<-Lrnr_xgboost$new(nrounds = 1000)
glmnet_0.2<-Lrnr_glmnet$new(alpha = 0.2, lambda = 300)
glmnet_0.4<-Lrnr_glmnet$new(alpha = 0.4, lambda = 300)
glmnet_0.6<-Lrnr_glmnet$new(alpha = 0.6, lambda = 300)
glmnet_0.8<-Lrnr_glmnet$new(alpha = 0.8, lambda = 300)

lrn1 <- Lrnr_mean$new()
lrn2<-Lrnr_glm_fast$new()
lrn3<-Lrnr_hal9001$new()

Q_learner <- Lrnr_sl$new(
  #learners = list(xgboost_100,xgboost_500,xgboost_1000,lrn2),
  learners = list(xgboost_50,xgboost_100,xgboost_500,
                  lrn1,lrn2,xgboost_1000),
  metalearner = Lrnr_nnls$new()
)

g_learner <- Lrnr_sl$new(
  learners = list(lrn2),
  #learners = list(xgboost_100,xgboost_500,glmnet_0.2,glmnet_0.8,lrn1,lrn2),
  metalearner = Lrnr_nnls$new()
)

b_learner <- Lrnr_sl$new(
  #learners = list(xgboost_100,xgboost_500,xgboost_1000,lrn2),
  learners = list(xgboost_100,lrn2,xgboost_1000),
  metalearner = Lrnr_nnls$new()
)

learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

V_vars <- "W4"
# Define spec:
tmle_spec <- tmle3_mopttx_blip_revere(
  V = V_vars, type = "blip1",
  b_learner = learner_list$B, maximize = TRUE, complex = TRUE
)

set.seed(1111)
#Generate data:
n <- 1000
data <- gen_data(n, 4)
node_list <- list(W = c("W1", "W2", "W3", "W4"), A = "A", Y = "Y")
fit <- tmle3(tmle_spec, data=data, node_list=node_list, learner_list=learner_list)
fit
