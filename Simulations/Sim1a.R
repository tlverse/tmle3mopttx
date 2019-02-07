#Simulation 1a: 
#Rule dependent on single covariate
#~50% A=1 and ~50% A=0 allocation is the optimal rule
#When W4>0, optimal A=1
#When W4<0, optimal A=0

library(tmle3mopttx)
library(sl3)
library(tmle3)

tda_manual= function(opt, tda_task){
  
  #Define new task:
  tmle_task<-tda_task
  
  #Make working with opt easier:
  self<-opt
  private<-opt$.__enclos_env__$private
  
  #Get blip fits:
  blip_fits<-private$.blip_fits
  
  #Get estimated coefficients:
  fit_coef<-data.frame(self$coef_fits)
  
  #Define the blip task:
  blip_tmle_task <- make_sl3_Task(self$V_data(tmle_task),
                                  covariates = self$V,outcome = NULL)
  
  blip_fin <- matrix(nrow = nrow(blip_tmle_task$data), ncol = length(blip_fits[[1]]))

  for (j in 1:length(blip_fits[[1]])) {
    
    temp <- lapply(1:length(tmle_task$folds), function(v) {
      pred<-blip_fits[[v]][[j]]$fit_object$cv_fit$predict_fold(blip_tmle_task, fold_number = 1)
      pred[tmle_task$folds[[v]]$validation_set, ]
  })
  
  # Average over split-specific fits per algorithm
  blip_pred <- do.call(rbind, temp)
  pred <- data.frame(pred = as.matrix(blip_pred) %*% t(fit_coef))
  row.names(pred) <- unlist(lapply(1:length(tmle_task$folds), function(i) tmle_task$folds[[i]]$validation_set))
  pred <- pred[order(as.numeric(row.names(pred))), ]
  pred <- data.frame(pred)

}

  if (length(blip_fits[[1]]) == 1) {
    rule <- as.numeric(pred$pred > 0)
    } else {
      # Combine all the sl validation samples:
      if (private$.maximize) {
        if (blip_type == "blip1") {
          rule <- max.col(blip_fin) + 1
          } else {
            rule <- max.col(blip_fin)
            }
        } else {
          if (blip_type == "blip1") {
            rule <- max.col(-1 * blip_fin) + 1
            } else {
              rule <- max.col(-1 * blip_fin)
            }
        }
      }
  return(rule)
}

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

set.seed(11)
data_full <- gen_data(1000000, 4)
data_tda <- gen_data(500000, 4)
data_tda <- data_tda[,1:6]
psi<-mean(data_full$Yd0)
mean(data_full$Y1)
mean(data_full$Y0)
table(data_full$d0)

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

##########################
# n=1000

MC=500
n=1000

est<-data.frame(matrix(data = NA, nrow = MC,12))
names(est)<-c("type","param","init_est","tmle_est","se","lower","upper",
                  "psi_transformed","lower_transformed","upper_transformed","coverage", "coverage data-adaptive")

for(i in 1:MC){
  
  #Generate data:
  data <- gen_data(n, 4)
  W<-data_tda[,1:4]
  
  # Define spec:
  tmle_spec <- tmle3_mopttx_blip(
    V = c("W4"), type = "blip1",
    b_learner = learner_list$B, maximize = TRUE, complex = TRUE
  )
  
  # Define nodes:
  node_list <- list(W = c("W1", "W2", "W3", "W4"), A = "A", Y = "Y")

  fit <- tmle3(tmle_spec, data=data, node_list=node_list, learner_list=learner_list)
  fit
  
  #est[i,1:10]<-tmle_spec$make_est_fin(fit, tmle_spec$options$max, p.value = 0.1)
  est[i,1:10]<-fit$print()
  est[i,11]<-ifelse((est[i,9]<=psi && est[i,10] >= psi),1,0)
  
  #True data-adaptive parameter:
  tda_task <- tmle_spec$make_tmle_task(data_tda, node_list)
  opt<-tmle_spec$return_rule()
  
  rule<-tda_manual(opt=opt, tda_task=tda_task)
  Ydn <- as.numeric(runif(500000) < Qbar0(rule, W))
  Edn<-mean(Ydn)
  est[i,12]<-ifelse((est[i,9]<=Edn && est[i,10] >= Edn),1,0)
}

save.image("~/Dropbox/Berkeley_Projects/Software/tmle3mopttx/Simulations/Sim1a/Sim1a_n1000_MC500.RData")

