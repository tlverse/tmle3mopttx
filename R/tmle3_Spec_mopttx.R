#' Defines a tmle for the mean under the optimal individualized treatment with categorical treatment
#'
#' @importFrom R6 R6Class
#'
#' @export
#
tmle3_Spec_mopttx <- R6Class(
  classname = "tmle3_Spec_mopttx",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(V, type, ...) {
      options <- list(V = V, type=type)
      do.call(super$initialize, options)
    },
    
    factor_to_indicators = function(x) {
      x_vals <- vals_from_factor(x)
      ind_mat <- sapply(x_vals, function(x_val) as.numeric(x_val == x))
      colnames(ind_mat) <- x_vals
      
      return(ind_mat)
    },
    
    vals_from_factor = function(x) {
      sort(unique(x))
    },
    
    extract_val = function(fold, split_preds) {
      
      #Extract fold and corresponding validation samples.
      v <- fold_index()
      valid_idx <- validation()
      
      #Extract validation samples for fold v for all the values of split_preds
      val_preds <- sapply(split_preds, function(split_pred) {
        split_pred[[v]][valid_idx]
      })
      
      #Add index to it (aka, which sample is in question?)
      val_preds <- as.data.frame(val_preds)
      val_preds$index <- valid_idx
      val_preds$folds <- rep(v,nrow(val_preds))
      result <- list(preds = val_preds)
      
      return(result)
    },
    
    #Get split specific predictions for Q,Q(a,W),ga,rule
    make_split_specific = function(initial_likelihood, tmle_task) {

      #Get data, Y and A
      data<-tmle_task$data
  
      #Predict on all the data:
      new_folds<-origami::make_folds(data, fold_fun = origami::folds_resubstitution)[[1]]

      #Necessary to redefine the task:
      tY<-tmle_task$get_regression_task("Y")
      tA<-tmle_task$get_regression_task("A")
      
      split_preds <- cross_validate(self$opttx_split_preds, tmle_task$folds, new_folds, data, tY, tA, .combine = F)
      split_preds$errors<-NULL
      
      val_preds <- cross_validate(self$extract_val, tmle_task$folds, split_preds)$preds
      val_preds <- val_preds[order(val_preds$index), ]
      
      private$.split_preds<-split_preds
      private$.val_preds<-val_preds
    },
    
    pred_all_Q = function(v, newdata, covars, outcome, A_vals,new_folds) {
      sapply(A_vals, function(A_val) {
        newdata[,"A"] <- A_val
        task<-sl3::make_sl3_Task(newdata, covariates = covars, outcome = outcome, folds=new_folds)
        pred<-initial_likelihood$factor_list$Y$learner$predict_fold(task, fold=v)
        pred
      })
      
    },
    
    opttx_split_preds = function(fold, new_folds, data, tY, tA, ...){
      
      v<-fold_index()
      
      A<-data$A
      Y<-data$Y
      A_vals<-self$vals_from_factor(A)
      
      #QaW:
      QaW<-self$pred_all_Q(v=v, newdata=data, covars=names(tY$X), outcome="Y", A_vals=A_vals, 
                      new_folds=new_folds)
      colnames(QaW)<-paste0("level A=", A_vals)
      
      #pA:
      task<-sl3::make_sl3_Task(data, covariates = names(tA$X), outcome = "A", folds=new_folds)
      pA <- do.call("rbind", initial_likelihood$factor_list$A$learner$predict_fold(task, fold=v))
      pA <- do.call("rbind", pA)
      pA[pA < 0.05] <- 0.05
      colnames(pA)<-paste0("level A=", A_vals)
      
      #Rule
      A_ind<-self$factor_to_indicators(A)
      Y_mat <- replicate(length(A_vals), Y)
      DR <- (A_ind/pA) * (Y_mat - QaW) + QaW
      colnames(DR)<-paste0("level A=", A_vals)
      Z <- max.col(DR)
      
      list(QaW = QaW, pA = pA, DR = DR, Z = Z, Y = Y, A = A, v = rep(v, length(Y)))
    },

    learn_rule = function(tmle_task, learner_list){

      #Grab V:
      V<-private$.options$V
      
      #Learn rule:
      cv_rule<-cross_validate(self$cv_rule, tmle_task$folds, learner_list=learner_list, 
                              tmle_task=tmle_task, V=V, .combine=F)
      
      pred<-matrix(nrow = nrow(tmle_task$data), ncol = cv_rule$lev[[1]])

      #Construct SL for the rule:
      for(i in 1:cv_rule$lev[[1]]){
        
        #Grab i contrast:
        y<-do.call(rbind, cv_rule$DR)[,i]

        #Grab i predicted contrast:
        inter<-lapply(cv_rule$cvPred, '[[',i)
        x<-do.call(rbind, inter)
        
        fit_coef <- stats::coef(nnls::nnls(as.matrix(x), as.matrix(y)))
        fit_coef <- fit_coef/sum(fit_coef)
        
        pred[,i]<-as.matrix(x) %*% fit_coef
      }
      
      optA<-max.col(pred)
      
      private$.optA<-optA
    },
    
    cv_rule = function(fold, learner_list, tmle_task, V){
      
      v<-fold_index()
      
      #Use DR and pA fit on !v folds, predicted on all:
      DR <- private$.split_preds$DR[[v]]
      pA <- private$.split_preds$pA[[v]]
      
      #Type of pseudo-blip:
      blip_type <- private$.options$type
      
      if (blip_type == "blip1") {
        #First category as a reference category:
        y <- DR[, -1] - DR[, 1]
      } else if (blip_type == "blip2") {
        #Average of all categories as a reference category:
        y <- DR - rowMeans(DR)
      } else if (blip_type == "blip3") {
        #Weighted average of all categories as a reference category:
        y <- DR - rowSums(DR * pA)
      } else {
        y <- DR
      }
      
      #Predict for each category, for now
      #TO DO: multivariate SL
      res<-list()
      for(i in 1:ncol(y)){
        
        new_data<-cbind.data.frame(Y=y[,i], tmle_task$data[,V,with=FALSE])
        new_data_v<-new_data[fold$validation_set,]
        
        cov<-names(new_data)[-1]
        outcome<-names(new_data)[1]
  
        #Regress by creating a new tmle task:
        blip_tmle_task <- sl3::make_sl3_Task(new_data, covariates=cov,
                                                   outcome=outcome, folds=tmle_task$folds)
        sl_fit<-learner_list$B$train(blip_tmle_task)
        
        #Grab fold-specific fit and predict on validation samples (for each learner):
        val_task<-make_sl3_Task(new_data_v, covariates=cov, 
                                outcome=outcome, folds=fold)
        res[[i]]<-sl_fit$fit_object$cv_fit$fit_object$fold_fits[[v]]$predict(val_task)
        row.names(res[[i]])<-fold$validation_set

      }
      
      return(list(cvPred=res, valSet=fold$validation_set, DR=y[fold$validation_set,], lev=ncol(y)))
    },
    
    get_rule = function(){
      return(private$.optA)
    },

    make_params = function(tmle_task, likelihood) {
      baseline_level <- self$options$baseline_level
      intervention <- define_lf(LF_static, "A", value = baseline_level)
      tsm <- Param_TSM$new(likelihood, intervention)
      mean_param <- Param_mean$new(likelihood)
      par <- Param_delta$new(likelihood, delta_param_PAR, list(tsm, mean_param))
      paf <- Param_delta$new(likelihood, delta_param_PAF, list(tsm, mean_param))
      rr <- Param_delta$new(likelihood, delta_param_PAF, list(tsm, mean_param))
      tmle_params <- list(tsm, mean_param, par, paf, rr)

      return(tmle_params)
    }


  ),
  active = list(),
  private = list(
    .split_preds=list(),
    .val_preds=list(),
    .optA=list()
  )
)

#'
#' O=(W,A,Y)
#' W=Covariates
#' A=Treatment (binary or categorical)
#' Y=Outcome (binary or bounded continuous)
#' V=Covariates the rule depends on
#' @importFrom sl3 make_learner Lrnr_mean
#' @export
tmle3_mopttx <- function(V, type) {
  tmle3_Spec_mopttx$new(V=V, type=type)
}
