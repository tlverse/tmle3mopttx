#' Learns an optimal rule given a tmle_task and likelihood
#'
#' @importFrom R6 R6Class
#'
#' @export
#
Optimal_Rule <- R6Class(
  classname = "Optimal_Rule",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(tmle_task, likelihood, cv_fold = -1, V = NULL, blip_type="blip2") {
      private$.tmle_task <- tmle_task
      private$.likelihood <- likelihood
      private$.cv_fold <- cv_fold
      private$.blip_type <- blip_type
      if(missing(V)){
        V <- tmle_task$npsem$W$variables
        private$.V <- V
      }
      private$.cv_fold <- cv_fold
    },
    factor_to_indicators = function(x, x_vals) {
      ind_mat <- sapply(x_vals, function(x_val) as.numeric(x_val == x))
      colnames(ind_mat) <- x_vals
      return(ind_mat)
    },
    V_data = function(tmle_task){
      tmle_task$data[,self$V,with=FALSE]
    },
    fit_blip = function(){
      tmle_task <- self$tmle_task
      likelihood <- self$likelihood
      cv_fold <- self$cv_fold
      
      A_vals <- tmle_task$npsem$A$variable_type$levels  
      
      cf_tasks <- lapply(A_vals, function(A_val) {
        newdata <- data.table(A=A_val)
        cf_task <- tmle_task$generate_counterfactual_task(UUIDgenerate(), newdata)
        return(cf_task)
      })
      
      Q_vals <- sapply(cf_tasks, likelihood$get_likelihood, "Y", cv_fold)
      g_vals <- sapply(cf_tasks, likelihood$get_likelihood, "A", cv_fold)
      
      
      # DR A-IPW mapping of blip
      A <- tmle_task$get_tmle_node("A")
      Y <- tmle_task$get_tmle_node("Y")
      A_ind <- self$factor_to_indicators(A, A_vals) 
      Y_mat <- replicate(length(A_vals), Y)
      
      DR <- (A_ind/g_vals) * (Y_mat - Q_vals) + Q_vals
      colnames(DR)<-paste0("level A=", A_vals)
      
      # set up task for blip
      
      #Type of pseudo-blip:
      blip_type <- self$blip_type
      
      if (blip_type == "blip1") {
        #First category as a reference category:
        blip_outcome <- DR[, -1] - DR[, 1]
      } else if (blip_type == "blip2") {
        #Average of all categories as a reference category:
        blip_outcome <- DR - rowMeans(DR)
      } else if (blip_type == "blip3") {
        #Weighted average of all categories as a reference category:
        blip_outcome <- DR - rowSums(DR * pA)
      } else {
        blip_outcome <- DR
      }
      
      #Predict for each category, for now
      #TO DO: multivariate SL
      blip_fits<-list()
      i=1
      for(i in 1:ncol(blip_outcome)){
        
        new_data<-cbind.data.frame(blip_outcome=blip_outcome[,i], self$V_data(tmle_task))
        # new_data_v<-new_data[fold$validation_set,]
        
        #todo: think about revere here
        blip_tmle_task <- sl3::make_sl3_Task(new_data, covariates=self$V,
                                             outcome="blip_outcome", folds=tmle_task$folds)
        
        
        blib <- make_learner_stack(
          "Lrnr_mean",
          "Lrnr_glm_fast"
        )
        
        metalearner <- make_learner(Lrnr_nnls)
        b_learner <- make_learner(Lrnr_sl, blib, metalearner)
        
        blip_fit<-learner_list$B$train(blip_tmle_task)
        blip_fits <- c(blip_fits, blip_fit)
      }
      
      private$.blip_fits <- blip_fits
    },
    rule = function(tmle_task){
      #todo: think about revere here
      blip_tmle_task <- sl3::make_sl3_Task(self$V_data(tmle_task), covariates=self$V,
                                           outcome=NULL, folds=tmle_task$folds)
      
      blip_fits <- self$blip_fits
      blip_preds <- sapply(blip_fits, function(blip_fit) blip_fit$predict(blip_tmle_task))
      rule <- max.col(blip_preds)
    }
    
  ),
  active = list(
    tmle_task = function(){
      return(private$.tmle_task)
    },
    likelihood = function(){
      return(private$.likelihood)
    },
    cv_fold = function(){
      return(private$.cv_fold)
    },
    V = function(){
      return(private$.V)
    },
    blip_type = function(){
      return(private$.blip_type)
    },
    blip_fits = function(){
      return(private$.blip_fits)
    }
    
  ),
  private = list(
    .tmle_task = NULL,
    .likelihood = NULL,
    .cv_fold = NULL,
    .V = NULL,
    .blip_type = NULL,
    .blip_fits = NULL
  )
)