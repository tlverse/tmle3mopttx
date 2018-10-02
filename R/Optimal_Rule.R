#' Learns the Optimal Rule given a tmle_task and likelihood
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
    initialize = function(tmle_task, likelihood, cv_fold = "split-specific", V = NULL, blip_type="blip2", blip_library) {
      private$.tmle_task <- tmle_task
      private$.likelihood <- likelihood
      private$.cv_fold <- cv_fold
      private$.blip_type <- blip_type
      private$.blip_library <- blip_library
      
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
    V_data = function(tmle_task, fold=NULL){
      if(is.null(fold)){
        tmle_task$data[,self$V,with=FALSE]
      }else{
        tmle_task$data[,self$V,with=FALSE][tmle_task$folds[[fold]]$training_set,] 
      }
    },
    DR_full = function(v,indx){
      DR<-data.frame(private$.DR_full[[v]])
      return(data.frame(DR[indx,]))
    },
    fit_blip = function(){
      tmle_task <- self$tmle_task
      likelihood <- self$likelihood
      cv_fold <- self$cv_fold
      
      A_vals <- tmle_task$npsem$A$variable_type$levels  
      
      #Generate counterfactual tasks for each value of A:
      cf_tasks <- lapply(A_vals, function(A_val) {
        newdata <- data.table(A=A_val)
        cf_task <- tmle_task$generate_counterfactual_task(UUIDgenerate(),new_data=newdata)
        return(cf_task)
      })
      
      #DR A-IPW mapping of blip
      A <- tmle_task$get_tmle_node("A")
      Y <- tmle_task$get_tmle_node("Y")
      A_ind <- self$factor_to_indicators(A, A_vals) 
      Y_mat <- replicate(length(A_vals), Y)
      
      #TO DO: Add safety net for very small values of g
      if(cv_fold=="split-specific"){
        #Split-specific results:
        n_fold<-length(tmle_task$folds)
        
        #Grab split-specific predictions for all the samples:
        Q_vals_full <- lapply(1:n_fold, function(cv_fd) sapply(cf_tasks, likelihood$get_likelihood, "Y", cv_fd))
        g_vals_full <- lapply(1:n_fold, function(cv_fd) sapply(cf_tasks, likelihood$get_likelihood, "A", cv_fd))
        
        #Grab split-specific predictions for training samples only:
        Q_vals <- lapply(1:n_fold, function(cv_fd) sapply(cf_tasks, likelihood$get_likelihood, "Y", cv_fd)[tmle_task$folds[[cv_fd]]$training_set,] )
        g_vals <- lapply(1:n_fold, function(cv_fd) sapply(cf_tasks, likelihood$get_likelihood, "A", cv_fd)[tmle_task$folds[[cv_fd]]$training_set,] )

      }else{
        #Full or just one fold results:
        n_fold<-1
        Q_vals_full <- list(sapply(cf_tasks, likelihood$get_likelihood, "Y", cv_fold))
        g_vals_full <- list(sapply(cf_tasks, likelihood$get_likelihood, "A", cv_fold))
      }
      
      #List for split-specific
      DR_full <- lapply(1:n_fold,function(i) (A_ind/g_vals_full[[i]]) * (Y_mat - Q_vals_full[[i]]) + Q_vals_full[[i]]) 
      DR <- lapply(1:n_fold,function(i) (A_ind[tmle_task$folds[[i]]$training_set,]/g_vals[[i]]) * (Y_mat[tmle_task$folds[[i]]$training_set,] - Q_vals[[i]]) + Q_vals[[i]]) 

      ######################
      # set up task for blip
      ######################
      
      #Type of pseudo-blip:
      blip_type <- self$blip_type
      
      if (blip_type == "blip1") {
        #First category as a reference category:
        blip_outcome <- lapply(1:n_fold, function(i) DR[[i]][, -1] - DR[[i]][, 1])
        blip_outcome_full <- lapply(1:n_fold, function(i) DR_full[[i]][, -1] - DR_full[[i]][, 1])
      } else if (blip_type == "blip2") {
        #Average of all categories as a reference category:
        blip_outcome <- lapply(1:n_fold, function(i) DR[[i]] - rowMeans(DR[[i]]))
        blip_outcome_full <- lapply(1:n_fold, function(i) DR_full[[i]] - rowMeans(DR_full[[i]]))
      } else if (blip_type == "blip3") {
        #Weighted average of all categories as a reference category:
        blip_outcome <- lapply(1:n_fold, function(i) DR[[i]] - rowSums(DR[[i]] * g_vals[[i]]))
        blip_outcome_full <- lapply(1:n_fold, function(i) DR_full[[i]] - rowSums(DR_full[[i]] * g_vals_full[[i]]))
      } else {
        blip_outcome <- DR
      }
      
      #Predict for each category, for now
      #TO DO: multivariate SL
      #(use only training data)
      blip_fits<-lapply(1:n_fold, function(i){
        #Note that "blip1" will always have one less task here
        lapply(1:ncol(data.frame(blip_outcome[[i]])), function(j) {
          new_data<-cbind.data.frame(blip_outcome=data.frame(blip_outcome[[i]])[,j], self$V_data(tmle_task,i))
          flds<-origami::make_folds(new_data, V=5)
          blip_tmle_task <- sl3::make_sl3_Task(new_data, covariates=self$V, outcome="blip_outcome", folds = flds)
          self$blip_library$train(blip_tmle_task)
        })
      })
      private$.blip_fits <- blip_fits
      private$.DR_full<-blip_outcome_full
    },

    rule = function(tmle_task){
      blip_tmle_task <- sl3::make_sl3_Task(self$V_data(tmle_task), covariates=self$V, outcome=NULL, folds=tmle_task$folds)
      blip_fits <- self$blip_fits
      blip_fin<-matrix(nrow = nrow(blip_tmle_task$data),ncol = length(blip_fits[[1]]))

      for(j in 1:length(blip_fits[[1]])){
        
        #Fold-specific predictions:
        temp<-lapply(1:length(tmle_task$folds), function(v){
          #Note: splits-specific fits used here are generated entirely from the training data
          int<-lapply(1:5, function(fd) {blip_fits[[v]][[j]]$fit_object$cv_fit$fit_object$fold_fits[[fd]]$predict(blip_tmle_task)})
          #Average over split-specific fits per algorithm
          dat<-do.call(cbind,int)
          blip_all<-t(apply(dat, 1, function(x) tapply(x, colnames(dat), mean)))
          blip_all[tmle_task$folds[[v]]$validation_set,]
        })
        #Actual DR:
        temp2<-lapply(1:length(tmle_task$folds), function(v){
          self$DR_full(v,tmle_task$folds[[v]]$validation_set)[,j]
        })
        
        #Average over split-specific fits per algorithm
        blip_pred<-do.call(rbind,temp)
        blip_y<-unlist(temp2)
        
        #Create sl prediction (alpha from validation samples in split-specific Q/g):
        fit_coef <- coef(nnls::nnls(as.matrix(blip_pred), as.matrix(blip_y)))
        
        if(sum(fit_coef)==0){
          fit_coef<-fit_coef
        }else{
          fit_coef <- fit_coef/sum(fit_coef)
        }
        
        #Rearrange:
        pred<-data.frame(pred=as.matrix(blip_pred) %*% fit_coef)
        row.names(pred)<-unlist(lapply(1:length(tmle_task$folds), function(i) tmle_task$folds[[i]]$validation_set))
        pred <- pred[sort(row.names(pred)),]

        blip_fin[,j]<-pred
      }
      
      #TO DO: blip2 for binary? Is max.col ok?
      if(length(blip_fits[[1]])==1){
        rule <- as.numeric(blip_fin > 0)
      }else{
        #Combine all the sl validation samples:
        rule <- max.col(blip_fin)
      }
      
      rule
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
    },
    blip_fits_sl = function(){
      return(private$.blip_fits_sl)
    },
    blip_library = function(){
      return(private$.blip_library)
    }
  ),
  private = list(
    .tmle_task = NULL,
    .likelihood = NULL,
    .cv_fold = NULL,
    .V = NULL,
    .blip_type = NULL,
    .blip_fits = NULL,
    .blip_fits_sl = NULL,
    .blip_library = NULL,
    .DR_full = NULL
  )
)
