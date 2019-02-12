library(data.table)

#load results
results_from_dir=function(path){
  files=dir(path,full.names=T)
  allresults=lapply(files,function(file){load(file); results})
  results=rbindlist(allresults)
}

results <- results_from_dir("sim1a_fixed_results")
result_cols <- names(results)[3:13]
results[,c(lapply(.SD,mean),n_sim=.N),by=list(type),.SDcols=result_cols]
