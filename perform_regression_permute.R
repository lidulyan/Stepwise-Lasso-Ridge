perform_regression_permute <- function(mydtt,Niteration,data_split,type_reg){
  set.seed(0,sample.kind = "Rejection")
  random_seq = unique(sample(20000, Niteration, rep=F))
  number_of_iter <- seq(1:Niteration)
  
  RMSE = 0
  staats_i = as.data.frame(RMSE)
  staats_i$MSE = 0
  staats_i$R = 0
  
  best_model_coef_i = list()
  
  plots <-  list()
  
  selected_formula <- list()
  
  for (i in number_of_iter){
    result <- perform_regression(mydtt, data_split, type_reg, random_seq[i])
    staats_i <- rbind(staats_i, result[[1]])
    best_model_coef_i <-c(best_model_coef_i, result[[2]])
    print(c(i,random_seq[i]))
  }
  return(list(staats_i, best_model_coef_i,random_seq))
}


