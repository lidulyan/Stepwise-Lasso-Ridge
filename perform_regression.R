### ### ### ### ### ### ### ### 
### What does the function do?### 
### ### ### ### ### ### ### ### 

# 1. Splits data into 80-20%
# 2. Does LOOCV on training set to pick the best hyperparameter model:
  # in case of the stepwise, it is the number of predictors that is allowed in the final model (nvmax: data.frame(nvmax = 1:46)) 
  # in case of the ridge and lasso regressions is the lambda (lambda : lambda = 10^seq(-3, 3, length = 100)
# 3. Predicts the result based on the best model chosen on the training set (the lowest RMSE value)

### ### ### 
### INPUT ### 
### ### ### 

# my_df - is the data
# split - the proportion of the testing/training set (e.g. .80)
# type_regres
  # - "leapForward" - forward stepwise
  # - "leapBackward" - backward stepwise
  # - "ridge" - ridge regression (makes the coefficient close to zero but never 0)
  # - "lasso" - lasso regression (makes the coefficients zero of some predictors)
# seed - a random number: different seed values result in different data splits


### ### ### 
### OUTPUT ### 
### ### ### 
# 1. data table with  RMSE, R, MSE values
# 2. coefficient values of the best model that made the prediction on the testing set
# 3. the best model formula 
# 4. the plot of the important predictors
# 5. the regression plot of prediction on the testing set


perform_regression <- function(my_df, split, type_regres,seed){
  set.seed(seed,sample.kind = "Rejection")
  name_of_dep_var = colnames(my_df)[1]
  inTraining <- createDataPartition(unlist(my_df[1]), p = split, list = FALSE)
  training <- my_df[inTraining,]
  testing  <- my_df[-inTraining,]
  lambda <- 10^seq(-3, 3, length = 100)
  set.seed(seed)
  if (type_regres == "leapForward"){
    model <- train(
      formula(my_df), data = training, method = type_regres,
      trControl = trainControl("LOOCV"),
      tuneGrid = data.frame(nvmax = 1:46))
    coef_of_the_best_model <- coef(model$finalModel, model$bestTune$nvmax)
    selected_formula <- select(my_df, c(name_of_dep_var,names(w)[2:length(names(w))]))
    selected_fomula <- lm(formula(selected_formula), data = my_df)
    import_predictor_plot <- NULL
    
  } else if (type_regres == "leapBackward"){
    model <- train(
      formula(my_df), data = training, method = type_regres,
      trControl = trainControl("LOOCV"),
      tuneGrid = data.frame(nvmax = 1:46))
    coef_of_the_best_model <- coef(model$finalModel, model$bestTune$nvmax)
    selected_formula <- select(my_df, c(name_of_dep_var,names(w)[2:length(names(w))]))
    selected_fomula <- lm(formula(selected_formula), data = my_df)
    import_predictor_plot <- NULL
    
  } else if (type_regres == "ridge"){
    model <- train(
      formula(my_df), data = training, method = "glmnet",
      trControl = trainControl("LOOCV"),
      tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-3, 3, length = 100)))
    selected_fomula <- model
    # Model coefficients
    coef_of_the_best_model <-  coef(model$finalModel, model$bestTune$lambda)
    import_predictor_plot <- plot(varImp(selected_fomula, scale = FALSE))
  } else if (type_regres == "lasso"){
    model <- train(
      formula(my_df), data = training, method = "glmnet",
      trControl = trainControl("LOOCV"),
      tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-3, 3, length = 100)))
    selected_fomula <- model
    # Model coefficients
    coef_of_the_best_model <-  coef(model$finalModel, model$bestTune$lambda)
    import_predictor_plot <- plot(varImp(selected_fomula, scale = FALSE))
  }
  # Make predictions
  predictions <- selected_fomula %>% predict(testing)
  # Model prediction performance
  result <- data.frame(
    RMSE = RMSE(predictions, unlist(testing[1])),
    MSE = mean((predictions - unlist(testing[1]))^2),
    R = sqrt(R2(predictions, unlist(testing[1])))
  )
  for_plots <-data.frame(predictions, unlist(testing[1]))
  n <- ggplot(for_plots, aes(x = for_plots$unlist.testing.1.., y = for_plots$predictions)) +
    geom_smooth(method = "lm") +
    stat_cor() +
    geom_point() +
    theme_bw()+
    xlim(-3, 1) +
    ylim(-3, 2) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(size=20)) 
  return(list(result, coef_of_the_best_model, selected_fomula, import_predictor_plot,n))
}


