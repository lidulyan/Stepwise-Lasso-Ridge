# perform_regression

What does the function do?
1. Splits data into 80-20%
2. Does LOOCV on training set to pick the best hyperparameter model:
   in case of the stepwise, it is the number of predictors that is allowed in the final model (nvmax: data.frame(nvmax = 1:46)) 
   in case of the ridge and lasso regressions is the lambda (lambda : lambda = 10^seq(-3, 3, length = 100)
3. Predicts the result based on the best model chosen on the training set (the lowest RMSE value)


INPUT 
 my_df - is the data
 split - the proportion of the testing/training set (e.g. .80)
 type_regres
   - "leapForward" - forward stepwise
   - "leapBackward" - backward stepwise
   - "ridge" - ridge regression (makes the coefficient close to zero but never 0)
   - "lasso" - lasso regression (makes the coefficients zero of some predictors)
 seed - a random number: different seed values result in different data splits


OUTPUT 
 1. data table with  RMSE, R, MSE values
 2. coefficient values of the best model that made the prediction on the testing set
 3. the best model formula 
 4. the plot of the important predictors
 5. the regression plot of prediction on the testing set
