# perform_regression

What does the function do?
1. Splits data into 80-20%
2. Does LOOCV on training set to pick the best hyperparameter model:
   in case of the stepwise, it is the number of predictors that is allowed in the final model (nvmax: data.frame(nvmax = 1:46)) 
   in case of the ridge and lasso regressions is the lambda (lambda : lambda = 10^seq(-3, 3, length = 100)
3. Predicts the result based on the best model chosen on the training set (the lowest RMSE value)


INPUT
1. my_df - is the data
2. split - the proportion of the testing/training set (e.g. .80)
3. type_regres
   - "leapForward" - forward stepwise
   - "leapBackward" - backward stepwise
   - "ridge" - ridge regression (makes the coefficient close to zero but never 0)
   - "lasso" - lasso regression (makes the coefficients zero of some predictors)
4. seed - a random number: different seed values result in different data splits


OUTPUT 
 1. data table with  RMSE, R, MSE values
 2. coefficient values of the best model that made the prediction on the testing set
 3. the best model formula 
 4. the plot of the important predictors
 5. the regression plot of prediction on the testing set


# perform_regression_permute
What does the function do?
1. Does perform_regression function 
2. Repeates it N times with different splits by varying the seed

INPUT
1. mydtt - is the data
2. Niteration - how many times you want to permute
3. data_split - the proportion of the testing/training set (e.g. .80)
4. type_regres
   - "leapForward" - forward stepwise
   - "leapBackward" - backward stepwise
   - "ridge" - ridge regression (makes the coefficient close to zero but never 0)
   - "lasso" - lasso regression (makes the coefficients zero of some predictors)
 
OUTPUT 
1. data table with  RMSE, R, MSE values
2. coefficient values of the best model that made the prediction on the testing set (there will be best model per each permutation)
3. the array of seeds used to split data
