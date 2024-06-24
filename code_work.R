# Schrute Farm Final Group Project -----------------------------------------------------------------
# Complete R code with all elaborations ####

# Libraries and packages
# install.packages("corrplot")
# install.packages("yardstick")
# install.packages("randomForest")
# install.packages("class")
# install.packages("xgboost")
# install.packages("rgl")
# install.packages("scatterplot3d")
# install.packages("factoextra")
# install.packages("cluster")
# install.packages("ISLR2")
# install.packages("MASS")
# install.packages("mvtnorm")
# install.packages("ggpubr")
# install.packages("mclust")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("caret")
# install.packages("glmnet")
# install.packages("splines")
# install.packages("Rtsne")
# install.packages("viridis")
# install.packages("dendextend")
# install.packages("colorspace")
# install.packages("tsne")
# install.packages("data.table")
library(data.table)
library(tsne)
library(viridis)
library(dendextend)
library(colorspace)
library(Rtsne)
library(factoextra)
library(cluster)
library(ISLR2)
library(MASS)
library(mvtnorm)
library(ggpubr)
library(mclust)
library(scatterplot3d)
library(yardstick)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(caret)
library(glmnet)
library(randomForest)
library(class)
library(xgboost)
library(splines)

# 1. PREPROCESSING --------------------------------------------------------------------------------------

# Importing the dataset
data_raw <- read.csv("./BrazHousesRent.csv",
                     header = T,
                     sep = ",",
                     stringsAsFactors = T)

# Renaming some columns to be more efficient
colnames(data_raw)[5] <- "parking_spaces"
colnames(data_raw)[9] <- "hoa"
colnames(data_raw)[10] <- "rent_amount"
colnames(data_raw)[11] <- "property_tax"
colnames(data_raw)[12] <- "fire_insurance"

# Structure of the dataset
str(data_raw)

# Replacing missing values: notice that missing values are expressed as "-"
data_raw[data_raw == "-"] <- NA

# Find variable names with null values
null_vars <- colnames(data_raw)[apply(is.na(data_raw), 2, any)]

# Print the variable names
print(null_vars) # this shows that floor is the only variable with null val

# Convert "floor" to character type
data_raw$floor <- as.character(data_raw$floor)

# Replace NA values with "0"
data_raw$floor[is.na(data_raw$floor)] <- "0"

# Convert "floor" back to integer type
data_raw$floor <- as.integer(data_raw$floor)

str(data_raw)

# Search for duplicates based on all variables in the data frame
duplicates <- duplicated(data_raw)

# Create a subset of the data frame without duplicates
data_raw_wd <- subset(data_raw, !duplicates)

# Select the columns to exclude
exclude_columns <- c("city", "animal", "furniture")

# Create a subset of data_raw_wd excluding the specified columns
num_var_wd <- data_raw_wd[, !(names(data_raw_wd) %in% exclude_columns)]
str(num_var_wd)

# Basic stats
(means_vec = apply(X = num_var_wd, MARGIN = 2, FUN = mean))
(median_vec = apply(X = num_var_wd, MARGIN = 2, FUN = median))
(sd_vec = apply(X = num_var_wd, MARGIN = 2, FUN = sd))

# Graphical exploration
# Set up the subplot layout
num_rows <- ceiling(sqrt(ncol(num_var_wd)))
num_cols <- ceiling(ncol(num_var_wd) / num_rows)

# Create a new plotting window
par(mfrow = c(num_rows, num_cols))

# Loop over each numeric variable and create a histogram subplot
for (i in 1:ncol(num_var_wd)) {
  hist(num_var_wd[, i], freq = FALSE, main = names(num_var_wd)[i], col = rgb(.7, .7, .7), border = "white", xlab = "")
  abline(v = means_vec[i], lwd = 2)
  abline(v = median_vec[i], lwd = 2, col = rgb(.7, 0, 0))
  legend("top", c("Mean", "Median"), lwd = 2, col = c(1, rgb(.7, 0, 0)), cex = 0.8, bty = "n")
}

# Reset the plotting environment
par(mfrow = c(1, 1))

## 1.1. Removing outliers ####

# Create a list of numeric variables
numeric_vars <- names(data_raw_wd)[sapply(data_raw_wd, is.numeric)]

# Set the number of rows and columns for the subplot layout
num_rows <- ceiling(sqrt(length(numeric_vars)))
num_cols <- ceiling(length(numeric_vars) / num_rows)

# Set up the plotting environment
par(mfrow = c(num_rows, num_cols), col = "blue")

# Loop over each numeric variable and create a boxplot subplot
for (i in 1:length(numeric_vars)) {
  var <- numeric_vars[i]
  boxplot(data_raw_wd[[var]], horizontal = TRUE, main = var)
}

# Reset the plotting environment
par(mfrow = c(1, 1))

### 1.1.1. IQR exploration ####

# Select the column for which you want to visualize the interval
column <- "fire_insurance"

data <- data_raw_wd

# Calculate the IQR for the column
iqr <- IQR(data[[column]])

# Define the multiple of the IQR for the interval
interval_multiple <- 3

# Calculate the lower and upper bounds of the interval
lower_bound <- quantile(data[[column]], 0.25) - interval_multiple * iqr
upper_bound <- quantile(data[[column]], 0.75) + interval_multiple * iqr

upper_bound

# Create a boxplot with the interval
boxplot(data[[column]], ylim = c(0, max(data[[column]])), col = "lightblue", border = "black", main = "Rent Amount",
        ylab = "Rent Amount", boxwex = 0.5, notch = TRUE)
abline(h = lower_bound, col = "red", lwd = 2)
abline(h = upper_bound, col = "red", lwd = 2)

### 1.1.2. Outliers criteria ####

# Create a logical condition for each outlier criterion
condition_original <- data_raw_wd$area <= 1000 &
  data_raw_wd$rooms <= 10 &
  data_raw_wd$bathroom <= 6 &
  data_raw_wd$hoa <= 15000 &
  data_raw_wd$rent_amount <= 20000 &
  data_raw_wd$property_tax <= 20000 &
  data_raw_wd$fire_insurance <= 500

condition <- data_raw_wd$area <= 1000 & # close to 6*IQR
  data_raw_wd$rooms <= 10 & # close to 6*IQR
  data_raw_wd$bathroom <= 5 & # visual
  data_raw_wd$parking_spaces <= 6 & # close to 3*IQR
  data_raw_wd$floor <= 30 & # close to 3*IQR
  data_raw_wd$hoa <= 10000 & # close to 6*IQR
  data_raw_wd$rent_amount <= 15000 & # close to 3*IQR
  data_raw_wd$property_tax <= 5000 & # visual
  data_raw_wd$fire_insurance <= 250 # close to 4*IQR

condition_2 <- data_raw_wd$area <= 600 & # close to 3*IQR
  data_raw_wd$rooms <= 6 & # close to 3*IQR
  data_raw_wd$bathroom <= 3 & # visual
  data_raw_wd$parking_spaces <= 5 & # close to 3*IQR
  data_raw_wd$floor <= 30 & # close to 3*IQR
  data_raw_wd$hoa <= 5000 & # close to 3*IQR
  data_raw_wd$rent_amount <= 15000 & # close to 3*IQR
  data_raw_wd$property_tax <= 1500 & # close to 3*IQR
  data_raw_wd$fire_insurance <= 220 # close to 3*IQR

# Subset the dataset based on the condition
data_raw_wo <- subset(data_raw_wd, condition_2)

# Number of rows in the dataset without outliers
nrow(data_raw_wo)

# Number of rows removed
nrow(data_raw_wd) - nrow(data_raw_wo)
(nrow(data_raw) - nrow(data_raw_wo))*100/nrow(data_raw_wd)
(nrow(data_raw_wd) - nrow(data_raw_wo))*100/nrow(data_raw_wd)

# Select the columns to exclude
exclude_columns <- c("city", "animal", "furniture")

# Create a subset of data_raw_wd excluding the specified columns
num_var_wo <- data_raw_wo[, !(names(data_raw_wo) %in% exclude_columns)]

# Basic stats
(means_vec = apply(X = num_var_wo, MARGIN = 2, FUN = mean))
(median_vec = apply(X = num_var_wo, MARGIN = 2, FUN = median))
(sd_vec = apply(X = num_var_wo, MARGIN = 2, FUN = sd))

# Graphical exploration
for(i in 1:ncol(num_var_wo)){
  hist(num_var_wo[,i], freq = F, main = names(num_var_wo)[i],col = rgb(.7,.7,.7), border = "white", xlab = "")
  abline(v = means_vec[i], lwd = 2)
  abline(v = median_vec[i], lwd = 2, col = rgb(.7,0,0))
  legend("top", c("Mean", "Median"), lwd = 2, col = c(1, rgb(.7,0,0)),cex = .8, bty = "n")
}

## 1.2. Insights ####

# Histogram to display the distribution of target variable
hist(data_raw_wo$rent_amount, col = "blue", main = "Histogram of Rent Amount", xlab = "Rent Amount", ylab = "Frequency")

# Select only the numeric variables from your dataset
numeric_vars <- data_raw_wo[, sapply(data_raw_wo, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_vars)

# Print the correlation matrix
print(cor_matrix)

# Customize the correlation plot
corrplot(cor_matrix, method = "number", type = "upper", order = "hclust", 
         col = colorRampPalette(c("#FDE725", "#440154"))(100),
         tl.col = "black", tl.srt = 45)

# Add a title to the plot
title(main = "Correlation Matrix of Numeric Variables", line = 2.5)

### 1.2.1. Numeric variables ####

# Select numeric variables from the data_raw_wo dataset
numeric_vars <- sapply(data_raw_wo, is.numeric)
data_numeric <- data_raw_wo[, numeric_vars]

# Calculate the number of rows and columns for the grid
num_vars <- sum(numeric_vars)
num_rows <- ceiling(sqrt(num_vars))
num_cols <- ceiling(num_vars / num_rows)

# Create a list to store the individual histogram plots
hist_plots <- list()

# Iterate over each numeric variable and create a histogram plot
for (col in names(data_numeric)) {
  hist_plots[[col]] <- ggplot(data_raw_wo, aes(x = .data[[col]])) +
    geom_histogram(fill = "steelblue", color = "white") +
    labs(title = col) +
    theme_bw()
}

# Arrange the histogram plots in a grid
grid.arrange(grobs = hist_plots, nrow = num_rows, ncol = num_cols)

### 1.2.2. Binary relationships ####

# Boxplot to show the relation between city and rent amount
boxplot(data_raw_wo$rent_amount ~ data_raw_wo$city, 
        xlab = "City",
        ylab = "Rent Amount",
        main = "Rent Amount by City")

# Boxplot to show the relation between forniture and rent amount
boxplot(data_raw_wo$rent_amount ~ data_raw_wo$furniture, 
        xlab = "Furnished",
        ylab = "Rent Amount",
        main = "Rent Amount by Furniture")

# Boxplot to show the relation between animal and rent amount
boxplot(data_raw_wo$rent_amount ~ data_raw_wo$animal, 
        xlab = "Animal",
        ylab = "Rent Amount",
        main = "Rent Amount by Animal")

### 1.2.3. Ternary relationships ####

# GGPlot 
GGally::ggpairs(data_raw_wo, columns = 10:12)

## 1.3. Creating train and test sets ####

# we divide the entire data set in two partitions (80%, 20%). Then we extract X and y
set.seed(27, sample.kind = "Rounding")
test_index <- createDataPartition(y =data_raw_wo$rent_amount,p = 0.15, list=FALSE)
train <- data_raw_wo[-test_index,] #creating train set -> 8247 rows
test <- data_raw_wo[test_index,] #creating test set -> 2064 rows

# Isolate predictors and target variable in the train set
x_train <- train[, !(names(train) %in% "rent_amount")]
y_train <- train$rent_amount
y_train_sc <- log(y_train)

# Isolate predictors and target variable in the test set
x_test <- test[, !(names(test) %in% "rent_amount")]
y_test <- test$rent_amount
y_test_sc <- log(y_test)

# 2. SCALING --------------------------------------------------------------------------------------

# Create a pre-processing model on the training set to compute mean and standard deviation
preproc_model <- preProcess(x_train, method = c("center", "scale"))

# Apply the pre-processing model to the training set
x_train_sc <- predict(preproc_model, x_train)

# Preview the updated scaled training set
head(x_train_sc)

#--to scale the test set wrt mean and std dev of  training set--#

# Apply the pre-processing model to the test set
x_test_sc <- predict(preproc_model, x_test)

# Preview the updated scaled test set
head(x_test_sc)

## 2.1. Convert factor variables to dummy variables ####

# Create a data frame excluding the factor variables for train set
x_train_sc_numeric <- x_train_sc[, !(names(x_train_sc) %in% c("city", "animal", "furniture"))]

# Create dummy variables for the factor variables
dummy_transform <- dummyVars(~ ., data = x_train_sc[, c("city", "animal", "furniture")])
x_train_dummies <- predict(dummy_transform, newdata = x_train_sc[, c("city", "animal", "furniture")])

# Combine the numeric variables and dummy variables
x_train_final <- cbind(x_train_sc_numeric, x_train_dummies)

# Verify the structure of the final training set
str(x_train_final)

# Create a data frame excluding the factor variables for test set
x_test_sc_numeric <- x_test_sc[, !(names(x_test_sc) %in% c("city", "animal", "furniture"))]

# Create dummy variables for the factor variables using the same transformation
x_test_dummies <- predict(dummy_transform, newdata = x_test_sc[, c("city", "animal", "furniture")])

# Combine the numeric variables and dummy variables
x_test_final <- cbind(x_test_sc_numeric, x_test_dummies)

# Verify the structure of the final test set
str(x_test_final)

# 3. Task 1: REGRESSION MODELS --------------------------------------------------------------------------------------

## 3.1. Linear models --------------------------------------------------------------------------------------

### 3.1.1. Linear regression with all variables scaled data ####
pairs(y_train ~ city + rooms + parking_spaces + furniture + fire_insurance, data = x_train)

mod <- lm(y_train ~ ., data = x_train)
summary(mod) #Adjusted R-squared:  0.9861

### 3.1.2. Linear regression with some variables ####

mod2 <- lm(y_train ~ city + rooms + parking_spaces + furniture + fire_insurance, data = x_train)
summary(mod2) #Adjusted R-squared:  0.9764

### 3.1.3. Linear regression with fire insurance and hoa #### 

mod3 <- lm(y_train ~ hoa + fire_insurance, data = x_train_sc)
summary(mod3) # TRIED FIRE_INSURANCE AND ONE AMONG ALL THE OTHERS PREDICTORS 
# HOA, yields the highest R^2 (0.982)

### 3.1.4. Linear regression with ONLY FIRE INSURANCE ####

mod4 <- lm(y_train ~ fire_insurance, data = x_train)
summary(mod4) #adjusted R-squared:  0.9729

# Predict the response variable
y_pred<- predict(mod)
y_pred2<- predict(mod2)
y_pred3<- predict(mod3)
y_pred4<- predict(mod4)

# Calculate the residuals
residuals<- y_train - y_pred
residuals2<- y_train - y_pred2
residuals3<- y_train - y_pred3
residuals4<- y_train - y_pred4

# Calculate RMSE
rmse<- sqrt(mean(residuals^2))
rmse
rmse2<- sqrt(mean(residuals2^2))
rmse2
rmse3<- sqrt(mean(residuals3^2))
rmse3
rmse4<- sqrt(mean(residuals4^2))
rmse4

## 3.2. Linear models (scaled) --------------------------------------------------------------------------------------

### 3.2.1. Linear regression with all variables scaled data ####
pairs(y_train_sc ~ city + rooms + parking_spaces + furniture + fire_insurance, data = x_train_sc)

mod <- lm(y_train_sc ~ ., data = x_train_sc)
summary(mod) #Adjusted R-squared:  0.886

### 3.2.2. Linear regression with some variables ####

mod2 <- lm(y_train_sc ~ city + rooms + parking_spaces + furniture + fire_insurance, data = x_train_sc)
summary(mod2) #Adjusted R-squared:  0.878 

### 3.2.3. Linear regression with fire insurance and hoa #### 

mod3 <- lm(y_train_sc ~ hoa + fire_insurance, data = x_train_sc)
summary(mod3) # TRIED FIRE_INSURANCE AND ONE AMONG ALL THE OTHERS PREDICTORS 
# HOA, yields the highest R^2 (0.8448)

### 3.2.4. Linear regression with ONLY FIRE INSURANCE ####
mod4 <- lm(y_train_sc ~ fire_insurance, data = x_train_sc)
summary(mod4) #adjusted R-squared:  0.8274

# Predict the response variable
y_pred<- predict(mod)
y_pred2<- predict(mod2)
y_pred3<- predict(mod3)
y_pred4<- predict(mod4)

# Calculate the residuals
residuals<- y_train_sc - y_pred
residuals2<- y_train_sc - y_pred2
residuals3<- y_train_sc - y_pred3
residuals4<- y_train_sc - y_pred4

# Calculate RMSE
rmse<- sqrt(mean(residuals^2))
rmse
rmse2<- sqrt(mean(residuals2^2))
rmse2
rmse3<- sqrt(mean(residuals3^2))
rmse3
rmse4<- sqrt(mean(residuals4^2))
rmse4

## 3.3 AIC ####

lin_fit_aic <- step(lm(y_train ~ 1,
                           data = x_train_sc),
                       scope = list(upper = mod2, # the maximum to consider is a model with all variables
                                    lower = mod4), # the minimum to consider is a model with only fire_insurance
                       direction = c("both", "backward", "forward"),
                       k=2)


anova(mod, lin_fit_aic, test = "Chisq")
rmse_aic<- sqrt(mean(residuals(lin_fit_aic)^2))
rmse_aic

## 3.4. Penalised approaches --------------------------------------------------------------------------------------

### 3.4.1. Ridge regression code 1 TARGET SCALED (RMSE: 0.2802683) ####
# In this version we get the min lambda as the optimal one

# Fit ridge regression model using cross validation
ridge_model <- cv.glmnet(as.matrix(x_train_final), y_train_sc, alpha = 0, nfolds = 10)

# Find the optimal lambda value
lambda_optimal_ridge <- ridge_model$lambda.min

# Predict on the test set using the optimal ridge model
y_pred_ridge <- predict(ridge_model, newx = as.matrix(x_test_final), s = lambda_optimal_ridge)

# Calculate RMSE
rmse_ridge <- sqrt(mean((y_pred_ridge - y_test_sc)^2))

# Print the RMSE value
print(rmse_ridge)

# Fit ridge regression model with the optimal lambda
ridge_model_optimal <- glmnet(as.matrix(x_train_final), y_train_sc, alpha = 0, lambda = lambda_optimal_ridge)

# Predict on the test set using the optimal ridge model
y_pred_ridge_optimal <- predict(ridge_model_optimal, newx = as.matrix(x_test_final))

# Calculate RMSE of optimal model
rmse_ridge_optimal <- sqrt(mean((y_pred_ridge_optimal - y_test_sc)^2))
print(rmse_ridge_optimal)

# Some interesting plots

# Plot the cross-validation curve
plot(ridge_model)

# Plot predicted vs. actual values
plot(y_test_sc, y_pred_ridge_optimal, xlab = "Actual Values", ylab = "Predicted Values", main = "Predicted vs. Actual (Ridge Regression)")
abline(0, 1, col = "red")

# Plot the residuals
residuals <- y_test_sc - y_pred_ridge
plot(residuals, xlab = "Observation", ylab = "Residuals", main = "Residual Plot (Ridge Regression)")
abline(h = 0, col = "red")

### 3.4.2. Ridge regression code 2 TARGET SCALED (RMSE: 0.2723232)#####

my_lam <- seq(from = 0.001, to = 2, length.out = 100)

# Automatic CV
ridge_cv <- cv.glmnet(x = as.matrix(x_train_final),
                      y = y_train_sc, 
                      lambda = my_lam, 
                      alpha = 0,
                      family = "gaussian")

# Directly plot the object returned by cv.glmnet() (fancier)
plot(ridge_cv)  

# The bands represent the upper and lower standard deviation curves.

# Best lambda (minimizing the average MSE over the folds)
ridge_cv$lambda.min

# Preferred best value: largest value of lambda such that error is within 1 
# standard error of the minimum.
ridge_cv$lambda.1se

# In such a way, the regularization is stronger, but we are not so far from the
# global minimum.
(lambda_star <- ridge_cv$lambda.1se) 

# Get the estimated coefficients for the chosen lambda
coef(ridge_cv,
     s = lambda_star)

# Predictions on the test set using the best lambda
predict(ridge_cv, 
        newx = as.matrix(x_test_final),
        s = lambda_star)

# Compute the RMSE using the best lambda value
y_pred_ridge <- predict(ridge_cv, newx = as.matrix(x_test_final), s = ridge_cv$lambda.1se)
rmse_ridge <- sqrt(mean((y_pred_ridge - y_test_sc)^2))

# Print the RMSE
print(rmse_ridge)

### 3.4.3. LASSO regression code 1 TARGET SCALED (RMSE: 0.2707216)#####

# In this version we get the min lambda as the optimal one

lasso_model <- cv.glmnet(as.matrix(x_train_final), y_train_sc, alpha = 1, nfolds = 10)

# Find the optimal lambda value
lambda_optimal_lasso <- lasso_model$lambda.min

# Predict on the test set using the optimal lasso model
y_pred_lasso <- predict(lasso_model, newx = as.matrix(x_test_final), s = lambda_optimal_lasso)

# Calculate RMSE
rmse_lasso <- sqrt(mean((y_pred_lasso - y_test_sc)^2))

# Print the RMSE value
print(rmse_lasso)

# Fit lasso regression model with the optimal lambda
lasso_model_optimal <- glmnet(as.matrix(x_train_final), y_train_sc, alpha = 1, lambda = lambda_optimal_lasso)

# Predict on the test set using the optimal ridge model
y_pred_lasso_optimal <- predict(lasso_model_optimal, newx = as.matrix(x_test_final))

# Calculate RMSE of optimal model
rmse_lasso_optimal <- sqrt(mean((y_pred_lasso_optimal - y_test_sc)^2))
print(rmse_lasso_optimal)

### 3.4.4. LASSO regression code 2 TARGET SCALED (RMSE: 0.2706835)#####

# Automatic CV
lasso_cv <- cv.glmnet(x = as.matrix(x_train_final),
                      y = y_train_sc, 
                      lambda = my_lam, 
                      alpha = 1,
                      family = "gaussian")

# Plot the CV score against lambda
plot(lasso_cv$lambda, 
     lasso_cv$cvm,
     type = "l", 
     lty = 1, 
     ylab = "Error",
     xlab = expression(lambda))
plot(log(lasso_cv$lambda), 
     lasso_cv$cvm,
     type = "l", 
     lty = 1, 
     ylab = "Error",
     xlab = expression(log(lambda)))
# Same as the one computed by hand.

# Directly plot the object returned by cv.glmnet() (fancier)
plot(lasso_cv)    
# The bands represent the upper and lower standard deviation curves.

# Best lambda (minimizing the average MSE over the folds)
lasso_cv$lambda.min

# Preferred best value: largest value of lambda such that error is within 1 
# standard error of the minimum.
lasso_cv$lambda.1se

# In such a way, the regularization is stronger, but we are not so far from the
# global minimum.
(lambda_star <- lasso_cv$lambda.1se) 

# Get the estimated coefficients for the chosen lambda
coef(lasso_cv,
     s = lambda_star)

# Predictions on the test set using the best lambda
predict(lasso_cv, 
        newx = as.matrix(x_test_final),
        s = lambda_star)

# Compute the RMSE using the best lambda value
y_pred_lasso <- predict(lasso_cv, newx = as.matrix(x_test_final), s = lasso_cv$lambda.1se)
rmse_lasso <- sqrt(mean((y_pred_lasso - y_test_sc)^2))

# Print the RMSE
print(rmse_lasso)

# Get the estimated coefficients for the chosen lambda
lasso_coef <- coef(lasso_cv, s = lambda_star)
lasso_coef
# Extract the excluded variables
excluded_vars <- rownames(lasso_coef)[lasso_coef[, 1] == 0]

# Print the excluded variables
print(excluded_vars)

### 3.4.5. Elastic net (RMSE: 0.2722309) ####

# Perform cross-validation for elastic net
elasticnet_cv <- cv.glmnet(x = as.matrix(x_train_final),
                           y = y_train_sc,
                           alpha = 0.5,  # Set the alpha parameter for elastic net (0.5 for equal mix of L1 and L2 penalties)
                           lambda = NULL,  # Set to NULL to let the function determine the lambda values automatically
                           nfolds = 10)  # Set the number of folds for cross-validation

# Find the optimal lambda value
lambda_optimal_enet <- elasticnet_cv$lambda.min

# Fit the elastic net model with the optimal lambda
elasticnet_model <- glmnet(x = as.matrix(x_train_final),
                           y = y_train_sc,
                           alpha = 0.5,  # Set the alpha parameter for elastic net (0.5 for equal mix of L1 and L2 penalties)
                           lambda = lambda_optimal_enet)

# Predict on the test set using the optimal elastic net model
y_pred_enet <- predict(elasticnet_model, newx = as.matrix(x_test_final))

rmse_enet <- sqrt(mean((y_pred_enet - y_test_sc)^2))
print(rmse_enet)

## 3.5. Non-linear models --------------------------------------------------------------------------------------

### 3.5.1. XGBoost (RMSE: 0.081246) ####

# Convert the data to DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(x_train_final), label = y_train_sc)
dtest <- xgb.DMatrix(data = as.matrix(x_test_final), label = y_test_sc)

# Set the parameters for XGBoost
params <- list(
  objective = "reg:squarederror",  # Objective function for regression
  eval_metric = "rmse",            # Evaluation metric
  nrounds = 100,                   # Number of boosting iterations
  early_stopping_rounds = 10,      # Early stopping rounds
  verbose = 0                      # Print messages during training
)

# Perform cross-validation with XGBoost
xgb_cv <- xgb.cv(
  params = params,                 # XGBoost parameters
  data = dtrain,                   # Training data
  nrounds = 100,                   # Number of boosting rounds
  nfold = 5,                       # Number of folds for cross-validation
  stratified = FALSE,              # Not applicable for regression
  print_every_n = 10,              # Print evaluation metrics every 10 rounds
  early_stopping_rounds = 10,      # Early stopping rounds
  maximize = FALSE,                # Whether to maximize the evaluation metric
  prediction = TRUE                # Return predictions for the test data
)

# Get the optimal number of iterations (rounds)
optimal_rounds <- xgb_cv$best_iteration

# Train the final model with the optimal number of iterations
xgb_model <- xgb.train(
  params = params,                 # XGBoost parameters
  data = dtrain,                   # Training data
  nrounds = optimal_rounds,        # Optimal number of iterations
  watchlist = list(train = dtrain, test = dtest),  # Monitoring test set performance
  verbose = 0                      # Print messages during training
)

# Make predictions using the final model
xgb_preds <- predict(xgb_model, dtest)

# RMSE without converting the prediction to original scale
rmse <- sqrt(mean((xgb_preds - y_test_sc)^2))
print(rmse)

# Convert the predictions back to original scale
y_pred <- exp(xgb_preds)

# Calculate RMSE on the test set
rmse <- sqrt(mean((y_pred - y_test)^2))

# Print the RMSE value
print(rmse)

# Some plots on Xgboost

# Train the final model with the optimal number of iterations
xgb_model <- xgb.train(
  params = params,                 # XGBoost parameters
  data = dtrain,                   # Training data
  nrounds = optimal_rounds,        # Optimal number of iterations
  watchlist = list(train = dtrain, test = dtest),  # Monitoring test set performance
  verbose = 0                      # Print messages during training
)

# Compute feature importance
importance <- xgb.importance(model = xgb_model)

# Convert importance matrix to data.table
importance_dt <- as.data.table(importance)

# Plot Feature Importance Barplot
xgb.plot.importance(importance_matrix = importance_dt)

# Spot the most influencing variables

# Train the final model with the optimal number of iterations
xgb_model <- xgb.train(
  params = params,                 # XGBoost parameters
  data = dtrain,                   # Training data
  nrounds = optimal_rounds,        # Optimal number of iterations
  watchlist = list(train = dtrain, test = dtest),  # Monitoring test set performance
  verbose = 0                      # Print messages during training
)

# Plot Feature Importance
xgb.importance(model = xgb_model)  # Compute feature importance

# Plot Feature Importance Barplot
importance <- xgb.importance(model = xgb_model)  # Compute feature importance
xgb.plot.importance(importance_matrix = importance)

# Show Top Variables Affecting Rent Amount
top_features <- importance[order(-importance$Gain), ]$Feature[1:10]  # Select top 10 features
print(top_features)

### 3.5.2. Random Forests (RMSE: 1.092544)#####

# Set the number of folds for cross-validation
num_folds <- 10

# Create an empty list to store the cross-validated predictions
cv_predictions <- list()

# Perform cross-validation
for (fold in 1:num_folds) {
  # Create training and validation indices for the current fold
  fold_indices <- createFolds(y_train_sc, k = num_folds, list = TRUE)
  train_indices <- unlist(fold_indices[-fold])
  validation_indices <- unlist(fold_indices[fold])
  
  # Subset the training and validation data for the current fold
  x_train_fold <- x_train_sc[train_indices, ]
  y_train_fold <- y_train_sc[train_indices]
  x_validation_fold <- x_train_sc[validation_indices, ]
  
  # Fit the random forest model
  rf_model <- randomForest(x = x_train_fold, y = y_train_fold, ntree = 500)
  
  # Predict on the validation set
  fold_predictions <- predict(rf_model, newdata = x_validation_fold)
  
  # Store the fold predictions in the list
  cv_predictions[[fold]] <- fold_predictions
}

# Combine the cross-validated predictions into a single vector
cv_predictions_combined <- do.call(c, cv_predictions)

# Calculate RMSE for the cross-validated predictions
cv_rmse <- sqrt(mean((cv_predictions_combined - y_train_sc)^2))

# Print the cross-validated RMSE
print(cv_rmse)

### 3.5.3. Regression Splines (RMSE: 0.100556068584153) ####

#### B-splines ####

# B-splines with scaled data
fit_bs <- lm(y_train_sc ~ bs(x = fire_insurance, knots = c(50, 100, 150)) + bs(x = rooms, knots = c(50, 100, 150)), data = x_train_final)
summary(fit_bs)

# Predict on training set
predicted_values_bs_train <- predict(fit_bs, newdata = x_train_final)
rmse_bs_train <- sqrt(mean((predicted_values_bs_train - y_train_sc)^2))

# Print RMSE for training set
print(paste("RMSE (B-splines - Training Set):", rmse_bs_train))

# Predict on test set
predicted_values_bs_test <- predict(fit_bs, newdata = x_test_final)
rmse_bs_test <- sqrt(mean((predicted_values_bs_test - y_test_sc)^2))

# Print RMSE for test set
print(paste("RMSE (B-splines - Test Set):", rmse_bs_test))

#### Natural ####

# Natural splines with scaled data
fit_ns <- lm(y_train_sc ~ ns(fire_insurance, df = 3) + ns(hoa, df = 3), data = x_train_final)
summary(fit_ns)

# Predict on training set
predicted_values_ns_train <- predict(fit_ns, newdata = x_train_final)
rmse_ns_train <- sqrt(mean((predicted_values_ns_train - y_train_sc)^2))

# Print RMSE for training set
print(paste("RMSE (Natural splines - Training Set):", rmse_ns_train))

# Predict on test set
predicted_values_ns_test <- predict(fit_ns, newdata = x_test_final)
rmse_ns_test <- sqrt(mean((predicted_values_ns_test - y_test_sc)^2))

# Print RMSE for test set
print(paste("RMSE (Natural splines - Test Set):", rmse_ns_test))

#### Smoothed B-splines ####

# B-splines with scaled data
fit_bs <- lm(y_train_sc ~ bs(x = fire_insurance, knots = c(50, 100, 150)) + bs(x = rooms, knots = c(50, 100, 150)), data = x_train_final)
summary(fit_bs)

# Smoothed B-splines
fit_bs_smooth <- smooth.spline(x = x_train_final$fire_insurance, y = fitted(fit_bs), spar = 0.5)

# Predict on training set
predicted_values_bs_train <- predict(fit_bs_smooth, x_train_final$fire_insurance)$y
rmse_bs_train <- sqrt(mean((predicted_values_bs_train - y_train_sc)^2))

# Print RMSE for training set
print(paste("RMSE (B-splines - Training Set):", rmse_bs_train))

# Predict on test set
predicted_values_bs_test <- predict(fit_bs_smooth, x_test_final$fire_insurance)$y
rmse_bs_test <- sqrt(mean((predicted_values_bs_test - y_test_sc)^2))

# Print RMSE for test set
print(paste("RMSE (B-splines - Test Set):", rmse_bs_test))

#### Smoothed Natural splines ####

# Natural splines with scaled data
fit_ns <- lm(y_train_sc ~ ns(fire_insurance, df = 3) + ns(hoa, df = 3), data = x_train_final)
summary(fit_ns)

# Smoothed natural splines
fit_ns_smooth <- smooth.spline(x = x_train_final$fire_insurance, y = fitted(fit_ns), spar = 0.5)

# Predict on training set
predicted_values_ns_train <- predict(fit_ns_smooth, x_train_final$fire_insurance)$y
rmse_ns_train <- sqrt(mean((predicted_values_ns_train - y_train_sc)^2))

# Print RMSE for training set
print(paste("RMSE (Natural splines - Training Set):", rmse_ns_train))

# Predict on test set
predicted_values_ns_test <- predict(fit_ns_smooth, x_test_final$fire_insurance)$y
rmse_ns_test <- sqrt(mean((predicted_values_ns_test - y_test_sc)^2))

# Print RMSE for test set
print(paste("RMSE (Natural splines - Test Set):", rmse_ns_test))

#### Plotting Smoothed Splines ####

# Create a data frame for plotting
df_grid <- data.frame(fire_insurance = seq(min(x_train_final$fire_insurance), max(x_train_final$fire_insurance), length.out = 100))

# Add predicted values from smoothed B-splines
df_grid$pred_bs <- predict(fit_bs_smooth, df_grid$fire_insurance)$y

# Add predicted values from smoothed natural splines
df_grid$pred_ns <- predict(fit_ns_smooth, df_grid$fire_insurance)$y

# Plotting
p0 <- ggplot(data = x_train_final, aes(x = fire_insurance, y = y_train_sc)) +
  geom_point(color = "blue") +
  xlab("Fire Insurance") +
  ylab("Rent Amount (Scaled)")

p <- p0 +
  geom_line(data = df_grid, mapping = aes(x = fire_insurance, y = pred_bs, color = "B-splines"), lwd = 1) +
  geom_line(data = df_grid, mapping = aes(x = fire_insurance, y = pred_ns, color = "Natural splines"), lwd = 1) +
  scale_colour_manual(name = "Spline", 
                      values = c("B-splines" = "red", "Natural splines" = "green"),
                      labels = c("B-splines", "Natural splines"))

p             

### 3.5.4. K-NN (RMSE: 0.3113073) ####

# Perform k-Nearest Neighbors using cross-validation
knn_model <- train(
  x = x_train_final,
  y = y_train_sc,
  method = "knn",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = data.frame(k = seq(1, 20, by = 2))
)

# Get the optimal k value
k_optimal <- knn_model$bestTune$k

# Predict on the test set using the optimal k
knn_final <- knn(
  train = x_train_final,
  test = x_test_final,
  cl = y_train_sc,
  k = k_optimal
)

# Convert knn_final to numeric
knn_final_numeric <- as.numeric(as.character(knn_final))

# Calculate RMSE
rmse_knn <- sqrt(mean((knn_final_numeric - y_test_sc)^2))

# Print the RMSE value
print(rmse_knn)

# 4. Task 2: CLUSTERING MODELS  --------------------------------------------------------------------------------------

### 4.1. Feature engineering ####
numeric_vars <- sapply(data_raw_wo, is.numeric)
data_clustering <- data_raw_wo[, numeric_vars]

# Print the structure of the data_clustering dataset
str(data_clustering)

# Create a function to plot histograms

plot_histogram <- function(data, variable) {
  hist(data[[variable]],
       main = paste("Histogram of", variable),
       xlab = variable,
       ylab = "Frequency",
       col = "lightblue",
       border = "white")
}

# Loop over each variable in the data_clustering dataset and plot histograms
for (variable in colnames(data_clustering)) {
  plot_histogram(data_clustering, variable)
}

set.seed(23)
# Scaling the data
data_clustering_sc <- scale(data_clustering)
data_clustering_sc <- as.data.frame(data_clustering_sc)

## 4.2. K-Means ####

# Plotting the elbow curve
fviz_nbclust(data_clustering_sc, kmeans, method = "wss")+ labs(subtitle = "WSS - Elbow method")

# Plotting the Avg. Silhouette plot
fviz_nbclust(data_clustering_sc, kmeans, method = "silhouette")+ labs(subtitle = "Silhouette method")

#### 4.2.1. Model with k = 2 ####
km_2 = kmeans(data_clustering_sc, 2, nstart = 1, iter.max = 1e2)# Silhouette coefficients
sil_coeff2 = silhouette(km_2$cluster, dist(data_clustering_sc))
# Visualization of silhouette coefficients
fviz_silhouette(sil_coeff2)

#Visualization of clusters for k =2
fviz_cluster(km_2, data = data_clustering_sc,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             
             geom = "point",
             
             ellipse.type = "convex", 
             
             ggtheme = theme_bw())

#Creating the table for geographical clustering
combined_data <- cbind(data_raw_wo, cluster = km_3$cluster)

city_cluster <- interaction(data_raw_wo$city, km_3$cluster)

obs_counts <- table(city_cluster)
obs_counts


#### 4.2.2. Model with k = 3 ####
km_3 = kmeans(data_clustering_sc, 3, nstart = 1, iter.max = 1e2)

# Silhouette coefficients
sil_coeff3 = silhouette(km_3$cluster, dist(data_clustering_sc))

# Visualization of silhouette coefficients
fviz_silhouette(sil_coeff3)

#  Visualization of clusters for k=3
fviz_cluster(km_2, data = data_clustering_sc,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             
             geom = "point",
             
             ellipse.type = "convex", 
             
             ggtheme = theme_bw())

# WSS (k=2)
km_2$tot.withinss
# WSS (k=3)
km_3$tot.withinss
#Mean features of clusters
cluster_means2 <- km_2$centers
cluster_means2
cluster_means3 <- km_3$centers
cluster_means3

### 4.3. Hierarchical Clustering ####

# Building models
hc_complete = hclust(dist(data_clustering_sc), method = "complete")
hc_single = hclust(dist(data_clustering_sc), method = "single")
hc_ward = hclust(dist(data_clustering_sc), method = "ward.D2")
hc_average = hclust(dist(data_clustering_sc), method = "average")
hc_trans = hclust(as.dist( 1 - cor(t(data_clustering_sc))), method="average")

# Plot the dendrogram hc_complete
plot(hc_complete, labels = FALSE, hang = -1)
rect.hclust(hc_complete, k = 3, border = "red")

# Plot the dendrogram hc_ward
plot(hc_ward, labels = FALSE, hang = -1)
rect.hclust(hc_ward, k = 3, border = "yellow")

# Plot the dendrogram hc_trans
plot(hc_trans, labels = FALSE, hang = -1)
rect.hclust(hc_trans, k = 3, border = "violet")

# Cut tree into 3 groups hc_ward
sub_grp1 <- cutree(hc_ward, k = 3)
# Geographical division for hc_ward
table(sub_grp1,data_raw_wo$city)

# Cut tree into 3 groups hc_trans
sub_grp2 <- cutree(hc_trans, k = 3)
# Geographical division for hc_ward
table(sub_grp2,data_raw_wo$city)

#Visualize clusters
fviz_cluster(list(data = data.matrix(data_clustering_sc), cluster = sub_grp1))
fviz_cluster(list(data = data.matrix(data_clustering_sc), cluster = sub_grp2))