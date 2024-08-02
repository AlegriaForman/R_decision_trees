# Loading the Data Set (for the Classification Decision Tree)
print("The code section below will first install one R package: rpart.plot.")
print("Please do not move to the next step until the package is fully installed.")
print("This will take some time. Once the installation is complete, this step will print first 6 rows of the data set.")


# Loading package to show the decision tree
install.packages("rpart.plot")

# Loading credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

print("installation completed!")
print("data set (first 6 rows)")
head(credit_default, 6)

# Splitting Data into Training and Testing Sets

set.seed(507690)

# Partition the data set into training and testing data
samp.size = floor(0.60*nrow(credit_default))

# Training set
print("Number of rows for the training set")
train_ind = sample(seq_len(nrow(credit_default)), size = samp.size)
train.data1 = credit_default[train_ind,]
nrow(train.data1)

# Testing set 
print("Number of rows for the validation set")
test.data1 = credit_default[-train_ind,]
nrow(test.data1)

# Constructing the Classification Decision Tree
set.seed(507690)

library(rpart)
model1 <- rpart(default ~ missed_payment + education + age, method="class", data=train.data1, control = rpart.control(minsplit=10))
printcp(model1)

# Cross Validation Error and Cost-Complexity
#plotcp(model) # Visualize cross validation results
plotcp(model1, minline = TRUE, lty = 3, col = 2, upper = c("size", "splits", "none"))

# Pruning the Tree
set.seed(507690)
pruned_model1 <- rpart(default ~ missed_payment + education + age, method="class",  data=train.data1, control = rpart.control(cp = 0.039))
printcp(pruned_model1)

# Plotting the Classification Decision Tree
library(rpart.plot)
rpart.plot(pruned_model1)

# Confusion Matrix
# Make predictions on the test data
pred <- predict(pruned_model1, newdata=test.data1, type='class')

# Construct the confusion matrix
conf.matrix <- table(test.data1$default, pred)[,c('no','yes')]
rownames(conf.matrix) <- paste("Actual default ", rownames(conf.matrix), sep = ": ")
colnames(conf.matrix) <- paste("Prediction default ", colnames(conf.matrix), sep = ": ")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

# Prediction
print("Prediction for defaulting (yes or no): missed_payment='none', education='postgraduate', age=42")
newdata1 <- data.frame(missed_payment='no', education='postgraduate', age=42)
predict(pruned_model1, newdata1, type='class')

print("Prediction for defaulting (yes or no): missed_payment='missed', education='high_school', age=42")
newdata2 <- data.frame(missed_payment='yes', education='high_school', age=42)
predict(pruned_model1, newdata2, type='class')

# Loading the Data Set (for the Regression Decision Tree)
# Load the data set
economic <- read.csv(file='economic.csv', header=TRUE, sep=",")

# Print the first six rows
print("head")
head(economic, 6)

# Splitting Data into Training and Testing Sets
set.seed(507690)

# Partition the data set into training and testing data
samp.size = floor(0.80*nrow(economic))

# Training set
print("Number of rows for the training set")
train_ind = sample(seq_len(nrow(economic)), size = samp.size)
train.data2 = economic[train_ind,]
nrow(train.data2)

# Testing set 
print("Number of rows for the testing set")
test.data2 = economic[-train_ind,]
nrow(test.data2)

# Constructing the Regression Decision Tree
set.seed(507690)

library(rpart)
model2 <- rpart(wage_growth ~ economy + inflation + gdp, method="anova", data=train.data2, control = rpart.control(minsplit=10))
printcp(model2)

# Cross Validation Error and Cost-Complexity
# Visualize cross validation results
plotcp(model2, minline = TRUE, lty = 3, col = 2, upper = c("size", "splits", "none"))

# Pruning the Tree
set.seed(507690)
pruned_model2 <- rpart(wage_growth ~ economy + inflation + gdp, method="anova",  data=train.data2, control = rpart.control(cp = 0.019))
printcp(pruned_model2)

# Plotting the Classification Decision Tree
library(rpart.plot)
rpart.plot(pruned_model2)

# Root Mean Squared Error (RMSE)
# Here is a custom R function to calculate Root Mean Squared Error or RMSE for any regression model. The following function (called RMSE) will calculate the Root Mean Squared Error
# based on the formula shown above.
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}

# Calculate RMSE 
pred <- predict(pruned_model2, newdata=test.data2, type='vector')
print("Root Mean Squared Error")
rmse <- RMSE(pred, test.data2$wage_growth)
round(rmse, 4)

# Prediction

print("Predicted wage growth: economy='no_recession', inflation=2.10, gdp=2.5")
newdata3 <- data.frame(economy='no_recession', inflation=2.10, gdp=2.5)
predicted_wage_growth = predict(pruned_model2, newdata3, type='vector')
round(predicted_wage_growth,4)

print("Predicted wage growth: economy='no_recession', inflation=3.50, gdp=6.8")
newdata4 <- data.frame(economy='no_recession', inflation=3.50, gdp=6.8)
predicted_wage_growth = predict(pruned_model2, newdata4, type='vector')
round(predicted_wage_growth,4)

# This line will load the decision tree
install.packages("rpart.plot")

# This line will Load the data set for credit card default
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

print("The installation is complete")
print("data set (first 10 rows)")
head(credit_default, 10)

set.seed(6751342)

# This line will partition the data set into training and validation data using 70% and 30% split
samp.size = floor(0.70*nrow(credit_default))

# This line is re training set
print("Number of rows for the training set")
train_cust = sample(seq_len(nrow(credit_default)), size = samp.size)
train.cust1 = credit_default[train_cust,]
nrow(train.cust1)

# This line is re validation set 
print("Number of rows for the validation set")
valid.cust1 = credit_default[-train_cust,]
nrow(valid.cust1)

# This line is re total rows in the credit_default.csv
print("Number of rows in the credit_default.csv")
creditdef = credit_default
nrow(creditdef)

set.seed(6751342)

library(rpart)
# This line will show the regression decision tree
decitree1 <- rpart(default ~ missed_payment + credit_utilize + assets, method="class", data=train.cust1, control = rpart.control(minsplit=10))
printcp(decitree1)

# This line will show the plot the validation error against the cost-complexity parameter (cp)
plotcp(decitree1, minline = TRUE, lty = 3, col = 2, upper = c("size", "splits", "none"))

set.seed(6751342)
# This line will prune the tree
pruned_decitree1 <- rpart(default ~ missed_payment + credit_utilize + assets, method="class",  data=train.cust1, control = rpart.control(cp = 0.021))
printcp(pruned_decitree1)

library(rpart.plot)
# This line will show the plotting classification decision tree
rpart.plot(pruned_decitree1)

# This line will create predictions on the validation data
pred_decitree1 <- predict(pruned_decitree1, newdata=valid.cust1, type='class')

# This line will create the confusion matrix
conf.matrix <- table(valid.cust1$default, pred_decitree1)[,c('no','yes')]
rownames(conf.matrix) <- paste("Actual default ", rownames(conf.matrix), sep = ": ")
colnames(conf.matrix) <- paste("Prediction default ", colnames(conf.matrix), sep = ": ")

# This line will format the confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

# This line will print using the classification model for an individual 
# who has not missed payments, owns a car and a house, and has a 30% credit utilization
print("Prediction for defaulting (yes or no): missed_payment='no', assets='car and house', credit utilization=30%")
predmod1 <- data.frame(missed_payment='no', assets='car_house', credit_utilize=0.30)
predict(pruned_decitree1, predmod1, type='class')

# This line will print using the classification model for an individual 
# individual who has missed payments, does not have any assets, and has a 30% credit utilization
print("Prediction for defaulting (yes or no): missed_payment='yes', assets='none', credit utilization=30%")
predmod2 <- data.frame(missed_payment='yes', assets='none', credit_utilize=0.30)
predict(pruned_decitree1, predmod2, type='class')

# 4. Regression Decision Tree
# This line will load the economic data set
economic <- read.csv(file='economic.csv', header=TRUE, sep=",")

# Print the first 10 rows
print("head")
head(economic, 10)

set.seed(6751342)

# This line will split data to training and testing sets from economic.csv using 80% and 20% split
samp.size = floor(0.80*nrow(economic))

# This line will show the training set
print("Number of rows for the training set")
train_eco = sample(seq_len(nrow(economic)), size = samp.size)
train.eco1 = economic[train_eco,]
nrow(train.eco1)

# This line will show the validation set 
print("Number of rows for the validation set")
valid.eco1 = economic[-train_eco,]
nrow(valid.eco1)

# This line will print the total number of rows in the economic.csv
print("Number of rows  in the economic.csv")
econtotnum = economic
nrow(econtotnum)

set.seed(6751342)
# This line will show the regression decision tree
library(rpart)
decitree2 <- rpart(wage_growth ~ economy + unemployment + gdp, method="anova", data=train.eco1, control = rpart.control(minsplit=10))
printcp(decitree2)

# This line will show the plot the validation error against the cost-complexity parameter (cp)
plotcp(decitree2, minline = TRUE, lty = 3, col = 2, upper = c("size", "splits", "none"))

set.seed(6751342)
pruned_decitree2 <- rpart(wage_growth ~ economy + unemployment + gdp, method="anova",  data=train.eco1, control = rpart.control(cp = 0.014))
printcp(pruned_decitree2)

library(rpart.plot)
# This line will show the plotting classification decision tree
rpart.plot(pruned_decitree2)

# This line is the custom R function to calculate Root Mean Squared Error (RMSE) for any regression model. 
#The following function (called RMSE) will calculate the Root Mean Squared Error
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}

# This line will calculate RMSE 
ecopred <- predict(pruned_decitree2, newdata=valid.eco1, type='vector')
print("Root Mean Squared Error")
rmse <- RMSE(ecopred, valid.eco1$wage_growth)
round(rmse, 4)

print("Predicted wage growth: economy='no_recession', unemployment=3.4%, gdp=3.5%")
predecomod1 <- data.frame(economy='no_recession', unemployment=3.40, gdp=3.50)
predicted_wage_growth1 = predict(pruned_decitree2, predecomod1, type='vector')
round(predicted_wage_growth1,4)

print("Predicted wage growth: economy='recession', unemployment=7.4%, gdp=1.4%")
predecomod2 <- data.frame(economy='recession', unemployment=7.40, gdp=1.40)
predicted_wage_growth2 = predict(pruned_decitree2, predecomod2, type='vector')
round(predicted_wage_growth2,4)