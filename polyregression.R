#Author: Muntabir
#Polynomial Regression to create a bluffing detector wether the infromation is truth or bluff

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#We would not need to split the data since the dataset is small
# # Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

#Fitting Linear Regression to the dataset
#Just to compare with the polynomial regression for experiment purpose
lin_reg = lm(formula = Salary ~ .,
             data = dataset) #since we do not have training set or test set that is why we chose to have dataset in data parameter

#Fitting Polynomial Regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = dataset)

#Visualize the linear regression results
#install.packages("ggplot2")
library(ggplot2)
#Step 1: observation point (scatter plot) - Original training data points
#Step 2: regression line (linear line) - predicted regression line which shows how the predicted line fits into original training set
#Step 3: plot title
#step 4: plot level in X and Y axis
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), #here 'aes' means aesthetic function
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Postion of Level') +
  ylab('Salary')

#visualize the polynomial regression results 
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), #here 'aes' means aesthetic function
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Postion of Level') +
  ylab('Salary')

#Predicting a new result with linear Regression
y_pred = predict(lin_reg, data.frame(Level = 6.5))

#Predicting a new result with Polynomial Regression
y_pred = predict(poly_reg, data.frame(Level = 6.5, 
                                      Level2 = 6.5^2,
                                      Level3 = 6.5^3,
                                      Level4 = 6.5^4))