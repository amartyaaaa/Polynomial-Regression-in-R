# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
View(dataset)
dataset = dataset[2:3]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
#library(caTools)
#set.seed(123)
#split = sample.split(dataset$Salary, SplitRatio = 0.8)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

#fitting linear reg model to the dataset
lin_reg= lm(formula = Salary ~ Level, data=dataset)
summary(lin_reg)

#fitting the plynomial reg to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~., data = dataset)
summary(poly_reg)

#visualizing linear reg
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Salary vs Level (Dataset)') +
  xlab('Level') +
  ylab('Salary')

#visualizing poly reg
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level2, y = dataset$Salary),
             colour = 'red') +ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Salary vs Level (Dataset)') +
  xlab('Level') +
  ylab('Salary')
  geom_line(aes(x = dataset$Level2, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Salary vs Level2 (Dataset)') +
  xlab('Level') +
  ylab('Salary')
  
  #predicting a new salary using linear reg
  
  y_pred = predict(lin_reg, data.frame(Level = 6.5))
  
# predicting a new salary using poly reg
  
  y_pred1 = predict(poly_reg, data.frame(Level = 6.5, Level2 = 6.5^2, Level3 = 6.5^3, Level4 = 6.5^4))
  y_pred1