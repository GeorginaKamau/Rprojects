#Recurrent NeuralNetwork
#import libraries needed
library(neuralnet)
library(tidyverse)
#the sepal.length& width, petal length & width are independent variables while species is the dependent variable
#store in dataframe
data <- data.frame(iris)
iris <- iris %>% mutate_if(is.character, as.factor)
#first 5 rows
head(iris)
#view data distribution
summary(iris)
#split data into train set and test set(80:20)
set.seed(25)
data_rows <- floor(0.80 * nrow(iris)) 
train_indices <- sample(c(1:nrow(iris)), data_rows)
train_data <- iris[train_indices,]
#rows that werent selected for train set are put in test set
test_data <- iris[-train_indices,]
#training neural network
NN = neuralnet(
  Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
  data=train_data,
  hidden=c(4,2),
  linear.output = FALSE
)
plot(NN)

#Model evaluation
#predict categories using test dataset
pred <- predict(NN, test_data)
labels <- c("setosa", "versicolor", "virginca")
prediction_label <- data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  select(2) %>%
  unlist()
#the table shows how many flowers were correclt or incorrectly classified
#compares actual species to predicted species
table(test_data$Species, prediction_label)

#covert categorical variables to numerical
check = as.numeric(test_data$Species) == max.col(pred)
#check accuracy of model
accuracy = (sum(check)/nrow(test_data))*100
print(accuracy)
#our model is 93%acurate