#NeuralNetworks
set.seed(69)
library(neuralnet)
library(caret)
head(iris) #inbuilt R dataset
#the sepal.length& width, petal length & width are independent variables while species is the dependent variable
#put in dataframe
data <- data.frame(iris)
#shuffle data for generalization
data <- data[sample(nrow(data)), ]
head(data)

hist(data$Sepal.Length)

#scaling data using min-max normalization, data will all range from 0 - 1

# Min-Max Normalization for sepal.length
data$Sepal.Length <- (data$Sepal.Length - min(data$Sepal.Length)) / (max(data$Sepal.Length) - min(data$Sepal.Length))
hist(data$Sepal.Length)
# Min-Max Normalization for sepal.width
data$Sepal.Width <- (data$Sepal.Width - min(data$Sepal.Width)) / (max(data$Sepal.Width) - min(data$Sepal.Width))
hist(data$Sepal.Width)
# Min-Max Normalization for petal.length
data$Petal.Length <- (data$Petal.Length - min(data$Petal.Length)) / (max(data$Petal.Length) - min(data$Petal.Length))
hist(data$Petal.Length)
# Min-Max Normalization for petal.width
data$Petal.Width <- (data$Petal.Width - min(data$Petal.Width)) / (max(data$Petal.Width) - min(data$Petal.Width))
hist(data$Petal.Width)

#split data into train and test 
train_test_split_index <- 0.8 * nrow(data)
#80% train 
train <- data[1:train_test_split_index,]
head(train)
dim(train) #dimension of train data
#& 20% test
test <- data[(train_test_split_index+1): nrow(data),]
head(test)
dim(test) #size of test data

#fitting neural network 
set.seed(28)
#neuralnetwork with 5 hidden layers , 2 repetions and a maximum of 100000 steps
NN <- neuralnet(Species~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
               data = train,
               hidden = 4,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = 'full',
               algorithm = "rprop+",
               stepmax = 10000)

plot(NN)
# error
NN$result.matrix
# Prediction of test data
predict_testNN <- compute(NN, test[,c(1:4)])
predict_testNN

