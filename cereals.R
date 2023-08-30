#Correlation & NeuralNetwork
#cereals dataset has the following columns: cereal name, supplier, cold or hot, calories, protein, fat, sodium,fibre, carbo, sugars, potass, vitamins, rating
#most of the columns contain independent variables while the dependent variable is rating
data <- "C:/Users/Georgina/Desktop/cereals.csv" #import file
datas <- read.csv(data)
head(datas)
set.seed(234)

#random sampling
samplesize <- 0.60*nrow(datas)
index <-  sample(seq_len(nrow(datas)), size = samplesize)

#create training and test set for data
train <- datas[index, ]
head(train)

test <-  datas[-index, ]
head(test)

dim(train) <- c(length(train), 1)
dim(train)
dim(test) <- c(length(test), 1)
dim(test)

#scale the data
max <- apply(datas, 2, max)
min <- apply(datas, 2, min)
scaled <- as.data.frame(scale(datas, center = min, scale = max - min))

#Fitting NeuralNetwork
library(neuralnet)

#get layer size
#n_x is number of neurons in input layer n_h is number of neurons in hidden layer while n_y is the number of neurons in output layer
getLayerSize <- function(X, y, hidden_neurons, train=TRUE) {
  n_x <- dim(X)[1]
  n_h <- hidden_neurons
  n_y <- dim(y)[1]   
  
  size <- list("n_x" = n_x,
               "n_h" = n_h,
               "n_y" = n_y)
  
  return(size)
}
layer_size <- getLayerSize(X_train, y_train, hidden_neurons = 4)
layer_size


#fitting

NN <- neuralnet(rating~calories+protein+fat+sodium+fiber, train,data=datas, hidden = 3, linear.output = T)
#plot neural network
plot(NN)

#correlation, p value is < 0.05
#does amount of sugar affect rating
#H_0 : There is no linear relationship between sugars and rating
#H_a: There is a linear relattionship between sugars and rating

res <- cor.test(datas$sugars, datas$rating, 
                method = "pearson")

res

#p-value is 1.006e-15
#we reject the null hypothesis in favor of the alternative
#the correlation is statistically significant



#does fiber content affect rating
#H_0 : There is no linear relationship between fiber and rating
#H_a: There is a linear relattionship between fiber and rating

res <- cor.test(datas$fiber, datas$rating, 
                method = "kendall")
res

#p-value is 5.557e-06
#we reject the null hypothesis in favor of the alternative
#the correlation is statistically significant


#does calorie content affect rating
#H_0 : There is no linear relationship between calories and rating
#H_a: There is a linear relattionship between calories and rating

res <- cor.test(datas$calories, datas$fiber, 
                method = "pearson")
res

# p-value is 0.009602
#we reject the null hypothesis in favor of the alternative
#the correlation is statistically significant
