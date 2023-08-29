#correlation
#does kendall, spearman and pearson yeild different results?, which is the most accurate
library(neuralnet)
library(caret)
library(ggpubr)
data("Iris") #inbuiltt dataset in R
head(iris) #view first few rows of data
data1 <- data.frame(iris) # put data into dataframe

#Vizualize data using scatter plots
#kendall, spearman, pearson
ggscatter(data1, x = "Sepal.Length", y = "Petal.Length", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Sepal.length", ylab = "Petal.Length")
ggscatter(data1, x = "Sepal.Length", y = "Petal.Length", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Sepal.length", ylab = "Petal.Length")

ggscatter(data1, x = "Sepal.Length", y = "Petal.Length", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Sepal.length", ylab = "Petal.Length")

#correlation between sepal width and petal width

ggscatter(data1, x = "Sepal.Width", y = "Petal.Width", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Sepal.Width", ylab = "Petal.Width")

ggscatter(data1, x = "Sepal.Length", y = "Sepal.Width", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Sepal.Length", ylab = "Sepal.Width")

ggscatter(data1, x = "Petal.Width", y = "Petal.Length", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Petal.Width", ylab = "Petal.Length")

#shapiro-wilk normality test
#H_0 : Data is normally distributed (p>0.05)
#H_A : Data is not normally distributed (p<0.05)

shapiro.test(data1$Sepal.Length)
shapiro.test(data1$Sepal.Width)
shapiro.test(data1$Petal.Length)
shapiro.test(data1$Petal.Width)

#For data that is not normally distributed, Spearman or Kendall Tau are the best options for correlation

#performing correlation tests
#correlation between sepal length and petal length
res <- cor.test(data1$Sepal.Length, data1$Petal.Length, 
                method = "pearson")
res
#the correlation coefficient is 0.8717538 with degrees of freedom 148 a t statistic of 21.646, p value of < 2.2e-16
#at 95% the confidence interval is [0.8270, 0.9055]
