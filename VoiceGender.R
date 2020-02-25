dataset = read.csv("voice.csv")

a = which(dataset$mode == 0)
for(i in a){
  dataset[i,]$mode = mean(dataset$mode)
}
sum(which(dataset$mode==0))

ggplot(dataset)+geom_histogram(aes(dataset$modindx),bins = 100)
a = which(dataset$modindx == 0)
for(i in a){
  dataset[i,]$modindx = mean(dataset$modindx)
}
sum(which(dataset$modindx==0))


ggplot(dataset)+geom_histogram(aes(dataset$sd),bins = 200)
a = which(dataset$meanfreq == 0)
for(i in a){
  dataset[i,]$mode = mean(dataset$mode)
}
sum(which(dataset$mode==0))


dataset$label = factor(dataset$label, levels = c("male", "female"),labels = c(0,1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$label, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-21] = scale(training_set[-21])
test_set[-21] = scale(test_set[-21])

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier1 = svm(formula = label ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred1 = predict(classifier1, newdata = test_set[-21])

# Making the Confusion Matrix
cm1 = table(test_set[, 21], y_pred1)
cm1

#Decision Tree
library(rpart)
classifier2 = rpart(formula = label~.,data = training_set)
# Predicting the Test set results
y_pred2 = predict(classifier2, newdata = test_set[-21],type = "class")

# Making the Confusion Matrix
cm2 = table(test_set[, 21], y_pred2)
cm2

# Random Forest
library(randomForest)
classifier3 = randomForest(x = training_set[-21],y = training_set$label,ntree = 150)

# Predicting the Test set results
y_pred3 = predict(classifier3, newdata = test_set[-21])

# Making the Confusion Matrix
cm3 = table(test_set[, 21], y_pred3)
cm3

#KNN
library(class)
y_pred4 = knn(train = training_set[, -21],
             test = test_set[, -21],
             cl = training_set[, 21],
             k = 8,
             prob = TRUE)

# Making the Confusion Matrix
cm4 = table(test_set[, 21], y_pred4)
cm4

library(MLmetrics)
svm = Accuracy(y_pred1,test_set[,21])
decision = Accuracy(y_pred2,test_set[,21])
random = Accuracy(y_pred3,test_set[,21])
knn = Accuracy(y_pred4,test_set[,21])