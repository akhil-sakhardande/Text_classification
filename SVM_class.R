# R script to implement SVM on text data

rm(list = ls())
getwd()

# Reading the file
file5 <- read.csv("file_merged_3_months.csv")
#View(file5)
dim(file5)
names(file5)
file5$X <- NULL #Not useful column
names(file5)

# Sampling - Random sampling
# library(caTools)
# train_rows <- sample.split(file5$VALID, SplitRatio=0.8)
# train_rows
# file5_train <- file5[train_rows,]
# file5_test <- file5[!train_rows,]
# nrow(file5_train)
# nrow(file5_test)

#Sampling - Stratified sampling
install.packages("caret")
library(caret)
set.seed(998)
index <- createDataPartition(file5$VALID, p = .75, list = FALSE)
index
file5_train <- file5[index,]
file5_test <- file5[!index,]
nrow(file5_train)
nrow(file5_test)

# Checking the value distribution (Records in the sample sets)
nrow(subset(file5_train, file5_train$VALID == "Yes")) # 49
nrow(subset(file5_train, file5_train$VALID == "No")) # 122

nrow(subset(file5_test, file5_test$VALID == "Yes")) # 21
nrow(subset(file5_test, file5_test$VALID == "No")) # 53

# Creating a DTM matrix
#install.packages("RTextTools")
library(RTextTools)
names(file5_train)
head(file5_train["CONTENT.TEXT"],2)
file5_train.mat <- create_matrix(file5_train["CONTENT.TEXT"])
file5_train.mat

# Create and train the SVM model
# Configure the training data
nrow(file5_train)
container.train <- create_container(file5_train.mat, file5_train$VALID, trainSize=1:nrow(file5_train), virgin=FALSE)

# train a SVM Model
#model <- train_model(container.train, "SVM", kernel="linear", cost=1)
model <- train_model(container.train, "SVM", kernel="radial", cost=1)
model

# create a prediction document term matrix
predMatrix <- create_matrix(file5_test["CONTENT.TEXT"], originalMatrix=file5_train.mat)
#trace("create_matrix",edit=T)

# create the corresponding container
predSize = nrow(file5_test["CONTENT.TEXT"])
predSize
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
predictionContainer

# predict
results <- classify_model(predictionContainer, model)
results

#table(results$SVM_LABEL, file5_test$VALID)
table(results$SVM_LABEL, file5_test$VALID, dnn = c("Predicted", "Actual"))
d <- table(results$SVM_LABEL, file5_test$VALID, dnn = c("Predicted", "Actual"))

# Model accuracy
model_accuracy <- sum(diag(d))/sum(d)
model_accuracy
model_incorrect_class <- 1-sum(diag(d))/sum(d)
model_incorrect_class

4/35 # Type I error = 0
4/14 # Type II error = 100

# Tuning the SVM algorithm

#cost = 2^(2:8), kernel = "linear",
svm_tune <- tune.svm(VALID ~ ., data = file5_train, cost=10^(-1:2), gamma=c(.5,1,2), kernel = "radial")
#svm_tune <- tune.svm(VALID ~ ., data = file5_train, cost = 2^(2:8), gamma=c(.5,1,2), kernel = "radial")
print(svm_tune)
#svm_tune <- tune.svm(file5_train$VALID ~ ., data = file5_train, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2))) 

model_after_tune <- svm(VALID ~ ., data = file5_train, kernel="radial", cost=10, gamma=0.5)
model_after_tune
pred <- predict(model_after_tune, file5_test[,c(1:4)])
pred
table(pred, file5_test$VALID)
table(pred, file5_test$VALID, dnn = c("Predicted", "Actual"))
d <- table(pred, file5_test$VALID, dnn = c("Predicted", "Actual"))

# Model accuracy
model_accuracy <- sum(diag(d))/sum(d)
model_accuracy
model_incorrect_class <- 1-sum(diag(d))/sum(d)
model_incorrect_class
