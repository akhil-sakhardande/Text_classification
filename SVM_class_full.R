# R script to implement SVM on text data & train the model on entire data

#rm(list = ls())
getwd()

# Reading the file
file5 <- read.csv("file_merged_3_months.csv")
#View(file5)
dim(file5)
names(file5)
file5$X <- NULL #Not useful column
names(file5)

# Creating a DTM matrix
#install.packages("RTextTools")
library(RTextTools)
names(file5)
head(file5["CONTENT.TEXT"],2)
file5.mat <- create_matrix(file5["CONTENT.TEXT"])
file5.mat

# Create and train the SVM model
# Configure the training data
nrow(file5)
container <- create_container(file5.mat, file5$VALID, trainSize=1:nrow(file5), virgin=FALSE)

# train a SVM Model
#model <- train_model(container.train, "SVM", kernel="linear", cost=1)
model <- train_model(container, "SVM", kernel="radial", cost=1)
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
