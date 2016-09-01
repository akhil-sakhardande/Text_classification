# R code to execute SVM on Aug files

# Starting the modeling

setwd("/home/asakhardhande04_sm/Aug_first_week")
getwd()
dir()

final_file <- read.csv("TC_final_file_aug_cleaned.csv", header= TRUE)
nrow(final_file)
names(final_file)

final_file_model <- as.data.frame(final_file[,2])
nrow(final_file_model)
class(final_file_model)
names(final_file_model)
names(final_file_model) <- "CONTENT.TEXT"
head(final_file_model,3)
class(final_file_model$CONTENT.TEXT)

#final_file_model <- gsub('[^A-Za-z0-9_ ]', '', final_file_model)
head(final_file_model,3)
head(final_file_model["CONTENT.TEXT"],3)

# create a prediction document term matrix
predMatrix <- create_matrix(final_file_model["CONTENT.TEXT"], originalMatrix=file5_train.mat)
#predMatrix <- create_matrix(final_file_model["CONTENT.TEXT"], originalMatrix=file5.mat)
#trace("create_matrix",edit=T)

# create the corresponding container
predSize = nrow(final_file_model["CONTENT.TEXT"])
predSize
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
predictionContainer

# predict
results <- classify_model(predictionContainer, model)
results
nrow(results)
class(results)

results_YES <- subset(results, results$SVM_LABEL == "Yes")
nrow(results_YES) # 114

results_NO <- subset(results, results$SVM_LABEL == "No")
nrow(results_NO) # 3575
results_NO

# Attaching the results to the main file

dim(final_file)
final_file1 <- cbind(final_file, results)
dim(final_file1)

write.csv(final_file1, file = "TC_final_file_Aug.csv", row.names = FALSE)


