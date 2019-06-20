library("skimr")
library("corrplot")
library("e1071")
library(ggfortify)
library(ggplot2)
library(caret)
library(doMC)
registerDoMC(cores=8)
runs = 100
dfISCX<- read.csv(file="/home/latin/R/ISCX2017/MachineLearningCVE/CICISCX2017.csv",stringsAsFactors = FALSE)
# TRANSFORMANDO EM NUMERIC
for(columns in 1:78 ){
  dfISCX[,columns] = as.numeric(dfISCX[,columns])
  print(mean(dfISCX[,columns]))
  
}

#Sumario das features do dataset
str(dfISCX)
# Pegando porcentagens de cada classe
round(prop.table(table(dfISCX$Label)) * 100, digits = 5)
# Transformando inf em na
is.na(dfISCX) <-sapply(dfISCX,is.infinite)
# removendo linhas com NA e Inf
indexesNA = which(!complete.cases(dfISCX))
dfISCX <- dfISCX[-indexesNA,]
dfISCX <- na.omit(dfISCX)

# corISCXNA = cor(dfISCX[,1:68])i

# write.csv(dfISCX2, file = "dfISCXWithoutNAandInf.csv")
# dfISCX2<- read.csv(file="/home/latin/dfISCXWithoutNAandInf.csv",stringsAsFactors = FALSE)
# dfISCX2 = dfISCX2[,-1]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfISCX <- as.data.frame(lapply(dfISCX[1:78], normalize))

# #PCA1
# # dfISCXpcor <- prcomp(dfISCX[,1:78], scale = TRUE)
# # summary(dfISCXpcor)
# #PCA2
#  PCAPreprocessParams <- preProcess(dfISCX[,1:78], method=c("center","scale","pca"))
# # # summarize transform parameters
#  print(PCAPreprocessParams)
# # # transform the dataset using the parameters
#  dfISCX <- predict(PCAPreprocessParams, dfISCX[,1:79])
# # summarize the transformed dataset
# summary(PCATransformed)
accuracy <- vector(mode = "numeric",runs);
kappa <- vector(mode = "numeric",runs);
split = 0.6
start_time <- Sys.time()
for(ii in 1:runs ){
  trainIndex <- createDataPartition(as.factor(dfISCX$Label), p=split, list=FALSE)
  data_train <- dfISCX[ trainIndex,]
  data_test <- dfISCX[-trainIndex,]
  
  # train the model
  #model <- train(as.factor(data_train$X78) ~., data=data_train[,1:78], method="knn")
  model <- naiveBayes(data_train[,1:78],as.factor(data_train$Label))
  
  # summarize results
  #print(model)
  #model <- NaiveBayes(as.factor(dfcaesarean[ trainIndex,6]) ~., data=data_train)
  x_test <- data_test[,1:78]
  y_test <- data_test$Label
  predictions <- predict(model, x_test)
  mtx =  confusionMatrix(predictions, as.factor(y_test))
  accuracy[ii] = mtx$overall['Accuracy']
  kappa[ii] = mtx$overall['Kappa']
  print(ii)
}
summary(accuracy)
summary(kappa)

end_time <- Sys.time()
tim <- end_time - start_time
save(accuracy,kappa,tim,file="nbWithoutPCA")