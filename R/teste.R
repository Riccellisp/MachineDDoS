library("skimr")
library("corrplot")
library("e1071")
library(ggfortify)
library(ggplot2)
library(caret)
dfISCX<- read.csv("/home/riccelli/Downloads/ISCX2017/MachineLearningCVE/Friday-WorkingHours-Afternoon-DDos.pcap_ISCX.csv")

# Transformando inf em na
is.na(dfISCX) <-sapply(dfISCX,is.infinite)

# removendo linhas com NA e Inf
indexesNA = which(!complete.cases(dfISCX))
dfISCX <- dfISCX[-indexesNA,]
dfISCX <- na.omit(dfISCX)
corISCXNA = cor(dfISCX)

# Removendo variaveis sem desvio padrão
dfISCX$Bwd.PSH.Flags <- NULL
dfISCX$Bwd.URG.Flags <- NULL
dfISCX$Fwd.URG.Flags <- NULL
dfISCX$CWE.Flag.Count <- NULL
dfISCX$Fwd.Avg.Bytes.Bulk <- NULL
dfISCX$Fwd.Avg.Packets.Bulk <- NULL
dfISCX$Fwd.Avg.Bulk.Rate <- NULL
dfISCX$Bwd.Avg.Bytes.Bulk <- NULL
dfISCX$Bwd.Avg.Bulk.Rate <- NULL
dfISCX$Bwd.Avg.Packets.Bulk <- NULL

traffic <- dfISCX[,-69]
pcor <- prcomp(traffic, scale = TRUE)
summary(pcor)

PCAPreprocessParams <- preProcess(dfISCX[,1:68], method=c("center","scale","pca"))
# summarize transform parameters
print(PCAPreprocessParams)
# transform the dataset using the parameters
PCATransformed <- predict(PCAPreprocessParams, dfISCX[,1:69])
# summarize the transformed dataset
summary(PCATransformed)
PCATransformed = data.frame(PCATransformed,dfISCX[,69])
accuracy <- vector(mode = "numeric",30);
kappa <- vector(mode = "numeric",30);
split = 0.8
start_time <- Sys.time()
 for(ii in 1:30 ){
  # trainIndex <- createDataPartition(PCATransformed$Label, p=split, list=FALSE)
  # data_train <- PCATransformed[ trainIndex,]
  # data_test <- PCATransformed[-trainIndex,2:23]
  
  #train_control <- trainControl(method="boot", number=10)
  # train the model
  #model <- train(as.factor(data_train$Label) ~., data=data_train, method="nb")
  # summarize results
  #print(model)
  
  #model <- NaiveBayes(as.factor(dfcaesarean[ trainIndex,6]) ~., data=data_train)
  x_test <- data_test
  y_test <- data_test$Label
  predictions <- predict(model, x_test)
  mtx =  confusionMatrix(predictions, y_test)
  accuracy[ii] = mtx$overall['Accuracy']
  kappa[ii] = mtx$overall['Kappa']
 }
summary(accuracy)
summary(kappa)

end_time <- Sys.time()
tim <- end_time - start_time