library("skimr")
library("corrplot")
library("e1071")
library(ggfortify)
library(ggplot2)
library(caret)
library(Metrics)
library(class)
# install.packages("splitstackshape")
library(splitstackshape)
runs = 30
## LEMBRAR DE FAZER O MINMAX SCALING ANTES DE TREINAR OS ALGORITMOS
dfISCX<- read.csv(file="/home/riccelli/Downloads/export_dataframe5PercentCleanedPCA.csv",stringsAsFactors = FALSE)
#Sumario das features do dataset
str(dfISCX)
# Pegando porcentagens de cada classe
round(prop.table(table(dfISCX$X25)) * 100, digits = 5)
# Transformando inf em na
is.na(dfISCX) <-sapply(dfISCX,is.infinite)
# removendo linhas com NA e Inf
indexesNA = which(!complete.cases(dfISCX))
# dfISCX <- dfISCX[-indexesNA,]
dfISCX <- na.omit(dfISCX)

# TRANSFORMANDO EM NUMERIC
for(columns in 1:25 ){
  # dfISCX[,columns] = as.numeric(dfISCX[,columns])
  print(mean(dfISCX[,columns]))
}
# MIMMAX Scaling
prePro <- preProcess(dfISCX[,1:25], method = "range")
dfISCX[,1:25] = predict(prePro,dfISCX[,1:25])
#CRIANDO RANGE DE SPLITS
# splits = seq(0.6, 0.9, by=0.1)
splits =0.9

# CRIANDO MATRIX DE ACURACIA RUNS X SPLITS
acc <- matrix(nrow = runs, ncol = length(splits))
# CRIANDO MATRIX DE F1 RUNS X SPLITS
f11 <- matrix(nrow = runs, ncol = length(splits))
# CRIANDO MATRIX DE RECALL RUNS X SPLITS
recalll <- matrix(nrow = runs, ncol = length(splits))
# Criando vetor de tempo
tim <- vector(mode = "numeric",length(splits));
# RODANDO OS SPLITS
for (split in 1:length(splits)) {
  # PEGANDO TEMPO INICIAL  
  start_time <- Sys.time()
  #RODANDO AS REPETIÇÕES
  for(ii in 1:runs ){
    # Gerando dados estratificados de treino e teste com elementos de todas as classes
    data = stratified(dfISCX, "X25", splits[split], bothSets = TRUE, select = list(X25 = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")))
    #data = stratified(dfISCX, "X25", 0.6, bothSets = TRUE, select = list(X25 = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")))
    # Pegando dados de treino
    data_train <- data$SAMP1
    #Pegando dados de teste
    data_test <- data$SAMP2
    # Separando X teste e Y teste
    x_test <- data_test[,1:25]
    y_test <- data_test$X25
    # Aplicando knn com K otimo encontrado no final do codigo
    predictions <- knn(data_train[,1:25], x_test,as.factor(data_train$X25),k=3)
    # Matriz de acuracia
    acc[ii,split] = accuracy(y_test,predictions) 
    # Matriz de F1
    f11[ii,split] = f1(y_test,predictions)
    # Matriz de Recall
    recalll[ii,split] = recall(y_test,as.numeric(predictions))
    #Mostrando a rodada Atual
    print(ii)
  }
  #Pegando tempo final do split
  end_time <- Sys.time()
  # Vetor de tempos 
  tim[split] <- end_time - start_time
}
#Salvando em arquivo
save(acc,f11,recalll,tim,file="knnStratified5Percent50Runs")


###### DESCOBRIR K Otimizado ######################
# i=1                          # declaration to initiate for loop
# k.optm=1                     # declaration to initiate for loop
# for (i in 1:30){ 
#   knn.mod <-  knn(train=data_train[,1:25], test=data_test[,1:25], cl=data_train$X25, k=i)
#   k.optm[i] <- 100 * sum(data_test$X25 == knn.mod)/NROW(data_test$X25)
#   k=i  
#   cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
# }
##### DESCOBRIR K Otimizado ########################