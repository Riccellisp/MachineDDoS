# Lab 1 - Análise de Desempenho
# Aluno: Bruno Riccelli 
# Matrícula: 413727

library("skimr")
library("corrplot")
library("e1071")
#1.a Acesse o site https://archive.ics.uci.edu/ml/datasets.html, escolha um dataset de uma área de
#domínio de sua escolha que esteja listado como um problema de CLASSIFICAÇÃO
#(exceto os que estão nas listagem built-in) para download. Carregue o dataset escolhido via
#arquivo .CSV ou equivalente e imprima alguns dados do inicio, do final e um
#sumário/descrição geral dos dados estatísticos deste dataset. Indique em poucas palavras o
#propósito geral, quantas features, e quantas classes (binária, multi-classes) o dataset possui.
dfcaesarean<- read.csv("/home/riccelli/Downloads/caesarian.csv.arff")
head(dfcaesarean,15)
tail(dfcaesarean,15)
summary(dfcaesarean)
#dataset contendo informações de 80 grávidas e com base em seus atributos iremos
#classificar se o parto foi cesária ou normal.
#5 features

#1.b Gere a matriz de correlação, histogramas e box plots de cada coluna, e scatter plots das
#features, caso o dataset contenha um número elevado de features, escolha as 5 que
#contenham as maiores correlações positivas ou negativas.
corCaesarean = cor(dfcaesarean)

hist(dfcaesarean$X22,col="darkblue", border="black",xlab="Age",ylab="Frequencies",main="Histogram")
abline(v = c(median(dfcaesarean$X22), mean(dfcaesarean$X22)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))

hist(dfcaesarean$X1,col="darkblue", border="black",xlab="Delivery number",ylab="Frequencies",main="Histogram")
abline(v = c(median(dfcaesarean$X1), mean(dfcaesarean$X1)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))

hist(dfcaesarean$X0,col="darkblue", border="black",xlab="Delivery time",ylab="Frequencies",main="Histogram")
abline(v = c(median(dfcaesarean$X0), mean(dfcaesarean$X0)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))

hist(dfcaesarean$X2,col="darkblue", border="black",xlab="Blood of Pressure",ylab="Frequencies",main="Histogram")
abline(v = c(median(dfcaesarean$X2), mean(dfcaesarean$X2)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))

hist(dfcaesarean$X0.1,col="darkblue", border="black",xlab="Heart Problem",ylab="Frequencies",main="Histogram")
abline(v = c(median(dfcaesarean$X0.1), mean(dfcaesarean$X0.1)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))

boxplot(dfcaesarean$X22, dfcaesarean$X1, dfcaesarean$X0, dfcaesarean$X2, dfcaesarean$X0.1,
        main = "Multiple boxplots for comparision",
        at = c(1,2,4,5,6),
        names = c("Age", "Delivery number", "Delivery time", "Blood of Pressure","Heart Problem"),
        las = 2,
        col = c("orange","red","blue","green","black"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)

corrplot(corCaesarean,method = "pie")

#1.c Indique se os dados necessitam de algum pré-processamento (normalização, scaling, skew,
#variáveis categóricas). Justifique sua resposta baseado na letra b).

# a normalização é necessária, pois os valores estão em escalas diferentes

skAge = skewness(dfcaesarean$X22,TRUE,1)
skDlvNumber = skewness(dfcaesarean$X1,TRUE,1)
skDlvTime = skewness(dfcaesarean$X0,TRUE,1)
skPressure= skewness(dfcaesarean$X2,TRUE,1)
skHeart = skewness(dfcaesarean$X0.1,TRUE,1)

# valores positivos indicam que a media é maior que a mediana, a distribuição ta deslocada pra direita

#1.d Leia os 2 (dois) artigos disponibilizados no repositório Gdrive e descreva os seus design
#experimental em poucas palavras. Indique uma alteração de sua escolha que poderia ser
#realizada em cada design caso você fosse replicar/estender cada artigo com seus próprios
#resultados.

# 2.a Design 1: Verifique a performance de um único algoritmo de machine learning no dataset
#escolhido, utilize o esquema randomized train test split 80/20, repita este experimento 100
#vezes, compute a media, mediana, desvio padrão dos resultados. Gere um boxplot com os
#resultados.

library(caret)
library(klaR)
accuracy <- vector(mode = "numeric",100);
kappa <- vector(mode = "numeric",100);
split=0.80
for(ii in 1:100 ){
  trainIndex <- createDataPartition(dfcaesarean$X0.2, p=split, list=FALSE)
  data_train <- dfcaesarean[ trainIndex,1:5]
  data_test <- dfcaesarean[-trainIndex,1:5]
  
  #train_control <- trainControl(method="boot", number=10)
  # train the model
  # model <- train(as.factor(dfcaesarean[ trainIndex,6]) ~., data=data_train, trControl=train_control, method="nb")
   model <- train(as.factor(dfcaesarean[ trainIndex,6]) ~., data=data_train, method="nb")
  # summarize results
  #print(model)
  
  #model <- NaiveBayes(as.factor(dfcaesarean[ trainIndex,6]) ~., data=data_train)
   x_test <- data_test
   y_test <- dfcaesarean[-trainIndex,6]
   predictions <- predict(model, x_test)
   mtx =  confusionMatrix(predictions, y_test)
   accuracy[ii] = mtx$overall['Accuracy']
   kappa[ii] = mtx$overall['Kappa']
}
 summary(accuracy)
 summary(kappa)
 boxplot(accuracy,kappa,
         main = "Multiple boxplots for comparision",
         at = c(1,2),
         names = c("Accuracy ", "Kappa"),
         las = 2,
         col = c("red","blue"),
         border = "brown",
         horizontal = TRUE,
         notch = FALSE
 )
 
#2.b Design 2: Escolha um único item para variação/testes (data transformation technique,
# feature selection/dimensionality reduction, resampling technique, train/test size) e com o
# item escolhido, execute um experimento para medir a performance de um único algoritmo
# de machine learning variando valores do item selecionado. Escolha um gráfico que
# represente seu experimento e plot os resultados. Descreva em poucas linhas seu design
# experimental.
pcor <- prcomp(dfcaesarean, scale = TRUE)
summary(pcor)
# Redução de dimensionalidade não seria uma boa abordagem


proportions <- c(0.6,0.7,0.8,0.9)
acc = vector(mode = "numeric",length(proportions));

for(i in 1:length(proportions)){
  trainIndex <- createDataPartition(dfcaesarean$X0.2, p=proportions[i], list=FALSE)
  data_train <- dfcaesarean[ trainIndex,1:5]
  data_test <- dfcaesarean[-trainIndex,1:5]
  y_test <- dfcaesarean[-trainIndex,6]
  train_control <- trainControl(method="boot", number=100)
  # train the model
  model <- train(as.factor(dfcaesarean[ trainIndex,6]) ~., data=data_train, trControl=train_control, method="nb")
  # summarize results
  #print(model)
  predictions <- predict(model, data_test)
  confMtx  <- confusionMatrix(predictions, y_test)
  acc[i]  <- confMtx$overall['Accuracy']
}

#2.c Com o design escolhido na letra b), compare o desempenho de 2 algoritmos de
# machine learning, plot gráficos com os resultados. Opcional: Utilize um teste estatístico
# para comparar os resultados de cada algoritmo.
proportions <- c(0.6,0.7,0.8,0.9)
acc = vector(mode = "numeric",length(proportions));
accKnn = vector(mode = "numeric",length(proportions));
for(i in 1:length(proportions)){
  trainIndex <- createDataPartition(dfcaesarean$X0.2, p=proportions[i], list=FALSE)
  data_train <- dfcaesarean[ trainIndex,1:5]
  data_test <- dfcaesarean[-trainIndex,1:5]
  y_test <- dfcaesarean[-trainIndex,6]
  train_control <- trainControl(method="boot", number=100)
  # train the model
  model <- train(as.factor(dfcaesarean[ trainIndex,6]) ~., data=data_train, trControl=train_control, method="nb")
  modelKnn <- train(as.factor(dfcaesarean[ trainIndex,6]) ~., data=data_train, trControl=train_control, method="knn")
  # summarize results
  #print(model)
  predictions <- predict(model, data_test)
  predictionsKnn <- predict(modelKnn, data_test)
  confMtx  <- confusionMatrix(predictions, y_test)
  confMtxKnn  <- confusionMatrix(predictionsKnn, y_test)
  acc[i]  <- confMtx$overall['Accuracy']
  accKnn[i]  <- confMtxKnn$overall['Accuracy']
}
boxplot(acc,accKnn,
        main = "Multiple boxplots for comparision",
        at = c(1,2),
        names = c("Accuracy Naive Bayes", "Accuracy Knn"),
        las = 2,
        col = c("red","blue"),
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
testeT = t.test(acc,accKnn,paired=TRUE)
