library("skimr")
library("corrplot")
library("e1071")
library(ggfortify)
library(ggplot2)

dfISCX<- read.csv("ISCX2017/MachineLearningCVE/Friday-WorkingHours-Afternoon-DDos.pcap_ISCX.csv")
head(dfISCX,15)
tail(dfISCX,15)
summary(dfISCX)

corISCX = cor(dfISCX)
corrplot(corISCX, tl.cex=0.001,method = "pie",type = c("lower"))

# Transformando inf em na
is.na(dfISCX) <-sapply(dfISCX,is.infinite)

# removendo linhas com NA e Inf
indexesNA = which(!complete.cases(dfISCX))
dfISCXWithoutNA <- dfISCX[-indexesNA,]
 dfISCXWithoutNA2 <- na.omit(dfISCXWithoutNA)
corISCXNA = cor(dfISCXWithoutNA)

 df = dfISCXWithoutNA[,1:10]
 df1 = dfISCXWithoutNA[,11:20]
 df2 = dfISCXWithoutNA[,21:30]
 df3 = dfISCXWithoutNA[,31:40]
 df4 = dfISCXWithoutNA[,41:50]
 df5 = dfISCXWithoutNA[,51:60]
 df6 = dfISCXWithoutNA[,61:70]
 df7 = dfISCXWithoutNA[,71:79]
 # corIS = cor(df)
 # corrplot(corIS, tl.cex=0.5,method = "pie")
 
 corIS = cor(df)
 corrplot(corIS, tl.cex=0.5,method = "pie")
 
 corIS = cor(df1)
 corrplot(corIS, tl.cex=0.5,method = "pie")
 
 corIS = cor(df2)
 corrplot(corIS, tl.cex=0.5,method = "pie")
 
 corIS = cor(df3)
 corrplot(corIS, tl.cex=0.5,method = "pie")
 df3$Bwd.PSH.Flags <- NULL
 df3$Bwd.URG.Flags <- NULL
 df3$Fwd.URG.Flags <- NULL
 
 corIS = cor(df4)
 corrplot(corIS, tl.cex=0.5,method = "pie")
 df4$CWE.Flag.Count <- NULL
 
 corIS = cor(df5)
 corrplot(corIS, tl.cex=0.5,method = "pie")
 df5$Fwd.Avg.Bytes.Bulk <- NULL
 df5$Fwd.Avg.Packets.Bulk <- NULL
 df5$Fwd.Avg.Bulk.Rate <- NULL
 df5$Bwd.Avg.Bytes.Bulk <- NULL
 
 corIS = cor(df6)
 corrplot(corIS, tl.cex=0.5,method = "pie")
 
 df6$Bwd.Avg.Bulk.Rate <- NULL
 df6$Bwd.Avg.Packets.Bulk <- NULL
 
 
 corIS = cor(df7)
 corrplot(corIS, tl.cex=0.5,method = "pie")
 
 dfISCXWithoutNAAndZeros = data.frame(df,df1,df2,df3,df4,df5,df6,df7)
 
 traffic <- dfISCXWithoutNAAndZeros[,-69]
 pcor <- prcomp(traffic, scale = TRUE)
 summary(pcor)
 
 #FEATURE SELECTION
 #PCA
 #Plotting PCA (Principal Component Analysis)
 dfISCXWithoutNAAndZeros$Label <- as.factor(dfISCXWithoutNAAndZeros$Label)
 autoplot(prcomp(traffic))
 autoplot(prcomp(df), data = dfISCXWithoutNAAndZeros, colour = 'Label')
 autoplot(prcomp(df), data = dfISCXWithoutNAAndZeros, colour = 'Label', shape = FALSE, label.size = 3)
 
 library(cluster)
 autoplot(clara(dfISCXWithoutNAAndZeros[-69], 2))
 
 # autoplot(fanny(dfISCXWithoutNAAndZeros[-69], 2), frame = TRUE)
 
 # Data Pre-Processing With Caret
 
 ##Scale
   # The scale transform calculates the standard deviation for an attribute and divides 
   #each value by that standard deviation.
   
   # load libraries
   library(caret)
   # calculate the pre-process parameters from the dataset
   ScalePreprocessParams <- preProcess(dfISCXWithoutNAAndZeros[,1:68], method=c("scale"))
   # summarize transform parameters
   print(ScalePreprocessParams)
   # transform the dataset using the parameters
   ScaleTransformed <- predict(ScalePreprocessParams, dfISCXWithoutNAAndZeros[,1:68])
   # summarize the transformed dataset
   summary(ScaleTransformed)
 
 ## Center
   # calculate the pre-process parameters from the dataset
   CenterPreprocessParams <- preProcess(dfISCXWithoutNAAndZeros[,1:68], method=c("center"))
   # summarize transform parameters
   print(CenterPreprocessParams)
   # transform the dataset using the parameters
   CenterTransformed <- predict(CenterPreprocessParams, dfISCXWithoutNAAndZeros[,1:68])
   # summarize the transformed dataset
   summary(CenterTransformed)
 
## Standardize
 #Combining the scale and center transforms will standardize your data.
 #Attributes will have a mean value of 0 and a standard deviation of 1.
   # calculate the pre-process parameters from the dataset
   StandPreprocessParams <- preProcess(dfISCXWithoutNAAndZeros[,1:68], method=c("center","scale"))
   # summarize transform parameters
   print(StandPreprocessParams)
   # transform the dataset using the parameters
   StandTransformed <- predict(StandPreprocessParams, dfISCXWithoutNAAndZeros[,1:68])
   # summarize the transformed dataset
   summary(StandTransformed)   

## Normalize
   # Data values can be scaled into the range of [0, 1] which is called normalization.
   # calculate the pre-process parameters from the dataset
   NormPreprocessParams <- preProcess(dfISCXWithoutNAAndZeros[,1:68], method=c("range"))
   # summarize transform parameters
   print(NormPreprocessParams)
   # transform the dataset using the parameters
   NormTransformed <- predict(NormPreprocessParams, dfISCXWithoutNAAndZeros[,1:68])
   # summarize the transformed dataset
   summary(NormTransformed)

## BoxCox
   #When an attribute has a Gaussian-like distribution but is shifted, this is called a skew. 
   #The distribution of an attribute can be shifted to reduce the skew and make it more Gaussian.
   #The BoxCox transform can perform this operation (assumes all values are positive).
   BoxCoxPreprocessParams <- preProcess(dfISCXWithoutNAAndZeros[,1:68], method=c("BoxCox"))
   # summarize transform parameters
   print(BoxCoxPreprocessParams)
   # transform the dataset using the parameters
   BoxCoxTransformed <- predict(BoxCoxPreprocessParams, dfISCXWithoutNAAndZeros[,1:68])
   # summarize the transformed dataset
   summary(BoxCoxTransformed)
   
## Yeo-Johnson Transform
   # Another power-transform like the Box-Cox transform, but it supports raw values that are equal to zero and negative.
   YeoJohnsonPreprocessParams <- preProcess(dfISCXWithoutNAAndZeros[,1:68], method=c("range"))
   # summarize transform parameters
   print(YeoJohnsonPreprocessParams)
   # transform the dataset using the parameters
   YeoJohnsonTransformed <- predict(YeoJohnsonPreprocessParams, dfISCXWithoutNAAndZeros[,1:68])
   # summarize the transformed dataset
   summary(YeoJohnsonTransformed)
   
## Principal Component Analysis
# Transform the data to the principal components. The transform keeps components 
#above the variance threshold (default=0.95) or the number of components can be 
#specified (pcaComp). The result is attributes that are uncorrelated, useful for 
#algorithms like linear and generalized linear regression.   
   # calculate the pre-process parameters from the dataset
   PCAPreprocessParams <- preProcess(dfISCXWithoutNAAndZeros[,1:69], method=c("center","scale","pca"))
   # summarize transform parameters
   print(PCAPreprocessParams)
   # transform the dataset using the parameters
   PCATransformed <- predict(PCAPreprocessParams, dfISCXWithoutNAAndZeros[,1:69])
   # summarize the transformed dataset
   summary(PCATransformed)
      
#CROSS VALIDATION
    proportions <- c(0.6,0.7,0.8,0.9)
   #proportions <- c(0.6)
   accKnn = vector(mode = "numeric",length(proportions));
   start_time <- Sys.time()
    for(i in 1:length(proportions)){
     trainIndex <- createDataPartition(PCATransformed$Label, p=proportions[i], list=FALSE)
     data_train <- PCATransformed[ trainIndex,1:22]
     data_test <- PCATransformed[-trainIndex,2:22]
     y_test <- PCATransformed[-trainIndex,1]
     
     # define training control
     train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
     # train the model
     modelKnn <- train(data_train$Label~., data=data_train, trControl=train_control, method="knn")
     predictions <- predict(modelKnn, data_test)
     confMtx  <- confusionMatrix(predictions, y_test)
     accKnn[i]  <- confMtx$overall['Accuracy']
    }
   end_time <- Sys.time()
   tim <- end_time - start_time
   
 
# # histograms
# 
# hist(dfISCXWithoutNA$Destination.Port,col="darkblue", border="black",xlab="Dest Port",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Destination.Port), mean(dfISCXWithoutNA$Destination.Port)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Flow.Duration,col="darkblue", border="black",xlab="Flow Duration",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Flow.Duration), mean(dfISCXWithoutNA$Flow.Duration)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Total.Fwd.Packets,col="darkblue", border="black",xlab="Fwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Total.Fwd.Packets), mean(dfISCXWithoutNA$Total.Fwd.Packets)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Total.Backward.Packets,col="darkblue", border="black",xlab="Bwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Total.Backward.Packets), mean(dfISCXWithoutNA$Total.Backward.Packets)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Total.Length.of.Fwd.Packets,col="darkblue", border="black",xlab="Length Fwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Total.Length.of.Fwd.Packets), mean(dfISCXWithoutNA$Total.Length.of.Fwd.Packets)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Total.Length.of.Bwd.Packets,col="darkblue", border="black",xlab="Length Bwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Total.Length.of.Bwd.Packets), mean(dfISCXWithoutNA$Total.Length.of.Bwd.Packets)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.Packet.Length.Max,col="darkblue", border="black",xlab="Max Length Fwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.Packet.Length.Max), mean(dfISCXWithoutNA$Fwd.Packet.Length.Max)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.Packet.Length.Min,col="darkblue", border="black",xlab="Min Length Fwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.Packet.Length.Min), mean(dfISCXWithoutNA$Fwd.Packet.Length.Min)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.Packet.Length.Mean,col="darkblue", border="black",xlab="Mean Length Fwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.Packet.Length.Mean), mean(dfISCXWithoutNA$Fwd.Packet.Length.Mean)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.Packet.Length.Std,col="darkblue", border="black",xlab="Std Length Fwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.Packet.Length.Std), mean(dfISCXWithoutNA$Fwd.Packet.Length.Std)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.Packet.Length.Max,col="darkblue", border="black",xlab="Max Length Bwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.Packet.Length.Max), mean(dfISCXWithoutNA$Bwd.Packet.Length.Max)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.Packet.Length.Min,col="darkblue", border="black",xlab="Min Length Bwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.Packet.Length.Min), mean(dfISCXWithoutNA$Bwd.Packet.Length.Min)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.Packet.Length.Mean,col="darkblue", border="black",xlab="Mean Length Bwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.Packet.Length.Mean), mean(dfISCXWithoutNA$Bwd.Packet.Length.Mean)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.Packet.Length.Std,col="darkblue", border="black",xlab="Std Length Bwd Pkts",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.Packet.Length.Std), mean(dfISCXWithoutNA$Bwd.Packet.Length.Std)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Flow.Bytes.s,col="darkblue", border="black",xlab="Flow Bytes s",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Flow.Bytes.s), mean(dfISCXWithoutNA$Flow.Bytes.s)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Flow.Packets.s,col="darkblue", border="black",xlab="Flow Packets s",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Flow.Packets.s), mean(dfISCXWithoutNA$Flow.Packets.s)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Flow.IAT.Mean,col="darkblue", border="black",xlab="Flow IAT Mean s",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Flow.IAT.Mean), mean(dfISCXWithoutNA$Flow.IAT.Mean)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Flow.IAT.Std,col="darkblue", border="black",xlab="Flow IAT Std s",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Flow.IAT.Std), mean(dfISCXWithoutNA$Flow.IAT.Std)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Flow.IAT.Max,col="darkblue", border="black",xlab="Flow IAT Max",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Flow.IAT.Max), mean(dfISCXWithoutNA$Flow.IAT.Max)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Flow.IAT.Min,col="darkblue", border="black",xlab="Flow IAT Min",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Flow.IAT.Min), mean(dfISCXWithoutNA$Flow.IAT.Min)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.IAT.Total,col="darkblue", border="black",xlab="Fwd IAT Total",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.IAT.Total), mean(dfISCXWithoutNA$Fwd.IAT.Total)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.IAT.Mean,col="darkblue", border="black",xlab="Fwd IAT Mean",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.IAT.Mean), mean(dfISCXWithoutNA$Fwd.IAT.Mean)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.IAT.Std,col="darkblue", border="black",xlab="Fwd IAT Std",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.IAT.Std), mean(dfISCXWithoutNA$Fwd.IAT.Std)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.IAT.Max,col="darkblue", border="black",xlab="Fwd IAT Max",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.IAT.Max), mean(dfISCXWithoutNA$Fwd.IAT.Max)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.IAT.Min,col="darkblue", border="black",xlab="Fwd IAT Min",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.IAT.Min), mean(dfISCXWithoutNA$Fwd.IAT.Min)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.IAT.Total,col="darkblue", border="black",xlab="Bwd IAT Total",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.IAT.Total), mean(dfISCXWithoutNA$Bwd.IAT.Total)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.IAT.Mean,col="darkblue", border="black",xlab="Bwd IAT Mean",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.IAT.Mean), mean(dfISCXWithoutNA$Bwd.IAT.Mean)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.IAT.Max,col="darkblue", border="black",xlab="Bwd IAT Max",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.IAT.Max), mean(dfISCXWithoutNA$Bwd.IAT.Max)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.IAT.Min,col="darkblue", border="black",xlab="Bwd IAT Min",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.IAT.Min), mean(dfISCXWithoutNA$Bwd.IAT.Min)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.IAT.Std,col="darkblue", border="black",xlab="Bwd IAT Std",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.IAT.Std), mean(dfISCXWithoutNA$Bwd.IAT.Std)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.PSH.Flags,col="darkblue", border="black",xlab="Fwd PSH Flags",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.PSH.Flags), mean(dfISCXWithoutNA$Fwd.PSH.Flags)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Bwd.PSH.Flags,col="darkblue", border="black",xlab="Bwd PSH Flags",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Bwd.PSH.Flags), mean(dfISCXWithoutNA$Bwd.PSH.Flags)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))
# 
# hist(dfISCXWithoutNA$Fwd.URG.Flags,col="darkblue", border="black",xlab="Fwd URG Flags",ylab="Frequencies",main="Histogram")
# abline(v = c(median(dfISCXWithoutNA$Fwd.URG.Flags), mean(dfISCXWithoutNA$Fwd.URG.Flags)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
# legend(x="topright", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))



