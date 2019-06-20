data=csvread('NoisyData.csv');
% dividindo treinamento e teste
[treinamento,teste ] = holdout( data,70 );
% pegando as colunas e separando as respostas
XTrain=treinamento(:,1:end-1);YTrain=treinamento(:,end);
Xtest=teste(:,1:end-1);Ytest=teste(:,end);
keyboard;
nb_in=fitNaiveBayes(XTrain,YTrain);
nb_out=predict(nb_in, Xtest);
[Fmeasure(6),Accuracy(6)] = confusion_mat(Ytest, nb_out);
Fmeasure_svm=Fmeasure(6)
Accuracy_svm=Accuracy(6)
