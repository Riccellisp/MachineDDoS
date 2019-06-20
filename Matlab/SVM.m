data=csvread('NoisyData.csv');
% dividindo treinamento e teste
[treinamento,teste ] = holdout( data,70 );
% pegando as colunas e separando as respostas
XTrain=treinamento(:,1:end-1);YTrain=treinamento(:,end);
Xtest=teste(:,1:end-1);Ytest=teste(:,end);
% TReinando e testando
svm_in=fitcsvm(XTrain,YTrain,'KernelFunction','rbf');
svm_out=predict(svm_in, Xtest);
[Fmeasure(5),Accuracy(5)] = confusion_mat(Ytest, svm_out);
Fmeasure_svm=Fmeasure(5)
Accuracy_svm=Accuracy(5)
% Decision boundary
[x1 x2]=meshgrid(min(XTrain(:,1)):0.01:max(XTrain(:,1)),min(XTrain(:,2)):0.01:max(XTrain(:,2)));
Xn=[reshape(x1,1,size(x1,2)*size(x1,1))' reshape(x2,1,size(x2,2)*size(x2,1))'];
yn=predict(svm_in,Xn);
Yn=reshape(yn,size(x1));
DecisionBoundry( XTrain,YTrain,Yn)
title('Decision Boundry due to SVM (rbf)')