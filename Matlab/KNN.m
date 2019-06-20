data=csvread('NoisyData.csv');
keyboard
% dividindo treinamento e teste
[treinamento,teste ] = holdout( data,70 );
% pegando as colunas e separando as respostas
XTrain=treinamento(:,1:end-1);YTrain=treinamento(:,end);
Xtest=teste(:,1:end-1);Ytest=teste(:,end);
% TReinando e testando
knnTrain=fitcknn(XTrain,YTrain,'NumNeighbors',5);
knnTest = predict(knnTrain, Xtest);

% taxa de acertos
[Fmeasure(2),Accuracy(2)] = confusion_mat(Ytest, knnTest);
Fmeasure_knn=Fmeasure(2)

Accuracy_knn=Accuracy(2)

% decision boundary
[x1 x2]=meshgrid(min(XTrain(:,1)):0.01:max(XTrain(:,1)),min(XTrain(:,2)):0.01:max(XTrain(:,2)));
Xn=[reshape(x1,1,size(x1,2)*size(x1,1))' reshape(x2,1,size(x2,2)*size(x2,1))'];
yn=predict(knnTrain,Xn);
Yn=reshape(yn,size(x1));
DecisionBoundry( XTrain,YTrain,Yn )
title('Decision Boundry due to knn')