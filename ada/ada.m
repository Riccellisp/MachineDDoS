data=csvread('NoisyData.csv');
% dividindo treinamento e teste
[treinamento,teste ] = holdout( data,70 );
% pegando as colunas e separando as respostas
XTrain=treinamento(:,1:end-1);YTrain=treinamento(:,end);
Xtest=teste(:,1:end-1);Ytest=teste(:,end);
% Choose best in maxItr number of iterations
maxItr=100;
fm_=[];
for itr=1:maxItr
    [~,ada_test(:,itr)]= adaboost(XTrain,YTrain, Xtest);
    fm_=[fm_; confusion_mat(Ytest, ada_test(:,itr))];
end
[fm itr]=max(fm_);
ada_out=ada_test(:,itr);
% testando
[Fmeasure(6),Accuracy(6)] = confusion_mat(Ytest, ada_out);
Fmeasure_AdaBoost=Fmeasure(6)
Accuracy_AdaBoost=Accuracy(6)
