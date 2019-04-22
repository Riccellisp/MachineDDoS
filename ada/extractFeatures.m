function metrics = extractFeatures(timeByDestCell,sourceIPsByDestCell,destinationChar,pktLenghtByDestCell)
%     calculando a entropia
    entropyByDestFloat = entropy(sourceIPsByDestCell{1});
%     calculando a Variação
    sourceIPsVarByDestFloat = length(unique(sourceIPsByDestCell{1}))/length(sourceIPsByDestCell{1});
%     calculando a taxa de pacotes
    pktRateByDestInt = length(sourceIPsByDestCell{1});
%     replicando o destino para ficar com o mesmo tamanho 
    destinationMtx = repmat(destinationChar,[length(sourceIPsByDestCell{1}) 1]);
%     transformando em cell para criar o dataset
    destinationCell{1} = destinationMtx;
%     replicando a entropia e convertendo e celula
    entropyByDestMtx = repmat(entropyByDestFloat,[length(sourceIPsByDestCell{1}) 1]);
    entropyByDestCell{1} = entropyByDestMtx;
%     replicando a variação e convertendo e celula
    sourceIPsVarByDestMtx = repmat(sourceIPsVarByDestFloat,[length(sourceIPsByDestCell{1}) 1]);
    sourceIPsVarByDestCell{1} =  sourceIPsVarByDestMtx;
%     replicando a taxa de pacotes e convertendo e celula
    pktRateByDestMtx = repmat(pktRateByDestInt,[length(sourceIPsByDestCell{1}) 1]);
    pktRateByDestCell{1} =  pktRateByDestMtx;
    metrics = dataset(timeByDestCell,sourceIPsByDestCell,destinationCell,entropyByDestCell,sourceIPsVarByDestCell,pktRateByDestCell,pktLenghtByDestCell);
%     metrics = [timeByDest sourceIPsByDest{1} destination entropyByDest;...
%     sourceIPsVarByDest pktRateByDest cell2mat(pktLenghtByDest)  ];
end
