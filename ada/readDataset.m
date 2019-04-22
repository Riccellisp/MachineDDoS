size = length(dataset);
% %     lendo arquivos de respostas
    load('labeledFlowsISCX2012.mat')
    truth = readingTruthFiles(ff);
for i= 1:size
    comunication = dataset(i);
    % % pegando os tempos onde a comunicação ocorreu com determinado destino
    timeByDestCell = comunication{1,1}.timeByDestCell;
    %     pegando os IPs que enviaram para o destino
    sourceIPsByDestCell = comunication{1,1}.sourceIPsByDestCell;
    %     pegando o destino naquela janela
    destinationCell = comunication{1,1}.destinationCell;
    % % pegando a entropia da jaanela
    entropyByDestFloat =  comunication{1,1}.entropyByDestCell{1};
    % %  pegando a variaçaao da janela
    sourceIPsVarByDestFloat =  comunication{1,1}.sourceIPsVarByDestCell{1};
    % pegando a tx de pacotes
    pktRateByDestInt = comunication{1,1}.pktRateByDestCell{1};
    
    
    
    
    keyboard
end