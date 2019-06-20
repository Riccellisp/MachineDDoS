clear all
timeFuseFloat = 10800; % 3 hours in seconds for brrazilian time comparison
% prefixo do caminho da pasta dos arquivos
prefix = '/home/riccelli/Downloads/ISCXddos/csv/sp15jun_';
% sufixo com a extensão dos arquivos
sufix = '.csv';
% pasta onde os arquivos estão
folder = dir('/home/riccelli/Downloads/ISCXddos/csv/*.csv');
% Número de arquivos ou segundos na pasta(cada arquivo representa 1 segundo)
numberOfFilesInt = numel(folder);
% keyboard
for file = 1947:numberOfFilesInt
%   lendo cada arquivo e colocando em uma variavel Table
    datasetTable = readtable(strcat(prefix,num2str(file.','%05d'),sufix));
%     convertendo em célula
    datasetTableCell = table2cell(datasetTable);
% %     se o arquivo estiver em vazio, não houve comunicação naquele
% segundo
    if (isempty(datasetTableCell))
    continue;
    end
%   Subtraindo 3 horas de fuso horário
    timeZoneFloat = cell2mat([datasetTableCell(:,1)]) - timeFuseFloat;
%     celula contendo todos os IPs origem na janela
    sourceIPsCell = [datasetTableCell(:,2)];
%     celula contendo todos os IPs destino na janela
    destIPsCell = [datasetTableCell(:,3)];
%     celula contendo os IPs destino sem repetições
    uniqueDestIPsCell = unique(destIPsCell);
%     celula contendo os tamanhos dos pacotes enviados
    pktLenghtCell = [datasetTableCell(:,4)];
%   separando o arquivo por destino  
    for dest = 1:length(uniqueDestIPsCell)
%       indices da celula onde o primeiro destino ocorre
        indexDest = find(strcmp(destIPsCell, destIPsCell{dest}));
%       Ips Origem para o destino  
        sourceIPsByDestCell{dest} = sourceIPsCell(indexDest);
%         Tamanhos de pacote enviados ao destino
        pktLenghtByDestCell{dest} = pktLenghtCell(indexDest);
%       Tempos em que cada pacote foi enviado ao destino
        timeByDestCell{dest} = timeZoneFloat(indexDest);
%         função que calcula metricas e salvaa em dataset
        dataset{dest,:} = extractFeatures(timeByDestCell,sourceIPsByDestCell,destIPsCell{dest},pktLenghtByDestCell);
    end
        str = strcat('/home/riccelli/MachineDDoS/ada/mat/','sp15jun_',num2str(file),'s');
        save(str,'dataset','-v7.3');
        clear dataset
%         keyboard;
end
% % fid =fopen('ISCXFirstHour.dat', 'w' );
% fprintf(fid, '%g;%g;%g\n', entropyByDest, sourceIPsVarByDest, pktRateByDest);
% fclose(fid);
%     plotFeatures(metrics);