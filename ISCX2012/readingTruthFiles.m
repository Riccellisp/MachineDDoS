function truth = readingTruthFiles(cc)
for tt= 1:(length(cc.dataroot.TestbedTueJun15_dash_3Flows))
    source{tt} = cc.dataroot.TestbedTueJun15_dash_3Flows{tt}.source.Text;
    destination{tt} = cc.dataroot.TestbedTueJun15_dash_3Flows{tt}.destination.Text;
    appName{tt} = cc.dataroot.TestbedTueJun15_dash_3Flows{tt}.appName.Text;
    %     keyboard
    startTime{tt} =  cc.dataroot.TestbedTueJun15_dash_3Flows{tt}.startDateTime.Text;
    stopTime{tt} = cc.dataroot.TestbedTueJun15_dash_3Flows{tt}.stopDateTime.Text;
    tag{tt} = cc.dataroot.TestbedTueJun15_dash_3Flows{tt}.Tag.Text;
    %     keyboard;
    
end
% keyboard;
startTimeDateTime = datetime(startTime,'Format','yyyy-MM-dd''T''HH:mm:ss');
stopTimeDateTime = datetime(stopTime,'Format','yyyy-MM-dd''T''HH:mm:ss');
% keyboard;
aux = stopTimeDateTime-startTimeDateTime;
aux = seconds(aux);
for u = 1:length(aux)
    duration{u} = aux(u);
end
truth = [source; destination; startTime; stopTime; duration; tag];
truth = truth';
% keyboard;
% fileID = fopen('response.dat','w');
% % fprintf(fileID,'%6s %12s\n','x','exp(x)');
% fprintf(fileID, '%s,%s,%s,%s,%s,%s\n', truth{:});
% fclose(fileID);
end