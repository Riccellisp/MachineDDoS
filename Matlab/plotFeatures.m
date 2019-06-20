function plotFeatures(FeaturesMetrics)
for i = 1: length(FeaturesMetrics(:,:,:))
aux = FeaturesMetrics(:,:,:);
plot3(aux(1,i),aux(2,i),aux(3,i),'o');

hold on
end
xlabel('entropySourceIPs');
ylabel('VarSourceIPs');
zlabel('PktRate');
% keyboard
end