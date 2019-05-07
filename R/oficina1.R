# Lab 1 - Análise de Desempenho
# Aluno: Bruno Riccelli 
# Matrícula: 413727
age = 27;
# 1. Criando o vetor base (ou lista, numpy array) :
# 1a. Escolha e exiba um único número aleatório inteiro n entre 100 e 500, este numero será o
#tamanho de seu vetor na próxima questão.
n = floor(runif(1,100,501));
n
# 1b. Crie uma variável chamada vetor que conterá n itens (calculado na letra anterior), 
#sendo que cada elemento deste vetor é a multiplicação da idade do aluno por um número 
#aleatório inteiro (random number) qualquer compreendido entre 1 e 500 
#(Sem que o número aleatório se repita) 
vetor= age * sample(1:500,n,0);
sizeUniqueVtr = length(unique(vetor));
if(sizeUniqueVtr == length (vetor)){
  print("deu bom")
}else{
  print("deu ruim")
}
lowerValue= min(vetor);
upperValue = max(vetor);
# 1c. Exiba o conteúdo final da variável vetor.
vetor

#2. Obtendo informações sobre seus dados:
#2a. Calcule e exiba a quantidade de elementos, media, mediana e variância de sua lista 
#numérica vetor.
media = mean(vetor);
media
mediana = median(vetor);
mediana
variancia = var(vetor);
variancia
#2b. Gere um histograma de seu vetor e compare visualmente seu histograma com os valores
#obtidos na letra anterior.
hist(vetor,col="darkblue", border="black",xlab="valores do vetor",ylab="Frenquencia",main="Histogram")
abline(v = c(median(vetor), mean(vetor)),col = c("orange", "red"),lwd = c(3,3),lty=c(3,3));
legend(x="topleft", c("Mediana","Media"),col=c("orange","red"),lty=c(3,3),lwd=c(1,1))

plot(vetor,1:n,"h",xlab="valores do vetor",ylab="Numero de valores")

#2c. Caso este vetor fosse fruto de um experimento para análise de desempenho, você 
#utilizaria a media ou a mediana na discussão de seus dados? E quanto as outras medidas 
#do item a), o que eles dizem sobre esta distribuição de dados? Justifique sua resposta.
# R.
#Utilizaria nesse caso a média, pois a mesma consegue capturar um valor médio sem sofrer com os outliers
#(valores muito dispersos). Prova desse fato pode ser encontrada no histograma, onde pode-se observar que
#media e mediana possuem valores aproximados. Assim, a mediana, que por definição tende a não sofrer
#impacto dos outliers possui nesse experimento valor aproximado a media. Vale ressaltar que essa medida 
#possui complexidade maior comparada com a média.
# A média mostra o ponto onde ocorre um balanceamento entre os valores de uma  distribuição que pode ser observada no histograma
# Já a mediana mostra o valor central em uma distribuição
# A variancia mostra o grau de dispersão entre os valores e a média
