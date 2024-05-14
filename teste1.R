library(ltm)

head(data(LSAT))
descript(LSAT)

#A função rasch() do pacote ltm estima o parâmetro de discriminação. 
#Para definir o parâmetro de discriminação como sendo 1, deve-se utilizar o 
#argumento constraint.
#Esse argumento deve ser fornecido na forma de uma matriz. 
#Assim, para p itens o número p + 1 indica o parâmetro de discriminação. 
#Assim, constraint=cbind(length(LSAT)+1,1) indica que o parâmetro p + 1, 
#que corresponde ao parâmetro de discriminação, dve ser 1.

fit1<-rasch(LSAT,constraint=cbind(length(LSAT)+1,1))
summary(fit1)

coef(fit1,prob=TRUE, order=TRUE)
fit2<-rasch(LSAT) #Sem o argumento constraint, a função estima o parâmetro de discriminação.

#OBS: utilizar ANOVA para comparar modelos.
#Os valores de ˆθ podem ser obtidos com a função factor.scores():

factor.scores(fit2,met="EAP")

#Após a calibração, pode-se utilizar um padrão de respostas qualquer para estimar a habilidade. Basta fornecer o padrão de repostas da seguinte forma:
  
factor.scores(fit2, resp.patterns = rbind(c(1,0,1,0,1), c(NA,1,0,NA,1)))

##Ajuste do modelo com dois parâmetros

#Essa função pode ser especificada como uma fórmula onde o lado esquerdo deve conter os dados e do lado direito a variável latente z1:
  
fit3<-ltm(LSAT~z1)

##ajuste do modelo com três parâmetros

#Será utilizado o conjunto de dados ’saresp manha’ para estimar esse modelo

manha.tpm<-tpm(manha,control=list(optimizer="nlminb"))

#os valores dos parâmetros estimados foram:
  
manha.tpm
manha.prof<-factor.scores(manha.tpm)

#Os valores de theta estimados são:
head(manha.prof$score)


##Gráficos

plot(fit3,legend=T) #CCI para o modelo fit2.

plot(fit2,type='IIC') # curvas de informação do item 

plot(fit3,type='IIC',items=0) #função de informação do teste

plot(factor.scores(fit2,met="EAP")) #plot do objeto que contém a habilidade fornece um gráfico de densidade dessa variável.


##############'saresp noturno’ e ajuste um modelo com 3 parâmetros
############## Pacote 'irtoys'