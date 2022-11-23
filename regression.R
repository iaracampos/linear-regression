
#rent number 5 , pop number 3
#rent = Bo+ B1 pop + u

library(readxl)
RENTAL <- read_excel("/RENTAL.XLS",col_names = FALSE)
# Declara os objetos 

rent = RENTAL$...5 
pop = RENTAL$...3
# --OBSERVACAO FILTRADA -- 

#pega os dados do dataset, que são menores que 50000
dadosFiltrados= subset(RENTAL, ...3 < 50000)


#numero de linhas 
nObs=nrow(RENTAL) 
#numero de linhas das observações filtradas 

nMenor=nrow(dadosFiltrados )
percentual= (nMenor*100)/nObs;


# --REGRESSÃO  rent = Bo+ B1 pop + u--

regressaoB=lm(rent~pop)
summary(regressaoA)
#INTERPRETAÇÃO: 

# --REGRESSÃO  rent = B1 pop + u--

regressaoD=lm(rent~pop-1)
summary(regressaoB)
#INTERPRETAÇÃO: 

# -- TESTANDO A HIPOTESE --

# HO: O tamanho da população afeta o preço 
# H1: O tamanho da população não afeta o preco 
# Metodo escolhido, p OBSERVADO
# na saida de regressoa o p - value é igual a 0.3194 ou seja aceita-se a hipotese nula



#--INTERVALO DE CONFIANÇA -- 
confint(regressaoB)
confint(regressaoD)

#Comparar o intervalos de confiança 


#--PLOTANDO O GRÁFICO  --
#Gerando os residuos

residuoB= resid(regressaoB)
residuoD=resid(regressaoD)

#funcao p plotar 
#plotando a regressao B
plot(residuoB, main='Resíduo da regressão B',xlab='Observação', ylab= ' Resíduo',
     col='#00576d',bg='#f88914', pch=23)

#plotando a regressao D 
plot(residuoD, main='Resíduo da regressão D',xlab='Observação', ylab= ' Resíduo',
     col='#104305',bg='#0b881e', pch=23)
#explicar a diferença residual 
