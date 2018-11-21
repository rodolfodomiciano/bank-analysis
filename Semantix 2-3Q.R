#1. Title: Bank Marketing

#4. Relevant Information:

#The data is related with direct marketing campaigns of a Portuguese banking institution. 
#The marketing campaigns were based on phone calls. Often, more than one contact to the same client was required, 
#in order to access if the product (bank term deposit) would be (or not) subscribed. 

#The classification goal is to predict if the client will subscribe a term deposit (variable y).

#5. Number of Instances: 45211 for bank-full.csv

#6. Number of Attributes: 16 + output attribute.

#7. Attribute information:

#Input variables:
# bank client data:
#1 - age (numeric)
#2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
#"blue-collar","self-employed","retired","technician","services") 
#3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
#4 - education (categorical: "unknown","secondary","primary","tertiary")
#5 - default: has credit in default? (binary: "yes","no")
#6 - balance: average yearly balance, in euros (numeric) 
#7 - housing: has housing loan? (binary: "yes","no")
#8 - loan: has personal loan? (binary: "yes","no")
# related with the last contact of the current campaign:
#9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
#10 - day: last contact day of the month (numeric)
#11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
#12 - duration: last contact duration, in seconds (numeric)
# other attributes:
#13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
#15 - previous: number of contacts performed before this campaign and for this client (numeric)
#16 - poutcome: outcome of the previous marketing campaign (categorical: "unknown","other","failure","success")

#Output variable (desired target):
#17 - y - has the client subscribed a term deposit? (binary: "yes","no")

#8. Missing Attribute Values: None


#Questoes

#1. Qual profissão tem mais tendência a fazer um empréstimo? De qual tipo?
#2. Fazendo uma relação entre número de contatos e sucesso da campanha quais
#são os pontos relevantes a serem observados?
#3. Baseando-se nos resultados de adesão desta campanha qual o número médio e
#o máximo de ligações que você indica para otimizar a adesão?
#4. O resultado da campanha anterior tem relevância na campanha atual?
#5. Qual o fator determinante para que o banco exija um seguro de crédito?
#6. Quais são as características mais proeminentes de um cliente que possua
#empréstimo imobiliário?


setwd("/Volumes/ExternalHD/VirtualBox/SharedFiles/Datasets/Semantix/bank")
getwd()

#install.packages("readr")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("plyr")
#library(readr)
#library(data.table)
#library(dplyr)
#library(plyr)

#carregando e conhecendo o Dataset

dforiginal <- fread("https://github.com/rodolfodomiciano/bank-analysis/raw/master/bank-full.csv")

summary(dforiginal)
head(dforiginal)
tail(dforiginal)
str(dforiginal)

# Questao 2 - Fazendo uma relação entre número de contatos e sucesso da campanha quais
#são os pontos relevantes a serem observados?

#Número de contatos - variável campaign
#Sucesso da Campanha - variável y


# Analisando relações entre sucesso/fracasso, ligações/contatos
#contatos são diferentes de ligações
#Ligações: Número total de ligações efetuadas, independente do resultado
#Contatos: Número de ligações realizadas para um cliente específico.

contatos <- data.frame(campaign = as.integer(dforiginal$campaign),  y = as.factor(dforiginal$y))
totalligacoes <- as.integer(sum(contatos$campaign))
totalcontatos <- data.frame(y = as.factor(names(summary(contatos$y))), contatos = as.integer(summary(contatos$y)))
totalcontatos$totcont <- sum(totalcontatos$contatos)
totalcontatos$perc_cont <- round(((totalcontatos$contatos/totalcontatos$totcont)*100), digits = 2)
totalcontatos$ligacoes <- ifelse (totalcontatos$y == "yes", totalcontatos$contatos, totalligacoes - (totalcontatos$contatos[2]))
totalcontatos$totlig <- totalligacoes
totalcontatos$perc_lig <- round(((totalcontatos$ligacoes/totalcontatos$totlig)*100), digits = 2)
totalcontatos



# 1. Taxa de sucesso em relação ao total de contatos realizados (importante para o cálculo do
# custo por cliente)

totalcontatos$perc_cont[2]

# A taxa de sucesso por total de ligações é de 11.7%

 
# 2. Taxa de sucesso em relação ao total de ligações efetuadas (importante para o cálculo do
# custo por ligação)

totalcontatos$perc_lig[2]

# A taxa de sucesso por total de ligações é de 4.23%

write.csv(totalcontatos, file = "questao2.csv")


#3. Baseando-se nos resultados de adesão desta campanha qual o número médio e
#o máximo de ligações que você indica para otimizar a adesão?

# Qual a taxa de sucesso para cada contato por número de ligações?

taxasucesso <- dforiginal[y == "yes", .N, by = campaign]
colnames(taxasucesso) <- c("Num_Ligacoes", "Sucesso")
ts.ordenado <- taxasucesso %>% 
  select(Num_Ligacoes, Sucesso) %>%
  arrange(Num_Ligacoes)
ts.ordenado

#Percentualmente:

ts.ordenado$TaxaSucesso <- round((ts.ordenado$Sucesso/sum(ts.ordenado$Sucesso)*100), digits = 2)
ts.ordenado$TaxaAcumulada <- round(cumsum(prop.table(ts.ordenado$TaxaSucesso)*100), digits = 2)
ts.ordenado

### 48.42 dos contratos foram fechados com apenas 1 ligação - efetividade máxima.
### 95.22 dos contratos foram fechado com 5 ligações.
### Até 3 ligações são responsáveis por 86,59%
### Até 5 ligações são responsáveis por 95,22%
### Até 10 ligações são responsáveis por 99,11%

write.csv(ts.ordenado, file = "questao3.csv")

#Questão 3. Baseando-se nos resultados de adesão desta campanha qual o número médio e
#o máximo de ligações que você indica para otimizar a adesão?

# Baseado nas análises da questão 2, sugiro um número médio de 3 ligações, que são responsáveis por 
# 86,59% do sucesso dos contratos, e um máximo de 5, responsável por 95,22%, pois observamos também
# uma correlação negativa entre o número de ligações e a taxa de sucesso.
# Para uma decisão ótima do número máximo de ligações, seria necessário comparar o custo por ligação 
# por contrato fechado. para verificar se vale a pena chegarmos a um máximo de 10 ligações. 
# É uma decisão de negócios onde não há dados suficientes para chegar no ponto ótimo (custo por ligação).

