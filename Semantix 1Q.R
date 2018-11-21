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

write.csv(dforiginal, file = "dforiginal.csv")


# Questão 1 - Profissão x Empréstimo. Vamos analisar os dois.


#1. Qual profissão tem mais tendência a fazer um empréstimo? De qual tipo?

# 1 - Profissão x Empréstimo Imobiliário

prof.imob <- dforiginal[housing == "yes", .N, by = job ]
prof.imob <- data.frame(job = as.factor(prof.imob$job), housing = as.integer(prof.imob$N))
prof.imob <- prof.imob %>% 
  select(job, housing) %>%
  arrange(desc(housing))
prof.imob
prof.imob.taxas <- prof.imob
prof.imob.taxas$TaxaSucesso <- round((prof.imob$housing/sum(prof.imob$housing)*100), digits = 2)
prof.imob.taxas$TaxaAcumulada <- round(cumsum(prop.table(prof.imob$housing)*100), digits = 2)
prof.imob.taxas


# 2 - Profissão x Empréstimo Pessoal

prof.loan <- dforiginal[loan == "yes", .N, by = job ]
prof.loan <- data.frame(job = as.factor(prof.loan$job), loan = as.integer(prof.loan$N))
prof.loan <- prof.loan %>% 
  select(job, loan) %>%
  arrange(desc(loan))
prof.loan
prof.loan.taxas <- prof.loan
prof.loan.taxas$TaxaSucesso <- round((prof.loan$loan/sum(prof.loan$loan)*100), digits = 2)
prof.loan.taxas$TaxaAcumulada <- round(cumsum(prop.table(prof.loan$loan)*100), digits = 2)
prof.loan.taxas


# 3 - Percentual total de jobs

jobs.perc <- count(dforiginal$job)
jobs.perc <- data.frame(job = as.factor(prof.loan$job), freq = jobs.perc['freq'])
jobs.perc <- jobs.perc %>% 
  select(job, freq) %>%
  arrange(desc(freq))
jobs.perc
jobs.perc.taxas <- jobs.perc
jobs.perc.taxas$Percentual <- round((jobs.perc$freq/sum(jobs.perc$freq)*100), digits = 2)
jobs.perc.taxas$PercAcumulado <- round(cumsum(prop.table(jobs.perc$freq)*100), digits = 2)
jobs.perc.taxas

#juntando as tabelas

dbemp <- join(prof.imob, prof.loan, by = "job", match = "all")
dbempfinal <- join(dbemp, jobs.perc, by = "job", match = "all")
dbempfinal

dbempfinalacumulado <- data.frame(job = dbempfinal$job, imob_acum = prof.imob.taxas$TaxaAcumulada, loan_acum = jobs.perc.taxas$PercAcumulado)


# Calculo da Frequencia por tipo de Emprestimo:

dbempfinal$freq_housing <- ((dbempfinal$housing/dbempfinal$freq)*100)
dbempfinal$freq_loan <- ((dbempfinal$loan/dbempfinal$freq)*100)
dbempfinal

#% em relação ao total

#housing

totalhousing <- sum(dbempfinal$housing)
totalhousing
totalloan <- sum(dbempfinal$loan)
totalloan

#% de empréstimos:

totalemprestimos <- totalloan/totalhousing
totalemprestimos

# O volume de housing foi de 25130 empréstimos, enquanto o de loan foi de 7244 para o mesmo conjunto de dados, um
# volume 3.47 vezes maior, confirmando a tendência de housing.

write.csv(dbempfinal, file = "dbempfinal.csv")

freq_housing_mean <- summary(dbempfinal$freq_housing)
freq_housing_mean <- data_frame(freq_housing_mean)
freq_housing_mean$Stats <- c("min", "1stQ", "Median", "Mean", "ThirdQ", "Max")
freq_loan_mean <- summary(dbempfinal$freq_loan)
freq_loan_mean <- data_frame(freq_loan_mean)
freq_loan_mean$Stats <- c("min", "1stQ", "Median", "Mean", "ThirdQ", "Max")
freq_mean <- join(freq_housing_mean, freq_loan_mean, by = "Stats", match= "all")
freq_mean

write.csv(freq_mean, file = "freq_mean-questao1.csv")

#Gerar Visualização e Relatórios no Tableau

# Comparativamente temos que a média de frequencia do Empréstimo imobiliário é 78.72%,
# com um mínimo de 7.14% e máximo 314.59%, enquanto a média de frequência do empréstimo 
# pessoal é de 23.31%, com mínimo de 0.92% e máximo de 84.26%.
# Portanto, a média de frequência imobiliario é maior que a média pessoal, indica a
# clara tendência pelo empréstimo imobiliário.
