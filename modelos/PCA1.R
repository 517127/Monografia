library(psych)
library(dplyr)
library (readr)
my.url <- "https://raw.githubusercontent.com/517127/Monografia/master/dados/dados_finais.csv"
mydata <- read_csv(url(my.url))
my.df<- my.df %>% select(-id)

## BatchGetSYmbols------------------
library(BatchGetSymbols)
firstdate <- "2016-04-04"
lastdate <- "2020-04-03"
frq <- "weekly"

ibov <- BatchGetSymbols(tickers = "^BVSP",
                        first.date = firstdate,
                        last.date = lastdate,
                        freq.data = frq,
                        do.cache = FALSE)

volume <- ibov$df.tickers$volume
precoaj <- ibov$df.tickers$price.adjusted
retorno <- ibov$df.tickers$ret.adjusted.prices


# componentes principais-----------------

setwd("/home/mateus/UFSM/8 SEMESTRE/MONOGRAFIA/dados")
my.df <- readRDS("dados_pca_wider.RDS")
library(dplyr)
library(psych)
my.df <- my.df[,2:80]
det(cor(my.df)) > 0 # condição necessária
KMO <- KMO(my.df) # ideal é o mais próximo de 1
KMO$MSA #ok
bart <- bartlett.test(my.df) #ideal é rejeitar h0
bart$statistic #ok 
bart$p.value #ok
cortest <- cortest.bartlett(my.df) #Ho: A matriz de correlação da população é uma matriz identidade, ou seja as variáveis não são correlacionadas na população.
cortest$chisq #ok 
cortest$p.value # ok

alpha.c <- alpha(my.df, check.keys = TRUE) #O Alfa de Cronbach é um teste de consistência interna dos dados, baseada na correlação entre as variáveis observadas.
alpha.c$alpha.drop # alpha mínimo é de 0,6
mean(alpha.c$alpha.drop[,1])  # media alta
corrplot::corrplot(corr = cor(my.df), 
                   xlab = "", ylab = "") #correlação
scree(my.df) # ver o número de fatores e componentes
fa.parallel(my.df) # ver uma sugestão de componentes e fatores


# como é necessário manter a interpretação econômica
# 1 componente é o ideal
my.fac <- pca(my.df, nfactors = 1, rotate = "varimax")
my.scores <- my.fac$scores
my.df <- data.frame(my.scores, ibov$df.tickers)
saveRDS(my.df, file =  "PCA_1.RDS")


