# script para importar os dados
# os dados são a última composição do ibovespa de 2020
# pacotes -----------------
library(BatchGetSymbols)
library(gtrendsR)
library(dplyr)
library(tidyr)

## definições -------------------
intvl <- "2016-04-03 2020-04-03" #intervalo gtrends
tickers <- GetIbovStocks() ## coletar todos os tickers do ibovespa
tickers <- unlist(tickers$tickers)
tickers <- c(tickers, "Ibovespa", "IBOV")


## a partir daqui é um pouco manual a coleta dos dados
## não consegui fazer um apply ou algo parecido então decidi
## coletar de cinco em cinco e juntar todos no final
#ticker1------
tickers1 <- tickers[1:5]
g.ticker <- gtrends(keyword = tickers1, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
my.df <- g.ticker
glimpse(my.df)
#ticker2------
tickers2 <- tickers[6:10]

g.ticker <- gtrends(keyword = tickers2, time = intvl)

g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)
#ticker3-----
tickers3 <- tickers[11:15]
g.ticker <- gtrends(keyword = tickers3, time = intvl)

g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)
#ticker4-------
tickers4 <- tickers[16:20]
g.ticker <- gtrends(keyword = tickers4, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)
#ticker5--------
tickers5 <- tickers[21:25]
g.ticker <- gtrends(keyword = tickers5, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)
#ticker6----------
tickers6 <- tickers[26:30]
g.ticker <- gtrends(keyword = tickers6, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)
#ticker7---------------
tickers7 <- tickers[31:35]
g.ticker <- gtrends(keyword = tickers7, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

#ticker8--------
tickers8 <- tickers[36:40]
g.ticker <- gtrends(keyword = tickers8, time = intvl)

g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

#ticker9---------
tickers9 <- tickers[41:45]
g.ticker <- gtrends(keyword = tickers9, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

#ticker10---------
tickers10 <- tickers[46:50]
g.ticker <- gtrends(keyword = tickers10, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

#ticker11-----------
tickers11 <- tickers[51:55]
g.ticker <- gtrends(keyword = tickers11, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

#ticker12-----------
tickers12 <- tickers[56:60]
g.ticker <- gtrends(keyword = tickers12, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

#ticker13---------
tickers13 <- tickers[61:65]
g.ticker <- gtrends(keyword = tickers13, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

#ticker14---------------
tickers14 <- tickers[66:70]
g.ticker <- gtrends(keyword = tickers14, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

#ticker15-----------
tickers15 <- tickers[71:75]
g.ticker <- gtrends(keyword = tickers15, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

##ticker 16------------
tickers16 <- tickers[76:79]
g.ticker <- gtrends(keyword = tickers16, time = intvl)
g.ticker <- data.frame(g.ticker$interest_over_time$hits,
                       g.ticker$interest_over_time$keyword) %>% tibble()
g.ticker$g.ticker.interest_over_time.hits <- as.character(g.ticker$g.ticker.interest_over_time.hits)
my.df <- bind_rows(my.df, g.ticker)
glimpse(my.df)

## transformaçoes e correções -----------
names(my.df) <- c("pesquisa","ticker")
my.df$pesquisa <- as.numeric(my.df$pesquisa)
my.df$pesquisa <- replace_na(my.df$pesquisa,0)
nrow(my.df)/209 
my.df$id <- rep(1:209,79)
my.df
saveRDS(my.df, file = "dados_pca_bruto.RDS")
#my.df <-readRDS("dados_pca.RDS")

## filtro para remover muitos zeros -----------------------
teste <- my.df %>% 
  group_by(ticker) %>% 
  summarise(pesquisa = sum(pesquisa))
filtradas <- filter(teste, pesquisa < 200)
filtradas <- dput(filtradas$ticker)
my.df <- pivot_wider(
  my.df,
  names_from = ticker,
  values_from = pesquisa
)
saveRDS(my.df, file = "dados_pca_wider.RDS")
#filtradas =  c("GNDI3", "HAPV3", "HYPE3", "IGTA3", "MULT3", "NTCO3", "PCAR3", 
#"PRIO3", "QUAL3", "TIMS3", "UGPA3", "VIVT3", "WEGE3", "YDUQ3"
#)


