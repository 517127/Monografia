#importar dados ibovespa
## dados treino -----------------
library(BatchGetSymbols)
firstdate <- "2016-04-04"
lastdate <- "2020-04-03"
frq <- "weekly"
ibov <- BatchGetSymbols(tickers = "^BVSP",
                        first.date = firstdate,
                        last.date = lastdate,
                        freq.data = frq,
                        do.cache = FALSE)
saveRDS(ibov$df.tickers, file = "IBOV.RDS")

## dados teste -----------------
firstdate <- "2020-03-30"
lastdate <- "2020-12-18"
frq <- "weekly"
ibov <- BatchGetSymbols(tickers = "^BVSP",
                        first.date = firstdate,
                        last.date = lastdate,
                        freq.data = frq,
                        do.cache = FALSE)
saveRDS(ibov$df.tickers, file = "dados_teste.RDS")
