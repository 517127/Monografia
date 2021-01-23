## pacotes necessarios ------------
library(lmtest)
library(urca)
library(dplyr)
library(forecast)
library(vars)
library(tsDyn)
library(ggplot2)
library (readr)
# importar os dados ---------------
my.url <- "https://raw.githubusercontent.com/517127/Monografia/master/dados/dados_finais.csv"
my.df <- read_csv(url(my.url))
my.df$my.dummy <- c(rep(0,196), rep(1,(209-196)))
my.model.volume <- my.df[,c(4, 1)]
my.model.volume$volume <- log(my.model.volume$volume) 
my.model.price <- my.df[,c(9, 1)]
my.model.price$price.adjusted <- log(my.model.price$price.adjusted)

# graficos
regplot <- ggplot(my.model.volume, 
                  aes(y = volume, x = PC1, color = "white")) + 
  geom_point(aes(color = "drv")) + geom_jitter() +
  theme_light() + 
  geom_smooth(method = "lm",  color = "red",)
regplot

regplot.p <- ggplot(my.model.price, 
                    aes(y = price.adjusted, x = PC1, color = "white")) + 
  geom_point(aes(color = "drv")) + geom_jitter() +
  theme_light() + 
  geom_smooth(method = "lm",  color = "red",)
regplot.p

# var select ----------
# primeiro passo da analise
VARselect(my.model.volume, type = "cons",
          exogen = my.df$my.dummy) #  4
VARselect(my.model.price, type = "trend", 
          exogen = my.df$my.dummy) # 2 5 ou 9
# cointegração---------
coint.volume <- ca.jo(my.model.volume, 
                      type = "eigen",
                      ecdet = "trend", 
                      K = 4,
                      season = 52,
                      dumvar = my.df$my.dummy
) # 2 vetores

summary(coint.volume)  # rejeita r<=0 tem cointegraçao

coint.price  <- ca.jo(my.model.price,
                      type = "eigen",
                      ecdet = "trend", 
                      K = 2,
                      #             season = 52,
                      dumvar = my.df$my.dummy
) # vetores
summary(coint.price) # rejeita h0
cajo.price <- cajools(coint.price)
Box.test(cajo.price$residuals[,1], 
         type = "Ljung-Box") # bom residuo 
cajo.fit <- cajo.price$fitted.values[,1]

# vecm ---------------------------
vecm.volume <- VECM(my.model.volume, lag = 5,
                    estim = "ML", 
                    include = "cons")
summary(rank.test(vecm.volume))
t(vecm.volume$model.specific$beta)

Box.test(vecm.volume$residuals[,1], 
         type = "Ljung-Box")

vecm.price <- VECM(my.model.price, lag = 2,
                   estim = "ML", 
                   include = "trend",
                   exogen = my.df$my.dummy)
summary(rank.test(vecm.price))
t(vecm.price$model.specific$beta)
Box.test(vecm.price$residuals[,1], 
         type = "Ljung-Box")

fitted.vecm <- vecm.price$fitted.values[,1]
#vec2var ------------
vecvar.volume <- vec2var(coint.volume, r = 1)
vecvar.price <- vec2var(coint.price, r = 1)

# residuo --------------
# hipotese nula é ausencia de autocorrelação
#serial.test(vecvar.volume, lags.pt = 15, type  = "ES") # REMOVER
serial.test(vecvar.volume, lags.pt = 15, type  = "PT.adjusted")
#serial.test(vecvar.price, lags.pt = 15, type = "ES")  # REMOVER
serial.test(vecvar.price, lags.pt = 15, type = "PT.adjusted")
normality.test(vecvar.volume)
normality.test(vecvar.price)
arch.test(vecvar.volume, lags.multi = 4)
arch.test(vecvar.price,lags.multi = 2)

# decomposição da variância ------------
fevd.vol <- fevd(vecvar.volume, n.ahead=100)
(fevd.vol$volume)
fevd.price <- fevd(vecvar.price, n.ahead=20)
(fevd.price$price.adjusted)

# irf ----

impulse_PC1 <- irf(vecvar.volume, 
                   impulse = "PC1", ortho = FALSE)
impulse_volume <- irf(vecvar.volume, 
                      impulse = "volume")

plot(impulse_PC1)
plot(impulse_volume)

impulse_price <- irf(vecvar.price, 
                     impulse = "price.adjusted",
                     ortho = TRUE)
impulse_PC1 <- irf(vecvar.price, 
                   impulse = "PC1")
plot(impulse_PC1)
plot(impulse_price)



#DADOS DE TREINO ----------------
fit.var.price <- fitted(vecvar.price)
price.prev <- fit.var.price[,1]
ret.real <- my.df$ret.adjusted.prices[4:209]
# DADOS DE TESTE--------------------------------
exo1 <- data.frame(rep(1,n_linhas))
names(exo1) <- "exo1"
pred.var <- predict(vecvar.price, 
                    dumvar =  exo1,
                    n.ahead = n_linhas)
prev.ibov_t <- pred.var$fcst$price.adjusted[,1]
my.url <- "https://raw.githubusercontent.com/517127/Monografia/master/dados/dados_teste.csv"
my.df <- read_csv(url(my.url))

real.ret_2 <- ibov$df.tickers$ret.adjusted.prices[2:38]
prev_pred <- c(price.prev,prev.ibov_t)
ret_ibov <- c(ret.real, real.ret_2)
#algoritmo comprado ----
l.prev_pred <- lag(prev_pred,1)
regra <- ifelse(prev_pred > l.prev_pred, 1, 0)
ret.var <- regra[2:length(regra)] *  ret_ibov
plot(cumsum(ret_ibov),
     type = "l",
     col = "red",
     #lwd = 3, lty = 3,
     main = "Buy and Hold x algoritmo",
     xlab = "weeks",
     ylab = "Buy and Hold x algoritmo"
)
lines(cumsum(ret.var))
abline(v = 205)
legend(
  "topleft",
  legend = c("Buy and Hold", "Algoritmo"),
  col = c("red", "black"),
  lty = 1.1,
  lwd = 1.1,
  bty = "n"
)
## comprado e vendido ------------------------
regra <- ifelse(prev_pred > l.prev_pred, 1, -1)
ret.var_cv <- regra[2:length(regra)] *  ret_ibov
plot(cumsum(ret.var_cv),
     type = "l",
     col = "red",
     #lwd = 3, lty = 3,
     main = "Buy and Hold x algoritmo",
     xlab = "weeks",
     ylab = "Buy and Hold x algoritmo"
)
lines(cumsum(ret_ibov))
lines(cumsum(ret.var), col = "blue")
abline(v = 205, lty = 3)
legend(  "topleft",
         legend = c("Buy and Hold", "Algotimo Duplo",
                    "Algoritmo Simples"),
         col = c("black", "red", "blue"),
         lty = 1,
         lwd = 2,
         cex = 0.8,
         bty = "n"
)
# comparação entre algoritmos
retornos <- c(sum(ret_ibov), sum(ret.var),sum(ret.var_cv))
desvios_p <- c(sd(ret_ibov), sd(ret.var), sd(ret.var_cv))
estrategia <- c("Buy and Hold","Algoritmo Simles",
                "Algoritmo duplo")
media <- c(mean(ret_ibov), mean(ret.var),mean(ret.var_cv))
data.frame(estrategia, retornos, desvios_p, media)



