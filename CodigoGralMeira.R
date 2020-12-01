#-------------------------------------------------------------------------#
#---------------------------- PAIRS TRADING ------------------------------#
#---------------------- GABRIEL GRAL E PAULO MEIRA -----------------------#
#------------------------ TRABALHO DE CONCLUSÃO --------------------------#
#------------------------------ PROJETOS 2 -------------------------------#
#-------------------------------------------------------------------------#


# Libraries ---------------------------------------------------------------
library(quantmod)
library(egcm)
library(urca)
library(plyr)
library(xts)
library(stargazer)
library(tseries)
library(xtable)
library(ggplot2)
library(reshape2)

# Criando base das ações --------------------------------------------------

X <- c("ABEV3.SA", "AZUL4.SA","B3SA3.SA","BBAS3.SA","BBDC4.SA","BBSE3.SA","BPAC11.SA","BRDT3.SA","BRFS3.SA","BRML3.SA","BTOW3.SA","CCRO3.SA","CIEL3.SA","CMIG4.SA","COGN3.SA","CSNA3.SA","CYRE3.SA","ELET3.SA","EQTL3.SA","GGBR4.SA","GOAU4.SA","GOLL4.SA","IRBR3.SA","ITSA4.SA","ITUB4.SA","JBSS3.SA","KLBN11.SA","LAME4.SA","LREN3.SA","MGLU3.SA","MRFG3.SA","MRVE3.SA","MULT3.SA","PETR3.SA","PETR4.SA","RADL3.SA","RAIL3.SA","RENT3.SA","SBSP3.SA","SULA11.SA","SUZB3.SA","UGPA3.SA","USIM5.SA","VALE3.SA","VVAR3.SA","WEGE3.SA","YDUQ3.SA")
getSymbols.yahoo(Symbols = X, env = globalenv(), from = "2018-01-01" )

Y <- merge(ABEV3.SA, AZUL4.SA, B3SA3.SA,BBAS3.SA,BBDC4.SA,BBSE3.SA,BPAC11.SA,BRDT3.SA,BRFS3.SA,BRML3.SA,BTOW3.SA,CCRO3.SA,CIEL3.SA,CMIG4.SA,COGN3.SA,CSNA3.SA,CYRE3.SA,ELET3.SA,EQTL3.SA,GGBR4.SA,GOAU4.SA,GOLL4.SA,IRBR3.SA,ITSA4.SA,ITUB4.SA,JBSS3.SA,KLBN11.SA,LAME4.SA,LREN3.SA,MGLU3.SA,MRFG3.SA,MRVE3.SA,MULT3.SA,PETR3.SA,PETR4.SA,RADL3.SA,RAIL3.SA,RENT3.SA,SBSP3.SA,SULA11.SA,SUZB3.SA,UGPA3.SA,USIM5.SA,VALE3.SA,VVAR3.SA,WEGE3.SA,YDUQ3.SA)

colu <- c()
for (i in 1:length(X)){
  colu[i] = 6*i
}
Y <- Y[,colu]
Y <- Y[complete.cases(Y),]

Zeta <- Y[c(1:492),] #Definindo a amostra até 2019

#Escrevendo base das ações
#write.csv(Y, file = "C:/Users/55419/Desktop/Omicron_Trading/BaseGralMeira.csv")

# Testes Engle Granger ----------------------------------------------------

Kappa <- data.frame()
n <- 47
for (i in 1:n){
  for (j in (i+1):n){
    teste1 <- egcm(Zeta[,i], Zeta[,j], urtest = "adf")
    teste2 <- egcm(Zeta[,i], Zeta[,j], urtest = "jo-e")
    teste3 <- egcm(Zeta[,i], Zeta[,j], urtest = "jo-t")
    teste4 <- egcm(Zeta[,i], Zeta[,j], urtest = "pp")
    info <- c(colnames(Zeta)[i], colnames(Zeta)[j], teste1$r.stat, teste1$r.p, teste2$r.stat, teste2$r.p, teste3$r.stat, teste3$r.p, teste4$r.stat, teste4$r.p)
    Kappa <- rbind(Kappa, info)
  }
}
Kappa_names <- c("Stock 1", "Stock 2", "ADF Statistic", "ADF p-value", "Johansen Eigenvalue Statistic", "Johansen Eigenvalue p-value", "Johansen Trace Statistic", "Johansen Trace p-value", "Phillips-Perron Statistic", "Phillips-Perron p-value")
colnames(Kappa) <- Kappa_names
head(Kappa)

# Filtrando ---------------------------------------------------------------

shared_alpha <- function(alpha){
  x <- dplyr::filter(Kappa, Kappa$`ADF p-value`<alpha)
  y <- dplyr::filter(Kappa, Kappa$`Johansen Eigenvalue p-value`<alpha)
  z <- dplyr::filter(Kappa, Kappa$`Johansen Trace p-value`<alpha)
  w <- dplyr::filter(Kappa, Kappa$`Phillips-Perron p-value`<alpha)
  return(dplyr::intersect(x,y,z,w))
}

#A função escolhe apenas os pares aprovados a um determinado nível de significância
Valid_Pairs <- shared_alpha(0.01)

View(Valid_Pairs) #Valid_Pairs é a tabela que contém nossos pares selecionados e os respectivos testes de hipótese


# Escolhendo estratégia ---------------------------------------------------

#Temos uma tabela com 4 pares, para cada um deles, temos que escolher a regra de decisão ótima.

ParesCointegrados <- cbind.xts(Zeta$BBSE3.SA.Adjusted, Zeta$CCRO3.SA.Adjusted, Zeta$BBSE3.SA.Adjusted, Zeta$CYRE3.SA.Adjusted, Zeta$BRDT3.SA.Adjusted, Zeta$EQTL3.SA.Adjusted, Zeta$BRDT3.SA.Adjusted, Zeta$LREN3.SA.Adjusted)
sigmas <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5)

#Para cada par, veremos qual elemento de sigma fornece o melhor desempenho
#Faço um for loop para cada par de ações, o código não é o mais elegante, mas facilitou bastante a manipulação dos dados e a plotagem dos gráficos
#Para cada par de ações, crio um vetor de retornos acumulados (Returns), e crio uma tabela com as informações de retorno, risco e quantidade de trades para cada elemento de sigma

# BBSE CCRO ---------------------------------------------------------------
Returns_BBSE_CCRO <- matrix()
Main_Info_BBSE_CCRO <- matrix(NA, nrow = 1, ncol = 4)
x <- ParesCointegrados$BBSE3.SA.Adjusted
y <- ParesCointegrados$CCRO3.SA.Adjusted
for (j in 1:length(sigmas)){
  rule <- sigmas[j]
  x$int <- rep(1, nrow(x))
  delta <- 0.0001 #Parâmetro do filtro de Kalman
  Vw <- delta/(1-delta)*diag(2) 
  Ve <- 0.001 #Parâmetro do filtro de Kalman
  R <- matrix(rep(0, 4), nrow=2)
  P <- matrix(rep(0, 4), nrow=2)
  beta <- matrix(rep(0, nrow(y)*2), ncol=2)
  y_est <- rep(0, nrow(y))
  e <- rep(0, nrow(y))
  Q <- rep(0, nrow(y))
  for(i in 1:nrow(y)) {
    if(i > 1) {
      beta[i, ] <- beta[i-1, ] 
      R <- P + Vw 
    }
    y_est[i] <- x[i, ] %*% beta[i, ] 
    Q[i] <- x[i, ] %*% R %*% t(x[i, ]) + Ve 
    
    e[i] <- y[i] - y_est[i]
    K <- R %*% t(x[i, ]) / Q[i] 
    
    beta[i, ] <- beta[i, ] + K * e[i]
    P = R - K %*% x[i, ] %*% R
  }
  beta <- xts(beta, order.by=index(Zeta))
  e <- xts(e, order.by=index(Zeta))
  sqrtQ <- xts(sqrt(Q), order.by=index(Zeta))
  signals <- merge(e, rule*sqrtQ, -rule*sqrtQ)
  colnames(signals) <- c("e", "sqrtQ", "negsqrtQ")
  sig <- ifelse((signals[1:length(index(signals))]$e > signals[1:length(index(signals))]$sqrtQ) & (lag.xts(signals$e, 1) < lag.xts(signals$sqrtQ, 1)), -1, 
                ifelse((signals[1:length(index(signals))]$e < signals[1:length(index(signals))]$negsqrtQ) & (lag.xts(signals$e, 1) > lag.xts(signals$negsqrtQ, 1)), 1, 0))
  colnames(sig) <- "sig"
  sig[sig == 0] <- NA
  sig <- na.locf(sig)
  sig <- diff(sig)/2
  num_trades <-  sum(abs(as.vector(na.omit(sig))))
  sim <- merge(lag.xts(sig,1), beta[, 1], x[, 1], y)
  colnames(sim) <- c("sig", "hedge", "StockA", "StockB")
  sim$posX <- sim$sig * -1000 * sim$hedge
  sim$posY <- sim$sig * 1000   
  sim$posX[sim$posX == 0] <- NA
  sim$posX <- na.locf(sim$posX)
  sim$posY[sim$posY == 0] <- NA
  sim$posY <- na.locf(sim$posY)
  pnlX <- sim$posX * diff(sim[, 3])
  pnlY <- sim$posY * diff(sim[, 4])
  pnl <- pnlX + pnlY
  cum_returns <- cumsum(na.omit(pnl))
  daily <- (tail(cumsum(na.omit(pnl)), n=1)/1000)^(1/length(cumsum(na.omit(pnl)))) - 1
  anual <- (1+daily)^252 - 1
  sharpe <- (tail(cumsum(na.omit(pnl))))/sd(na.omit(pnl))
  
  fit_res <- lm(log(cum_returns) ~ c(1:length(cum_returns)))
  Returns_BBSE_CCRO <- cbind(Returns_BBSE_CCRO, cum_returns)
  information <- c(as.numeric(num_trades), as.numeric(anual), sd(fit_res$residuals), as.numeric(sigmas[j]))
  Main_Info_BBSE_CCRO <- rbind(Main_Info_BBSE_CCRO, information)
}



# BBSE CYRE ---------------------------------------------------------------
Returns_BBSE_CYRE <- matrix()
Main_Info_BBSE_CYRE <- matrix(NA, nrow = 1, ncol = 4)
x <- ParesCointegrados$BBSE3.SA.Adjusted
y <- ParesCointegrados$CYRE3.SA.Adjusted
for (j in 1:length(sigmas)){
  rule <- sigmas[j]
  x$int <- rep(1, nrow(x))
  delta <- 0.0001
  Vw <- delta/(1-delta)*diag(2)
  Ve <- 0.001
  R <- matrix(rep(0, 4), nrow=2)
  P <- matrix(rep(0, 4), nrow=2)
  beta <- matrix(rep(0, nrow(y)*2), ncol=2)
  y_est <- rep(0, nrow(y))
  e <- rep(0, nrow(y))
  Q <- rep(0, nrow(y))
  for(i in 1:nrow(y)) {
    if(i > 1) {
      beta[i, ] <- beta[i-1, ] 
      R <- P + Vw 
    }
    y_est[i] <- x[i, ] %*% beta[i, ]
    Q[i] <- x[i, ] %*% R %*% t(x[i, ]) + Ve 
    
    
    e[i] <- y[i] - y_est[i]
    K <- R %*% t(x[i, ]) / Q[i] 
    
    beta[i, ] <- beta[i, ] + K * e[i]
    P = R - K %*% x[i, ] %*% R
  }
  beta <- xts(beta, order.by=index(Zeta))
  e <- xts(e, order.by=index(Zeta))
  sqrtQ <- xts(sqrt(Q), order.by=index(Zeta))
  signals <- merge(e, rule*sqrtQ, -rule*sqrtQ)
  colnames(signals) <- c("e", "sqrtQ", "negsqrtQ")
  sig <- ifelse((signals[1:length(index(signals))]$e > signals[1:length(index(signals))]$sqrtQ) & (lag.xts(signals$e, 1) < lag.xts(signals$sqrtQ, 1)), -1, 
                ifelse((signals[1:length(index(signals))]$e < signals[1:length(index(signals))]$negsqrtQ) & (lag.xts(signals$e, 1) > lag.xts(signals$negsqrtQ, 1)), 1, 0))
  colnames(sig) <- "sig"
  sig[sig == 0] <- NA
  sig <- na.locf(sig)
  sig <- diff(sig)/2
  num_trades <-  sum(abs(as.vector(na.omit(sig))))
  sim <- merge(lag.xts(sig,1), beta[, 1], x[, 1], y)
  colnames(sim) <- c("sig", "hedge", "StockA", "StockB")
  sim$posX <- sim$sig * -1000 * sim$hedge
  sim$posY <- sim$sig * 1000   
  sim$posX[sim$posX == 0] <- NA
  sim$posX <- na.locf(sim$posX)
  sim$posY[sim$posY == 0] <- NA
  sim$posY <- na.locf(sim$posY)
  pnlX <- sim$posX * diff(sim[, 3])
  pnlY <- sim$posY * diff(sim[, 4])
  pnl <- pnlX + pnlY
  cum_returns <- cumsum(na.omit(pnl))
  daily <- (tail(cumsum(na.omit(pnl)), n=1)/1000)^(1/length(cumsum(na.omit(pnl)))) - 1
  anual <- (1+daily)^252 - 1
  sharpe <- (tail(cumsum(na.omit(pnl))))/sd(na.omit(pnl))
  
  fit_res <- lm(log(cum_returns) ~ c(1:length(cum_returns)))
  Returns_BBSE_CYRE <- cbind(Returns_BBSE_CYRE, cum_returns)
  information <- c(as.numeric(num_trades), as.numeric(anual), sd(fit_res$residuals), as.numeric(sigmas[j]))
  Main_Info_BBSE_CYRE <- rbind(Main_Info_BBSE_CYRE, information)
}


# BRDT EQTL ---------------------------------------------------------------
Returns_BRDT_EQTL <- matrix()
Main_Info_BRDT_EQTL <- matrix(NA, nrow = 1, ncol = 4)
x <- ParesCointegrados$BRDT3.SA.Adjusted
y <- ParesCointegrados$EQTL3.SA.Adjusted
for (j in 1:length(sigmas)){
  rule <- sigmas[j]
  x$int <- rep(1, nrow(x))
  delta <- 0.0001
  Vw <- delta/(1-delta)*diag(2)
  Ve <- 0.001
  R <- matrix(rep(0, 4), nrow=2)
  P <- matrix(rep(0, 4), nrow=2)
  beta <- matrix(rep(0, nrow(y)*2), ncol=2)
  y_est <- rep(0, nrow(y))
  e <- rep(0, nrow(y))
  Q <- rep(0, nrow(y))
  for(i in 1:nrow(y)) {
    if(i > 1) {
      beta[i, ] <- beta[i-1, ]
      R <- P + Vw 
    }
    y_est[i] <- x[i, ] %*% beta[i, ] 
    Q[i] <- x[i, ] %*% R %*% t(x[i, ]) + Ve 
    
    
    e[i] <- y[i] - y_est[i]
    K <- R %*% t(x[i, ]) / Q[i] 
    
    beta[i, ] <- beta[i, ] + K * e[i]
    P = R - K %*% x[i, ] %*% R
  }
  beta <- xts(beta, order.by=index(Zeta))
  e <- xts(e, order.by=index(Zeta))
  sqrtQ <- xts(sqrt(Q), order.by=index(Zeta))
  signals <- merge(e, rule*sqrtQ, -rule*sqrtQ)
  colnames(signals) <- c("e", "sqrtQ", "negsqrtQ")
  sig <- ifelse((signals[1:length(index(signals))]$e > signals[1:length(index(signals))]$sqrtQ) & (lag.xts(signals$e, 1) < lag.xts(signals$sqrtQ, 1)), -1, 
                ifelse((signals[1:length(index(signals))]$e < signals[1:length(index(signals))]$negsqrtQ) & (lag.xts(signals$e, 1) > lag.xts(signals$negsqrtQ, 1)), 1, 0))
  colnames(sig) <- "sig"
  sig[sig == 0] <- NA
  sig <- na.locf(sig)
  sig <- diff(sig)/2
  num_trades <-  sum(abs(as.vector(na.omit(sig))))
  sim <- merge(lag.xts(sig,1), beta[, 1], x[, 1], y)
  colnames(sim) <- c("sig", "hedge", "StockA", "StockB")
  sim$posX <- sim$sig * -1000 * sim$hedge
  sim$posY <- sim$sig * 1000   
  sim$posX[sim$posX == 0] <- NA
  sim$posX <- na.locf(sim$posX)
  sim$posY[sim$posY == 0] <- NA
  sim$posY <- na.locf(sim$posY)
  pnlX <- sim$posX * diff(sim[, 3])
  pnlY <- sim$posY * diff(sim[, 4])
  pnl <- pnlX + pnlY
  cum_returns <- cumsum(na.omit(pnl))
  daily <- (tail(cumsum(na.omit(pnl)), n=1)/1000)^(1/length(cumsum(na.omit(pnl)))) - 1
  anual <- (1+daily)^252 - 1
  sharpe <- (tail(cumsum(na.omit(pnl))))/sd(na.omit(pnl))
  
  fit_res <- lm(log(cum_returns) ~ c(1:length(cum_returns)))
  Returns_BRDT_EQTL <- cbind(Returns_BRDT_EQTL, cum_returns)
  information <- c(as.numeric(num_trades), as.numeric(anual), sd(fit_res$residuals), as.numeric(sigmas[j]))
  Main_Info_BRDT_EQTL <- rbind(Main_Info_BRDT_EQTL, information)
}



# BDRE LREN ---------------------------------------------------------------

Returns_BRDT_LREN <- matrix()
Main_Info_BRDT_LREN <- matrix(NA, nrow = 1, ncol = 4)
x <- ParesCointegrados$BRDT3.SA.Adjusted
y <- ParesCointegrados$LREN3.SA.Adjusted
rule <- 2.3
for (j in 1:length(sigmas)){
  rule <- sigmas[j]
  x$int <- rep(1, nrow(x))
  delta <- 0.0001
  Vw <- delta/(1-delta)*diag(2)
  Ve <- 0.001
  R <- matrix(rep(0, 4), nrow=2)
  P <- matrix(rep(0, 4), nrow=2)
  beta <- matrix(rep(0, nrow(y)*2), ncol=2)
  y_est <- rep(0, nrow(y))
  e <- rep(0, nrow(y))
  Q <- rep(0, nrow(y))
  for(i in 1:nrow(y)) {
    if(i > 1) {
      beta[i, ] <- beta[i-1, ] 
      R <- P + Vw 
    }
    y_est[i] <- x[i, ] %*% beta[i, ] 
    Q[i] <- x[i, ] %*% R %*% t(x[i, ]) + Ve 
    
    
    e[i] <- y[i] - y_est[i]
    K <- R %*% t(x[i, ]) / Q[i] 
    
    
    beta[i, ] <- beta[i, ] + K * e[i]
    P = R - K %*% x[i, ] %*% R
  }
  beta <- xts(beta, order.by=index(Zeta))
  e <- xts(e, order.by=index(Zeta))
  sqrtQ <- xts(sqrt(Q), order.by=index(Zeta))
  signals <- merge(e, rule*sqrtQ, -rule*sqrtQ)
  colnames(signals) <- c("e", "sqrtQ", "negsqrtQ")
  sig <- ifelse((signals[1:length(index(signals))]$e > signals[1:length(index(signals))]$sqrtQ) & (lag.xts(signals$e, 1) < lag.xts(signals$sqrtQ, 1)), -1, 
                ifelse((signals[1:length(index(signals))]$e < signals[1:length(index(signals))]$negsqrtQ) & (lag.xts(signals$e, 1) > lag.xts(signals$negsqrtQ, 1)), 1, 0))
  colnames(sig) <- "sig"
  sig[sig == 0] <- NA
  sig <- na.locf(sig)
  sig <- diff(sig)/2
  num_trades <-  sum(abs(as.vector(na.omit(sig))))
  sim <- merge(lag.xts(sig,1), beta[, 1], x[, 1], y)
  colnames(sim) <- c("sig", "hedge", "StockA", "StockB")
  sim$posX <- sim$sig * -1000 * sim$hedge
  sim$posY <- sim$sig * 1000   
  sim$posX[sim$posX == 0] <- NA
  sim$posX <- na.locf(sim$posX)
  sim$posY[sim$posY == 0] <- NA
  sim$posY <- na.locf(sim$posY)
  pnlX <- sim$posX * diff(sim[, 3])
  pnlY <- sim$posY * diff(sim[, 4])
  pnl <- pnlX + pnlY
  cum_returns <- cumsum(na.omit(pnl))
  daily <- (tail(cumsum(na.omit(pnl)), n=1)/1000)^(1/length(cumsum(na.omit(pnl)))) - 1
  anual <- (1+daily)^252 - 1
  sharpe <- (tail(cumsum(na.omit(pnl))))/sd(na.omit(pnl))
  
  fit_res <- lm(log(cum_returns) ~ c(1:length(cum_returns)))
  Returns_BRDT_LREN <- cbind(Returns_BRDT_LREN, cum_returns)
  information <- c(as.numeric(num_trades), as.numeric(anual), sd(fit_res$residuals), as.numeric(sigmas[j]))
  Main_Info_BRDT_LREN <- rbind(Main_Info_BRDT_LREN, information)
}


# Escolher carteira para 2020 ---------------------------------------------
#Para cada par de ações, o valor a (a1, a2, a3, a4) é o vetor de resultados para o elemento de sigma ideal
#Para cada par de ações, o valor de b (b1, b2, b3, b4) é o índice de retorno-risco

print("Melhor desempenho de BBSE CCRO")
a1 <- Main_Info_BBSE_CCRO[which.max(Main_Info_BBSE_CCRO[,2]),]
b1 <- a1[2]/a1[3]
#Sigma = 1.4

print("Melhor desempenho de BBSE CYRE")
Main_Info_BBSE_CYRE[which.max(Main_Info_BBSE_CYRE[,2]),]
a2 <- Main_Info_BBSE_CYRE[which.max(Main_Info_BBSE_CYRE[,2]),]
b2 <- a2[2]/a2[3]
#Sigma = 2.3

print("Melhor desempenho de BRDT EQTL")
Main_Info_BRDT_EQTL[which.max(Main_Info_BRDT_EQTL[,2]),]
a3 <- Main_Info_BRDT_EQTL[which.max(Main_Info_BRDT_EQTL[,2]),]
b3 <- a3[2]/a3[3]
#Sigma = 1

print("Melhor desempenho de BRDT LREN")
Main_Info_BRDT_LREN[which.max(Main_Info_BRDT_LREN[,2]),]
a4 <-Main_Info_BRDT_LREN[which.max(Main_Info_BRDT_LREN[,2]),]
b4 <- a4[2]/a4[3]
#Sigma = 2.3


#Definindo proporção de cada ação na carteira com base em Retorno-Risco
c1 <- b1/sum(b1, b2, b3, b4)
c2 <- b2/sum(b1, b2, b3, b4)
c3 <- b3/sum(b1, b2, b3, b4)
c4 <- b4/sum(b1, b2, b3, b4)

#Unidades de spread investidas 
d1 <- c1*1000
d2 <- c2*1000
d3 <- c3*1000
d4 <- c4*1000

# Simulando ---------------------------------------------------------------

#Uma vez definida nossa estratégia, fazemos as simulações para o período de 2020. 

Omicron <- Y[c(493:718),]

#O parâmetro rule é o que determina a regra de decisão, e corresponde ao elemento ótimo de sigma para cada par.

## BBSE CCRO

x <- Omicron$BBSE3.SA.Adjusted
y <- Omicron$CCRO3.SA.Adjusted

rule <- 1.4      

x$int <- rep(1, nrow(x))
delta <- 0.0001
Vw <- delta/(1-delta)*diag(2)
Ve <- 0.001
R <- matrix(rep(0, 4), nrow=2)
P <- matrix(rep(0, 4), nrow=2)
beta <- matrix(rep(0, nrow(y)*2), ncol=2)
y_est <- rep(0, nrow(y))
e <- rep(0, nrow(y))
Q <- rep(0, nrow(y))
for(i in 1:nrow(y)) {
  if(i > 1) {
    beta[i, ] <- beta[i-1, ]
    R <- P + Vw 
  }
  y_est[i] <- x[i, ] %*% beta[i, ]
  Q[i] <- x[i, ] %*% R %*% t(x[i, ]) + Ve
  
  
  e[i] <- y[i] - y_est[i]
  K <- R %*% t(x[i, ]) / Q[i] 
  
  
  beta[i, ] <- beta[i, ] + K * e[i]
  P = R - K %*% x[i, ] %*% R
}
beta <- xts(beta, order.by=index(Omicron))
e <- xts(e, order.by=index(Omicron))
sqrtQ <- xts(sqrt(Q), order.by=index(Omicron))
signals <- merge(e, rule*sqrtQ, -rule*sqrtQ)
colnames(signals) <- c("e", "sqrtQ", "negsqrtQ")
sig <- ifelse((signals[1:length(index(signals))]$e > signals[1:length(index(signals))]$sqrtQ) & (lag.xts(signals$e, 1) < lag.xts(signals$sqrtQ, 1)), -1, 
              ifelse((signals[1:length(index(signals))]$e < signals[1:length(index(signals))]$negsqrtQ) & (lag.xts(signals$e, 1) > lag.xts(signals$negsqrtQ, 1)), 1, 0))
colnames(sig) <- "sig"
sig[sig == 0] <- NA
sig <- na.locf(sig)
sig <- diff(sig)/2
num_trades <-  sum(abs(as.vector(na.omit(sig))))
sim <- merge(lag.xts(sig,1), beta[, 1], x[, 1], y)
colnames(sim) <- c("sig", "hedge", "StockA", "StockB")
sim$posX <- sim$sig * -d1 * sim$hedge
sim$posY <- sim$sig * d1   
sim$posX[sim$posX == 0] <- NA
sim$posX <- na.locf(sim$posX)
sim$posY[sim$posY == 0] <- NA
sim$posY <- na.locf(sim$posY)
pnlX <- sim$posX * diff(sim[, 3])
pnlY <- sim$posY * diff(sim[, 4])
pnl <- pnlX + pnlY

daily <- (tail(cumsum(na.omit(pnl)), n=1)/1000)^(1/length(cumsum(na.omit(pnl)))) - 1
anual <- (1+daily)^252 - 1
sharpe <- (tail(cumsum(na.omit(pnl))))/sd(na.omit(pnl))

Covid_Returns_BBSE_CCRO <- cumsum(na.omit(pnl))
Covid_Info_BBSE_CCRO<- c(as.numeric(num_trades), as.numeric(anual), sd(na.omit(pnl)), as.numeric(sigmas[j]))
sum(abs(na.omit(as.vector(sig$sig))))

## BBSE CYRE

x <- Omicron$BBSE3.SA.Adjusted
y <- Omicron$CYRE3.SA.Adjusted
rule <- 2.3

x$int <- rep(1, nrow(x))
delta <- 0.0001
Vw <- delta/(1-delta)*diag(2)
Ve <- 0.001
R <- matrix(rep(0, 4), nrow=2)
P <- matrix(rep(0, 4), nrow=2)
beta <- matrix(rep(0, nrow(y)*2), ncol=2)
y_est <- rep(0, nrow(y))
e <- rep(0, nrow(y))
Q <- rep(0, nrow(y))
for(i in 1:nrow(y)) {
  if(i > 1) {
    beta[i, ] <- beta[i-1, ] 
    R <- P + Vw 
  }
  y_est[i] <- x[i, ] %*% beta[i, ]
  Q[i] <- x[i, ] %*% R %*% t(x[i, ]) + Ve 
  
  e[i] <- y[i] - y_est[i]
  K <- R %*% t(x[i, ]) / Q[i] 
  
  beta[i, ] <- beta[i, ] + K * e[i]
  P = R - K %*% x[i, ] %*% R
}
beta <- xts(beta, order.by=index(Omicron))
e <- xts(e, order.by=index(Omicron))
sqrtQ <- xts(sqrt(Q), order.by=index(Omicron))
signals <- merge(e, rule*sqrtQ, -rule*sqrtQ)
colnames(signals) <- c("e", "sqrtQ", "negsqrtQ")
sig <- ifelse((signals[1:length(index(signals))]$e > signals[1:length(index(signals))]$sqrtQ) & (lag.xts(signals$e, 1) < lag.xts(signals$sqrtQ, 1)), -1, 
              ifelse((signals[1:length(index(signals))]$e < signals[1:length(index(signals))]$negsqrtQ) & (lag.xts(signals$e, 1) > lag.xts(signals$negsqrtQ, 1)), 1, 0))
colnames(sig) <- "sig"
sig[sig == 0] <- NA
sig <- na.locf(sig)
sig <- diff(sig)/2
num_trades <-  sum(abs(as.vector(na.omit(sig))))
sim <- merge(lag.xts(sig,1), beta[, 1], x[, 1], y)
colnames(sim) <- c("sig", "hedge", "StockA", "StockB")
sim$posX <- sim$sig * -d2 * sim$hedge
sim$posY <- sim$sig * d2   
sim$posX[sim$posX == 0] <- NA
sim$posX <- na.locf(sim$posX)
sim$posY[sim$posY == 0] <- NA
sim$posY <- na.locf(sim$posY)
pnlX <- sim$posX * diff(sim[, 3])
pnlY <- sim$posY * diff(sim[, 4])
pnl <- pnlX + pnlY

daily <- (tail(cumsum(na.omit(pnl)), n=1)/1000)^(1/length(cumsum(na.omit(pnl)))) - 1
anual <- (1+daily)^252 - 1
sharpe <- (tail(cumsum(na.omit(pnl))))/sd(na.omit(pnl))

Covid_Returns_BBSE_CYRE <- cumsum(na.omit(pnl))
Covid_Info_BBSE_CYRE <- c(as.numeric(num_trades), as.numeric(anual), sd(na.omit(pnl)), as.numeric(sigmas[j]))
sum(abs(na.omit(as.vector(sig$sig))))

## BRDT EQTL

x <- Omicron$BRDT3.SA.Adjusted
y <- Omicron$EQTL3.SA.Adjusted
rule <- 1

x$int <- rep(1, nrow(x))
delta <- 0.0001
Vw <- delta/(1-delta)*diag(2)
Ve <- 0.001
R <- matrix(rep(0, 4), nrow=2)
P <- matrix(rep(0, 4), nrow=2)
beta <- matrix(rep(0, nrow(y)*2), ncol=2)
y_est <- rep(0, nrow(y))
e <- rep(0, nrow(y))
Q <- rep(0, nrow(y))
for(i in 1:nrow(y)) {
  if(i > 1) {
    beta[i, ] <- beta[i-1, ] 
    R <- P + Vw 
  }
  y_est[i] <- x[i, ] %*% beta[i, ] 
  Q[i] <- x[i, ] %*% R %*% t(x[i, ]) + Ve 
  
  e[i] <- y[i] - y_est[i]
  K <- R %*% t(x[i, ]) / Q[i] 
  
  
  beta[i, ] <- beta[i, ] + K * e[i]
  P = R - K %*% x[i, ] %*% R
}
beta <- xts(beta, order.by=index(Omicron))
e <- xts(e, order.by=index(Omicron))
sqrtQ <- xts(sqrt(Q), order.by=index(Omicron))
signals <- merge(e, rule*sqrtQ, -rule*sqrtQ)
colnames(signals) <- c("e", "sqrtQ", "negsqrtQ")
sig <- ifelse((signals[1:length(index(signals))]$e > signals[1:length(index(signals))]$sqrtQ) & (lag.xts(signals$e, 1) < lag.xts(signals$sqrtQ, 1)), -1, 
              ifelse((signals[1:length(index(signals))]$e < signals[1:length(index(signals))]$negsqrtQ) & (lag.xts(signals$e, 1) > lag.xts(signals$negsqrtQ, 1)), 1, 0))
colnames(sig) <- "sig"
sig[sig == 0] <- NA
sig <- na.locf(sig)
sig <- diff(sig)/2
num_trades <-  sum(abs(as.vector(na.omit(sig))))
sim <- merge(lag.xts(sig,1), beta[, 1], x[, 1], y)
colnames(sim) <- c("sig", "hedge", "StockA", "StockB")
sim$posX <- sim$sig * -d3 * sim$hedge
sim$posY <- sim$sig * d3   
sim$posX[sim$posX == 0] <- NA
sim$posX <- na.locf(sim$posX)
sim$posY[sim$posY == 0] <- NA
sim$posY <- na.locf(sim$posY)
pnlX <- sim$posX * diff(sim[, 3])
pnlY <- sim$posY * diff(sim[, 4])
pnl <- pnlX + pnlY

daily <- (tail(cumsum(na.omit(pnl)), n=1)/1000)^(1/length(cumsum(na.omit(pnl)))) - 1
anual <- (1+daily)^252 - 1
sharpe <- (tail(cumsum(na.omit(pnl))))/sd(na.omit(pnl))

Covid_Returns_BRDT_EQTL <- cumsum(na.omit(pnl))
Covid_Info_BRDT_EQTL <- c(as.numeric(num_trades), as.numeric(anual), sd(na.omit(pnl)), as.numeric(sigmas[j]))
sum(abs(na.omit(as.vector(sig$sig))))

## BRDT LREN 2.3

x <- Omicron$BRDT3.SA.Adjusted
y <- Omicron$LREN3.SA.Adjusted
rule <- 2.3

x$int <- rep(1, nrow(x))
delta <- 0.0001
Vw <- delta/(1-delta)*diag(2)
Ve <- 0.001
R <- matrix(rep(0, 4), nrow=2)
P <- matrix(rep(0, 4), nrow=2)
beta <- matrix(rep(0, nrow(y)*2), ncol=2)
y_est <- rep(0, nrow(y))
e <- rep(0, nrow(y))
Q <- rep(0, nrow(y))
for(i in 1:nrow(y)) {
  if(i > 1) {
    beta[i, ] <- beta[i-1, ] 
    R <- P + Vw 
  }
  y_est[i] <- x[i, ] %*% beta[i, ] 
  Q[i] <- x[i, ] %*% R %*% t(x[i, ]) + Ve 
  
  
  e[i] <- y[i] - y_est[i]
  K <- R %*% t(x[i, ]) / Q[i] 
  
  beta[i, ] <- beta[i, ] + K * e[i]
  P = R - K %*% x[i, ] %*% R
}
beta <- xts(beta, order.by=index(Omicron))
e <- xts(e, order.by=index(Omicron))
sqrtQ <- xts(sqrt(Q), order.by=index(Omicron))
signals <- merge(e, rule*sqrtQ, -rule*sqrtQ)
colnames(signals) <- c("e", "sqrtQ", "negsqrtQ")
sig <- ifelse((signals[1:length(index(signals))]$e > signals[1:length(index(signals))]$sqrtQ) & (lag.xts(signals$e, 1) < lag.xts(signals$sqrtQ, 1)), -1, 
              ifelse((signals[1:length(index(signals))]$e < signals[1:length(index(signals))]$negsqrtQ) & (lag.xts(signals$e, 1) > lag.xts(signals$negsqrtQ, 1)), 1, 0))
colnames(sig) <- "sig"
sig[sig == 0] <- NA
sig <- na.locf(sig)
sig <- diff(sig)/2
num_trades <-  sum(abs(as.vector(na.omit(sig))))
sim <- merge(lag.xts(sig,1), beta[, 1], x[, 1], y)
colnames(sim) <- c("sig", "hedge", "StockA", "StockB")
sim$posX <- sim$sig * -d4 * sim$hedge
sim$posY <- sim$sig * d4   
sim$posX[sim$posX == 0] <- NA
sim$posX <- na.locf(sim$posX)
sim$posY[sim$posY == 0] <- NA
sim$posY <- na.locf(sim$posY)
pnlX <- sim$posX * diff(sim[, 3])
pnlY <- sim$posY * diff(sim[, 4])
pnl <- pnlX + pnlY
plot(signals[3:length(index(signals))], ylab='e', main = 'Trade signals at one-standard deviation', col=c('blue', 'black', 'black'), lwd=c(1,2,2))


daily <- (tail(cumsum(na.omit(pnl)), n=1)/1000)^(1/length(cumsum(na.omit(pnl)))) - 1
anual <- (1+daily)^252 - 1
sharpe <- (tail(cumsum(na.omit(pnl))))/sd(na.omit(pnl))
plot(sig)
as.vector(sig) 
sum(abs(na.omit(as.vector(sig$sig))))
Covid_Returns_BRDT_LREN <- cumsum(na.omit(pnl))
Covid_Info_BRDT_LREN <- c(as.numeric(num_trades), as.numeric(anual), sd(na.omit(pnl)), as.numeric(sigmas[j]))


# Plotando retorno da carteira --------------------------------------------

Covid_Master_Returns <- Covid_Returns_BBSE_CCRO$posX + Covid_Returns_BBSE_CYRE$posX + Covid_Returns_BRDT_EQTL$posX + Covid_Returns_BRDT_LREN$posX
plot(Covid_Master_Returns, legend.loc=1, main = "Preço de fechamento ajustado das ações - R$")
colnames(Covid_Master_Returns) <- c("Retorno da carteira")

Covid_All <- merge.xts(Covid_Master_Returns, Covid_Returns_BBSE_CCRO, Covid_Returns_BBSE_CYRE, Covid_Returns_BRDT_EQTL, Covid_Returns_BRDT_LREN)
colnames(Covid_All) <- c("Retorno da Carteira", "BBSE3 CCRO3", "BBSE3 CYRE3", "BRDT3 EQTL3", "BRDT3 LREN3")
plot(Covid_All, legend.loc = 1, main = "Retorno acumulado da carteira e dos pares em 2020 (YTD) - R$")

xx <- merge(Zeta$BRDT3.SA.Adjusted, Zeta$LREN3.SA.Adjusted, join="inner")
colnames(xx) <- c("BRDT3", "LREN3")
plot(xx, legend.loc=1, main = "Preço de fechamento ajustado das ações - R$")
Main_Info_BRDT_LREN
View(Returns_BRDT_LREN)



# Testes de hipótese para o período de 2020 -------------------------------

#Calculando os testes apenas para os 4 pares em 2020

ParesOmicron <- merge.xts(Omicron$BBSE3.SA.Adjusted, Omicron$LREN3.SA.Adjusted, Omicron$BRDT3.SA.Adjusted, Omicron$CCRO3.SA.Adjusted, Omicron$EQTL3.SA.Adjusted, Omicron$CYRE3.SA.Adjusted)
João <- data.frame()
n <- 6
for (i in 1:n){
  for (j in (i+1):n){
    teste1 <- egcm(ParesOmicron[,i], ParesOmicron[,j], urtest = "adf")
    teste2 <- egcm(ParesOmicron[,i], ParesOmicron[,j], urtest = "jo-e")
    teste3 <- egcm(ParesOmicron[,i], ParesOmicron[,j], urtest = "jo-t")
    teste4 <- egcm(ParesOmicron[,i], ParesOmicron[,j], urtest = "pp")
    info <- c(colnames(ParesOmicron)[i], colnames(ParesOmicron)[j], teste1$r.stat, teste1$r.p, teste2$r.stat, teste2$r.p, teste3$r.stat, teste3$r.p, teste4$r.stat, teste4$r.p)
    João <- rbind(João, info)
  }
}
colnames(João) <- Kappa_names
João

#Calculando os testes apenas para os 4 pares em 2018-2020

ParesY <- merge.xts(Y$BBSE3.SA.Adjusted, Y$LREN3.SA.Adjusted, Y$BRDT3.SA.Adjusted, Y$CCRO3.SA.Adjusted, Y$EQTL3.SA.Adjusted, Y$CYRE3.SA.Adjusted)
Paulo <- data.frame()
n <- 6
for (i in 1:n){
  for (j in (i+1):n){
    teste1 <- egcm(ParesY[,i], ParesY[,j], urtest = "adf")
    teste2 <- egcm(ParesY[,i], ParesY[,j], urtest = "jo-e")
    teste3 <- egcm(ParesY[,i], ParesY[,j], urtest = "jo-t")
    teste4 <- egcm(ParesY[,i], ParesY[,j], urtest = "pp")
    info <- c(colnames(ParesY)[i], colnames(ParesY)[j], teste1$r.stat, teste1$r.p, teste2$r.stat, teste2$r.p, teste3$r.stat, teste3$r.p, teste4$r.stat, teste4$r.p)
    Paulo <- rbind(Paulo, info)
  }
}
colnames(Paulo) <- Kappa_names
Paulo

#Calculando os testes  para todos os pares em 2020

João <- data.frame()
n <- 47
for (i in 1:n){
  for (j in (i+1):n){
    teste1 <- egcm(Omicron[,i], Omicron[,j], urtest = "adf")
    teste2 <- egcm(Omicron[,i], Omicron[,j], urtest = "jo-e")
    teste3 <- egcm(Omicron[,i], Omicron[,j], urtest = "jo-t")
    teste4 <- egcm(Omicron[,i], Omicron[,j], urtest = "pp")
    info <- c(colnames(Omicron)[i], colnames(Omicron)[j], teste1$r.stat, teste1$r.p, teste2$r.stat, teste2$r.p, teste3$r.stat, teste3$r.p, teste4$r.stat, teste4$r.p)
    João <- rbind(João, info)
  }
}
colnames(João) <- Kappa_names

#Definindoa aqueles que passam em todos os testes de hipótese
shared_alpha_2020 <- function(alpha){
  x <- dplyr::filter(João, João$`ADF p-value`<alpha)
  y <- dplyr::filter(João, João$`Johansen Eigenvalue p-value`<alpha)
  z <- dplyr::filter(João, João$`Johansen Trace p-value`<alpha)
  w <- dplyr::filter(João, João$`Phillips-Perron p-value`<alpha)
  return(dplyr::intersect(x,y,z,w))
}

Valid_2020  <- shared_alpha_2020(0.01)
Valid_2020

#Calculando os testes  para todos os pares em 2018-2020

João <- data.frame()
n <- 47
for (i in 1:n){
  for (j in (i+1):n){
    teste1 <- egcm(Y[,i], Y[,j], urtest = "adf")
    teste2 <- egcm(Y[,i], Y[,j], urtest = "jo-e")
    teste3 <- egcm(Y[,i], Y[,j], urtest = "jo-t")
    teste4 <- egcm(Y[,i], Y[,j], urtest = "pp")
    info <- c(colnames(Y)[i], colnames(Y)[j], teste1$r.stat, teste1$r.p, teste2$r.stat, teste2$r.p, teste3$r.stat, teste3$r.p, teste4$r.stat, teste4$r.p)
    João <- rbind(João, info)
  }
}
colnames(João) <- Kappa_names
Valid_2020  <- shared_alpha_2020(0.05)
Valid_2020

