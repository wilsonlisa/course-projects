library(survival)
library(tidyr)
library(ggplot2)
library(scales)
library(xtable)

set.seed(27)

n <- 100
nsamp <- 10000

### X[1:100,] is Exp(0.01), C[1:100,] is Exp(0.01)
### X[101:200,] is Weibull(2, 50), C[101:200,] is Weibull(2, 50)
### X[201:300,] is Normal(150, 50), C[201:300] is Normal(150, 50)
X <- matrix(nrow=3*n, ncol=nsamp)
C <- matrix(nrow=3*n, ncol=nsamp)

for (i in 1:nsamp){
    X[1:100, i] <- rexp(n=100, rate=0.01)
    C[1:100, i] <- rexp(n=100, rate=0.01)
    
    X[101:200, i] <- rweibull(n=100, 2, 100)
    C[101:200, i] <- rweibull(n=100, 2, 100)
    
    X[201:300, i] <- rnorm(n=100, 150, 50)
    C[201:300, i] <- rnorm(n=100, 150, 50)
}

Ti <- matrix(nrow=9*n, ncol=nsamp)

for (i in 1:nsamp){
    for (j in 1:n){
        Ti[j, i] <- min(X[j, i], C[j, i]) # X is Exp, C is Exp
        Ti[j+100, i] <- min(X[j, i], C[j+100, i]) # X is Exp, C is Weib
        Ti[j+200, i] <- min(X[j, i], C[j+200, i]) # X is Exp, C is Normal
        
        Ti[j+300, i] <- min(X[j+100, i], C[j, i]) # X is Weib, C is Exp
        Ti[j+400, i] <- min(X[j+100, i], C[j+100, i]) # X is Weib, C is Weib
        Ti[j+500, i] <- min(X[j+100, i], C[j+200, i]) # X is Weib, C is Normal
        
        Ti[j+600, i] <- min(X[j+200, i], C[j, i]) # X is Normal, C is Exp
        Ti[j+700, i] <- min(X[j+200, i], C[j+100, i]) # X is Normal, C is Weib
        Ti[j+800, i] <- min(X[j+200, i], C[j+200, i]) # X is Normal, C is Normal
    }
}

del <- matrix(nrow=9*n, ncol=nsamp)

for (i in 1:nsamp){
    for (j in 1:n){
        del[j, i] <- ifelse(X[j, i] < C[j, i], 1, 0)
        del[j+100, i] <- ifelse(X[j, i] < C[j+100, i], 1, 0)
        del[j+200, i] <- ifelse(X[j, i] < C[j+200, i], 1, 0)
        
        del[j+300, i] <- ifelse(X[j+100, i] < C[j, i], 1, 0)
        del[j+400, i] <- ifelse(X[j+100, i] < C[j+100, i], 1, 0)
        del[j+500, i] <- ifelse(X[j+100, i] < C[j+200, i], 1, 0)
        
        del[j+600, i] <- ifelse(X[j+200, i] < C[j, i], 1, 0)
        del[j+700, i] <- ifelse(X[j+200, i] < C[j+100, i], 1, 0)
        del[j+800, i] <- ifelse(X[j+200, i] < C[j+200, i], 1, 0)
    }
}

## Quantiles
p_seq <- seq(0.05,0.95,0.05)
quant_exp <- qexp(p_seq, rate=0.01)
quant_weib <- qweibull(p_seq, 2, 100)
quant_norm <- qnorm(p_seq, 150, 50)

## Color scale
hues <- c("Gill" = hue_pal()(3)[1], "Efron" = hue_pal()(3)[2], "S*" = hue_pal()(3)[3])

## X is Exp, C is Exp
# Gill's KM
KM_G_EE <- matrix(nrow=length(quant_exp), ncol=nsamp)
sum_G_EE <- rep(0, length(quant_exp))
sum2_G_EE <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[1:100, i], del[1:100, i]) ~ 1, type = "kaplan-meier"), times = quant_exp)$surv
    KM_G_EE[1:length(KM_init), i] <- KM_init
    KM_G_EE[, i] <- replace_na(KM_G_EE[, i], KM_G_EE[length(KM_init), i])
    
    sum_G_EE <- sum_G_EE + KM_G_EE[,i]
    sum2_G_EE <- sum2_G_EE + (KM_G_EE[,i])^2
}    

# Efron's KM
KM_E_EE <- matrix(nrow=length(quant_exp), ncol=nsamp)
sum_E_EE <- rep(0, length(quant_exp))
sum2_E_EE <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[1:100, i], del[1:100, i]) ~ 1, type = "kaplan-meier"), times = quant_exp)$surv
    KM_E_EE[1:length(KM_init), i] <- KM_init
    KM_E_EE[, i] <- replace_na(KM_E_EE[, i], 0)
    
    sum_E_EE <- sum_E_EE + KM_E_EE[,i]
    sum2_E_EE <- sum2_E_EE + (KM_E_EE[,i])^2
}  

# S*(t)
S_star_EE <- matrix(nrow=length(quant_exp), ncol=nsamp)
I_t <- matrix(nrow=100, ncol=length(quant_exp))
sum_S_EE <- rep(0, length(quant_exp))
sum2_S_EE <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    for (j in 1:length(quant_exp)){
        I_t[, j] <- ifelse(Ti[1:100,i] > quant_exp[j], 1, 0)
    }
    S_star_EE[, i] <- colMeans(I_t)/pexp(quant_exp, rate = 0.01, lower.tail = FALSE)
    
    sum_S_EE <- sum_S_EE + S_star_EE[,i]
    sum2_S_EE <- sum2_S_EE + (S_star_EE[,i])^2
}

# Bias
S_EE <- pexp(quant_exp, rate = 0.01, lower.tail = FALSE)

bias_G_EE <- (1/nsamp)*sum_G_EE - S_EE
bias_E_EE <- (1/nsamp)*sum_E_EE - S_EE
bias_S_EE <- (1/nsamp)*sum_S_EE - S_EE

abb_ind <- seq(1,19,2)
dmat <- matrix(c(2,3,5,5,5), nrow=5, ncol=length(abb_ind)+1)
# dmat2 <- matrix(c(2,3,5,5,5), nrow=5, ncol=10)

bias_EE <- rbind(round(p_seq, 2), round(quant_exp,3), bias_G_EE, bias_E_EE, bias_S_EE)
rownames(bias_EE) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
# print(xtable(bias_EE[,1:10], digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, file="bias_EE.tex")
print(xtable(bias_EE[,abb_ind], caption="Bias when X is Exp(0.01), C is Exp(0.01)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="bias_EE.tex")

ggplot() +
    geom_line(aes(p_seq, bias_G_EE, color="Gill")) +
    geom_line(aes(p_seq, bias_E_EE, color="Efron")) +
    geom_line(aes(p_seq, bias_S_EE, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="Bias", title="Bias for X ~ Exp(0.01), C ~ Exp(0.01)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

# MSE
mse_G_EE <- (1/nsamp)*sum2_G_EE - ((1/nsamp)*sum_G_EE)^2 + bias_G_EE^2
mse_E_EE <- (1/nsamp)*sum2_E_EE - ((1/nsamp)*sum_E_EE)^2 + bias_E_EE^2
mse_S_EE <- (1/nsamp)*sum2_S_EE - ((1/nsamp)*sum_S_EE)^2 + bias_S_EE^2

mse_EE <- rbind(round(p_seq, 2), round(quant_exp,3), mse_G_EE, mse_E_EE, mse_S_EE)
rownames(mse_EE) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
# print(xtable(mse_EE[,1:10], digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, file="mse_EE.tex")
print(xtable(mse_EE[,abb_ind], caption="MSE when X is Exp(0.01), C is Exp(0.01)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="mse_EE.tex")

ggplot() +
    geom_line(aes(p_seq, mse_G_EE, color="Gill")) +
    geom_line(aes(p_seq, mse_E_EE, color="Efron")) +
    geom_line(aes(p_seq, mse_S_EE, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="MSE", title="MSE for X ~ Exp(0.01), C ~ Exp(0.01)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

## X is Exp, C is Weibull
# C cdf dominates for later values
ggplot(data=data.frame(x=seq(0,300)), aes(x)) +
    stat_function(fun=pexp, n=100, args=list(rate=0.01)) +
    stat_function(fun=pweibull, n=100, args=list(2, 100), color="blue", linetype="dashed")

# Gill's KM
KM_G_EW <- matrix(nrow=length(quant_exp), ncol=nsamp)
sum_G_EW <- rep(0, length(quant_exp))
sum2_G_EW <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[101:200, i], del[101:200, i]) ~ 1, type = "kaplan-meier"), times = quant_exp)$surv
    KM_G_EW[1:length(KM_init), i] <- KM_init
    KM_G_EW[, i] <- replace_na(KM_G_EW[, i], KM_G_EW[length(KM_init), i])
    
    sum_G_EW <- sum_G_EW + KM_G_EW[,i]
    sum2_G_EW <- sum2_G_EW + (KM_G_EW[,i])^2
}    

# Efron's KM
KM_E_EW <- matrix(nrow=length(quant_exp), ncol=nsamp)
sum_E_EW <- rep(0, length(quant_exp))
sum2_E_EW <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[101:200, i], del[101:200, i]) ~ 1, type = "kaplan-meier"), times = quant_exp)$surv
    KM_E_EW[1:length(KM_init), i] <- KM_init
    KM_E_EW[, i] <- replace_na(KM_E_EW[, i], 0)
    
    sum_E_EW <- sum_E_EW + KM_E_EW[,i]
    sum2_E_EW <- sum2_E_EW + (KM_E_EW[,i])^2
}  

# S*(t)
S_star_EW <- matrix(nrow=length(quant_exp), ncol=nsamp)
I_t <- matrix(nrow=100, ncol=length(quant_exp))
sum_S_EW <- rep(0, length(quant_exp))
sum2_S_EW <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    for (j in 1:length(quant_exp)){
        I_t[, j] <- ifelse(Ti[101:200,i] > quant_exp[j], 1, 0)
    }
    S_star_EW[, i] <- colMeans(I_t)/pweibull(quant_exp, 2, 100, lower.tail = FALSE)

    sum_S_EW <- sum_S_EW + S_star_EW[,i]
    sum2_S_EW <- sum2_S_EW + (S_star_EW[,i])^2
}

# Bias
S_EW <- pexp(quant_exp, rate = 0.01, lower.tail = FALSE)

bias_G_EW <- (1/nsamp)*sum_G_EW - S_EW
bias_E_EW <- (1/nsamp)*sum_E_EW - S_EW
bias_S_EW <- (1/nsamp)*sum_S_EW - S_EW

bias_EW <- rbind(round(p_seq, 2), round(quant_exp,3), bias_G_EW, bias_E_EW, bias_S_EW)
rownames(bias_EW) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(bias_EW[,abb_ind], caption="Bias when X is Exp(0.01), C is Weibull(2, 100)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, file="bias_EW.tex")

ggplot() +
    geom_line(aes(p_seq, bias_G_EW, color="Gill")) +
    geom_line(aes(p_seq, bias_E_EW, color="Efron")) +
    geom_line(aes(p_seq, bias_S_EW, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="Bias", title="Bias for X ~ Exp(0.01), C ~ Weibull(2, 100)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

# MSE
mse_G_EW <- (1/nsamp)*sum2_G_EW - ((1/nsamp)*sum_G_EW)^2 + bias_G_EW^2
mse_E_EW <- (1/nsamp)*sum2_E_EW - ((1/nsamp)*sum_E_EW)^2 + bias_E_EW^2
mse_S_EW <- (1/nsamp)*sum2_S_EW - ((1/nsamp)*sum_S_EW)^2 + bias_S_EW^2

mse_EW <- rbind(round(p_seq, 2), round(quant_exp,3), mse_G_EW, mse_E_EW, mse_S_EW)
rownames(mse_EW) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(mse_EW[,abb_ind], caption="MSE when X is Exp(0.01), C is Weibull(2, 100)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, file="mse_EW.tex")

ggplot() +
    geom_line(aes(p_seq, mse_G_EW, color="Gill")) +
    geom_line(aes(p_seq, mse_E_EW, color="Efron")) +
    geom_line(aes(p_seq, mse_S_EW, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="MSE", title="MSE for X ~ Exp(0.01), C ~ Weibull(2, 100)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

## X is Exp, C is Normal
# X cdf dominates until ~200, C only little more for later
ggplot(data=data.frame(x=seq(0,300)), aes(x)) +
    stat_function(fun=pexp, n=100, args=list(rate=0.01)) +
    stat_function(fun=pnorm, n=100, args=list(150, 50), color="blue", linetype="dashed")

# Gill's KM
KM_G_EN <- matrix(nrow=length(quant_exp), ncol=nsamp)
sum_G_EN <- rep(0, length(quant_exp))
sum2_G_EN <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[201:300, 1], del[201:300, 1]) ~ 1, type = "kaplan-meier"), times = quant_exp)$surv
    KM_G_EN[1:length(KM_init), i] <- KM_init
    KM_G_EN[, i] <- replace_na(KM_G_EN[, i], KM_G_EN[length(KM_init), i])
    
    sum_G_EN <- sum_G_EN + KM_G_EN[,i]
    sum2_G_EN <- sum2_G_EN + (KM_G_EN[,i])^2
}    

# Efron's KM
KM_E_EN <- matrix(nrow=length(quant_exp), ncol=nsamp)
sum_E_EN <- rep(0, length(quant_exp))
sum2_E_EN <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[201:300, i], del[201:300, i]) ~ 1, type = "kaplan-meier"), times = quant_exp)$surv
    KM_E_EN[1:length(KM_init), i] <- KM_init
    KM_E_EN[, i] <- replace_na(KM_E_EN[, i], 0)
    
    sum_E_EN <- sum_E_EN + KM_E_EN[,i]
    sum2_E_EN <- sum2_E_EN + (KM_E_EN[,i])^2
}  

# S*(t)
S_star_EN <- matrix(nrow=length(quant_exp), ncol=nsamp)
I_t <- matrix(nrow=100, ncol=length(quant_exp))
sum_S_EN <- rep(0, length(quant_exp))
sum2_S_EN <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    for (j in 1:length(quant_exp)){
        I_t[, j] <- ifelse(Ti[201:300,i] > quant_exp[j], 1, 0)
    }
    S_star_EN[, i] <- colMeans(I_t)/pnorm(quant_exp, 150, 50, lower.tail = FALSE)
    
    sum_S_EN <- sum_S_EN + S_star_EN[,i]
    sum2_S_EN <- sum2_S_EN + (S_star_EN[,i])^2
}

# Bias
S_EN <- pexp(quant_exp, rate = 0.01, lower.tail = FALSE)

bias_G_EN <- (1/nsamp)*sum_G_EN - S_EN
bias_E_EN <- (1/nsamp)*sum_E_EN - S_EN
bias_S_EN <- (1/nsamp)*sum_S_EN - S_EN

ggplot() +
    geom_line(aes(p_seq, bias_G_EN, color="Gill")) +
    geom_line(aes(p_seq, bias_E_EN, color="Efron")) +
    geom_line(aes(p_seq, bias_S_EN, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="Bias", title="Bias for X ~ Exp(0.01), C ~ Normal(150, 50)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

# MSE
mse_G_EN <- (1/nsamp)*sum2_G_EN - ((1/nsamp)*sum_G_EN)^2 + bias_G_EN^2
mse_E_EN <- (1/nsamp)*sum2_E_EN - ((1/nsamp)*sum_E_EN)^2 + bias_E_EN^2
mse_S_EN <- (1/nsamp)*sum2_S_EN - ((1/nsamp)*sum_S_EN)^2 + bias_S_EN^2

ggplot() +
    geom_line(aes(p_seq, mse_G_EN, color="Gill")) +
    geom_line(aes(p_seq, mse_E_EN, color="Efron")) +
    geom_line(aes(p_seq, mse_S_EN, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="MSE", title="MSE for X ~ Exp(0.01), C ~ Normal(150, 50)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

# unusually high S_star_EN[19, ] values: 276, 381, 411, 560, 997, 1184 
# driving high MSE for S*


## X is Weibull, C is Exp
# X dominates for later
ggplot(data=data.frame(x=seq(0,300)), aes(x)) +
    stat_function(fun=pexp, n=100, args=list(rate=0.01), color="blue", linetype="dashed") +
    stat_function(fun=pweibull, n=100, args=list(2, 100))

# Gill's KM
KM_G_WE <- matrix(nrow=length(quant_weib), ncol=nsamp)
sum_G_WE <- rep(0, length(quant_weib))
sum2_G_WE <- rep(0, length(quant_weib))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[301:400, i], del[301:400, i]) ~ 1, type = "kaplan-meier"), times = quant_weib)$surv
    KM_G_WE[1:length(KM_init), i] <- KM_init
    KM_G_WE[, i] <- replace_na(KM_G_WE[, i], KM_G_WE[length(KM_init), i])
    
    sum_G_WE <- sum_G_WE + KM_G_WE[,i]
    sum2_G_WE <- sum2_G_WE + (KM_G_WE[,i])^2
}    

# Efron's KM
KM_E_WE <- matrix(nrow=length(quant_exp), ncol=nsamp)
sum_E_WE <- rep(0, length(quant_exp))
sum2_E_WE <- rep(0, length(quant_exp))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[301:400, i], del[301:400, i]) ~ 1, type = "kaplan-meier"), times = quant_weib)$surv
    KM_E_WE[1:length(KM_init), i] <- KM_init
    KM_E_WE[, i] <- replace_na(KM_E_WE[, i], 0)
    
    sum_E_WE <- sum_E_WE + KM_E_WE[,i]
    sum2_E_WE <- sum2_E_WE + (KM_E_WE[,i])^2
}  

# S*(t)
S_star_WE <- matrix(nrow=length(quant_weib), ncol=nsamp)
I_t <- matrix(nrow=100, ncol=length(quant_weib))
sum_S_WE <- rep(0, length(quant_weib))
sum2_S_WE <- rep(0, length(quant_weib))

for (i in 1:nsamp){
    for (j in 1:length(quant_weib)){
        I_t[, j] <- ifelse(Ti[301:400,i] > quant_weib[j], 1, 0)
    }
    S_star_WE[, i] <- colMeans(I_t)/pexp(quant_weib, rate = 0.01, lower.tail = FALSE)
    
    sum_S_WE <- sum_S_WE + S_star_WE[,i]
    sum2_S_WE <- sum2_S_WE + (S_star_WE[,i])^2
}

# Bias
S_WE <- pweibull(quant_weib, 2, 50, lower.tail = FALSE)

bias_G_WE <- (1/nsamp)*sum_G_WE - S_WE
bias_E_WE <- (1/nsamp)*sum_E_WE - S_WE
bias_S_WE <- (1/nsamp)*sum_S_WE - S_WE

ggplot() +
    geom_line(aes(p_seq, bias_G_WE, color="Gill")) +
    geom_line(aes(p_seq, bias_E_WE, color="Efron")) +
    geom_line(aes(p_seq, bias_S_WE, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="Bias", title="Bias for X ~ Weibull(2, 100), C ~ Exp(0.01)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

# MSE
mse_G_WE <- (1/nsamp)*sum2_G_WE - ((1/nsamp)*sum_G_WE)^2 + bias_G_WE^2
mse_E_WE <- (1/nsamp)*sum2_E_WE - ((1/nsamp)*sum_E_WE)^2 + bias_E_WE^2
mse_S_WE <- (1/nsamp)*sum2_S_WE - ((1/nsamp)*sum_S_WE)^2 + bias_S_WE^2

ggplot() +
    geom_line(aes(p_seq, mse_G_WE, color="Gill")) +
    geom_line(aes(p_seq, mse_E_WE, color="Efron")) +
    geom_line(aes(p_seq, mse_S_WE, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="MSE", title="MSE for X ~ Weibull(2, 100), C ~ Exp(0.01)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

## X is Weibull, C is Weibull
# Gill's KM
KM_G_WW <- matrix(nrow=length(quant_weib), ncol=nsamp)
sum_G_WW <- rep(0, length(quant_weib))
sum2_G_WW <- rep(0, length(quant_weib))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[401:500, i], del[401:500, i]) ~ 1, type = "kaplan-meier"), times = quant_weib)$surv
    KM_G_WW[1:length(KM_init), i] <- KM_init
    KM_G_WW[, i] <- replace_na(KM_G_WW[, i], KM_G_WW[length(KM_init), i])
    
    sum_G_WW <- sum_G_WW + KM_G_WW[,i]
    sum2_G_WW <- sum2_G_WW + (KM_G_WW[,i])^2
}    

# Efron's KM
KM_E_WW <- matrix(nrow=length(quant_weib), ncol=nsamp)
sum_E_WW <- rep(0, length(quant_weib))
sum2_E_WW <- rep(0, length(quant_weib))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[401:500, i], del[401:500, i]) ~ 1, type = "kaplan-meier"), times = quant_weib)$surv
    KM_E_WW[1:length(KM_init), i] <- KM_init
    KM_E_WW[, i] <- replace_na(KM_E_WW[, i], 0)
    
    sum_E_WW <- sum_E_WW + KM_E_WW[,i]
    sum2_E_WW <- sum2_E_WW + (KM_E_WW[,i])^2
}  

# S*(t)
S_star_WW <- matrix(nrow=length(quant_weib), ncol=nsamp)
I_t <- matrix(nrow=100, ncol=length(quant_weib))
sum_S_WW <- rep(0, length(quant_weib))
sum2_S_WW <- rep(0, length(quant_weib))

for (i in 1:nsamp){
    for (j in 1:length(quant_weib)){
        I_t[, j] <- ifelse(Ti[401:500,i] > quant_weib[j], 1, 0)
    }
    S_star_WW[, i] <- colMeans(I_t)/pweibull(quant_weib, 2, 100, lower.tail = FALSE)
    
    sum_S_WW <- sum_S_WW + S_star_WW[,i]
    sum2_S_WW <- sum2_S_WW + (S_star_WW[,i])^2
}

# Bias
S_WW <- pweibull(quant_weib, 2, 100, lower.tail = FALSE)

bias_G_WW <- (1/nsamp)*sum_G_WW - S_WW
bias_E_WW <- (1/nsamp)*sum_E_WW - S_WW
bias_S_WW <- (1/nsamp)*sum_S_WW - S_WW

ggplot() +
    geom_line(aes(p_seq, bias_G_WW, color="Gill")) +
    geom_line(aes(p_seq, bias_E_WW, color="Efron")) +
    geom_line(aes(p_seq, bias_S_WW, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="Bias", title="Bias for X ~ Weibull(2, 100), C ~ Weibull(2, 100)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14))  

# MSE
mse_G_WW <- (1/nsamp)*sum2_G_WW - ((1/nsamp)*sum_G_WW)^2 + bias_G_WW^2
mse_E_WW <- (1/nsamp)*sum2_E_WW - ((1/nsamp)*sum_E_WW)^2 + bias_E_WW^2
mse_S_WW <- (1/nsamp)*sum2_S_WW - ((1/nsamp)*sum_S_WW)^2 + bias_S_WW^2

ggplot() +
    geom_line(aes(p_seq, mse_G_WW, color="Gill")) +
    geom_line(aes(p_seq, mse_E_WW, color="Efron")) +
    geom_line(aes(p_seq, mse_S_WW, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="MSE", title="MSE for X ~ Weibull(2, 100), C ~ Weibull(2, 100)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

## X is Weibull, C in Normal
# X dominates for whole time
ggplot(data=data.frame(x=seq(0,300)), aes(x)) +
    stat_function(fun=pweibull, n=100, args=list(2, 100)) +
    stat_function(fun=pnorm, n=100, args=list(150, 50), color="blue", linetype="dashed")

# Gill's KM
KM_G_WN <- matrix(nrow=length(quant_weib), ncol=nsamp)
sum_G_WN <- rep(0, length(quant_weib))
sum2_G_WN <- rep(0, length(quant_weib))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[501:600, i], del[501:600, i]) ~ 1, type = "kaplan-meier"), times = quant_weib)$surv
    KM_G_WN[1:length(KM_init), i] <- KM_init
    KM_G_WN[, i] <- replace_na(KM_G_WN[, i], KM_G_WN[length(KM_init), i])
    
    sum_G_WN <- sum_G_WN + KM_G_WN[,i]
    sum2_G_WN <- sum2_G_WN + (KM_G_WN[,i])^2
}    

# Efron's KM
KM_E_WN <- matrix(nrow=length(quant_weib), ncol=nsamp)
sum_E_WN <- rep(0, length(quant_weib))
sum2_E_WN <- rep(0, length(quant_weib))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[501:600, i], del[501:600, i]) ~ 1, type = "kaplan-meier"), times = quant_weib)$surv
    KM_E_WN[1:length(KM_init), i] <- KM_init
    KM_E_WN[, i] <- replace_na(KM_E_WN[, i], 0)
    
    sum_E_WN <- sum_E_WN + KM_E_WN[,i]
    sum2_E_WN <- sum2_E_WN + (KM_E_WN[,i])^2
}  

# S*(t)
S_star_WN <- matrix(nrow=length(quant_weib), ncol=nsamp)
I_t <- matrix(nrow=100, ncol=length(quant_weib))
sum_S_WN <- rep(0, length(quant_weib))
sum2_S_WN <- rep(0, length(quant_weib))

for (i in 1:nsamp){
    for (j in 1:length(quant_weib)){
        I_t[, j] <- ifelse(Ti[501:600,i] > quant_weib[j], 1, 0)
    }
    S_star_WN[, i] <- colMeans(I_t)/pnorm(quant_weib, 150, 50, lower.tail = FALSE)
    
    sum_S_WN <- sum_S_WN + S_star_WN[,i]
    sum2_S_WN <- sum2_S_WN + (S_star_WN[,i])^2
}

# Bias
S_WN <- pweibull(quant_weib, 2, 50, lower.tail = FALSE)

bias_G_WN <- (1/nsamp)*sum_G_WN - S_WN
bias_E_WN <- (1/nsamp)*sum_E_WN - S_WN
bias_S_WN <- (1/nsamp)*sum_S_WN - S_WN

ggplot() +
    geom_line(aes(p_seq, bias_G_WN, color="Gill")) +
    geom_line(aes(p_seq, bias_E_WN, color="Efron")) +
    geom_line(aes(p_seq, bias_S_WN, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="Bias", title="Bias for X ~ Weibull(2, 100), C ~ Normal(150, 50)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

# MSE
mse_G_WN <- (1/nsamp)*sum2_G_WN - ((1/nsamp)*sum_G_WN)^2 + bias_G_WN^2
mse_E_WN <- (1/nsamp)*sum2_E_WN - ((1/nsamp)*sum_E_WN)^2 + bias_E_WN^2
mse_S_WN <- (1/nsamp)*sum2_S_WN - ((1/nsamp)*sum_S_WN)^2 + bias_S_WN^2

ggplot() +
    geom_line(aes(p_seq, mse_G_WN, color="Gill")) +
    geom_line(aes(p_seq, mse_E_WN, color="Efron")) +
    geom_line(aes(p_seq, mse_S_WN, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="MSE", title="MSE for X ~ Weibull(2, 100), C ~ Normal(150, 50)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 


## X is Normal, C is Exponential
# C dominates until ~200, X only slightly for later
ggplot(data=data.frame(x=seq(0,300)), aes(x)) +
    stat_function(fun=pnorm, n=100, args=list(150, 50)) +
    stat_function(fun=pexp, n=100, args=list(rate=0.01), color="blue", linetype="dashed")

# Gill's KM
KM_G_NE <- matrix(nrow=length(quant_norm), ncol=nsamp)
sum_G_NE <- rep(0, length(quant_norm))
sum2_G_NE <- rep(0, length(quant_norm))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[601:700, i], del[601:700, i]) ~ 1, type = "kaplan-meier"), times = quant_norm)$surv
    KM_G_NE[1:length(KM_init), i] <- KM_init
    KM_G_NE[, i] <- replace_na(KM_G_NE[, i], KM_G_NE[length(KM_init), i])
    
    sum_G_NE <- sum_G_NE + KM_G_NE[,i]
    sum2_G_NE <- sum2_G_NE + (KM_G_NE[,i])^2
}    

# Efron's KM
KM_E_NE <- matrix(nrow=length(quant_norm), ncol=nsamp)
sum_E_NE <- rep(0, length(quant_norm))
sum2_E_NE <- rep(0, length(quant_norm))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[601:700, i], del[601:700, i]) ~ 1, type = "kaplan-meier"), times = quant_norm)$surv
    KM_E_NE[1:length(KM_init), i] <- KM_init
    KM_E_NE[, i] <- replace_na(KM_E_NE[, i], 0)
    
    sum_E_NE <- sum_E_NE + KM_E_NE[,i]
    sum2_E_NE <- sum2_E_NE + (KM_E_NE[,i])^2
}  

# S*(t)
S_star_NE <- matrix(nrow=length(quant_norm), ncol=nsamp)
I_t <- matrix(nrow=100, ncol=length(quant_norm))
sum_S_NE <- rep(0, length(quant_norm))
sum2_S_NE <- rep(0, length(quant_norm))

for (i in 1:nsamp){
    for (j in 1:length(quant_norm)){
        I_t[, j] <- ifelse(Ti[601:700,i] > quant_norm[j], 1, 0)
    }
    S_star_NE[, i] <- colMeans(I_t)/pexp(quant_norm, rate = 0.01, lower.tail = FALSE)
    
    sum_S_NE <- sum_S_NE + S_star_NE[,i]
    sum2_S_NE <- sum2_S_NE + (S_star_NE[,i])^2
}

# Bias
S_NE <- pnorm(quant_norm, 150, 50, lower.tail = FALSE)

bias_G_NE <- (1/nsamp)*sum_G_NE - S_NE
bias_E_NE <- (1/nsamp)*sum_E_NE - S_NE
bias_S_NE <- (1/nsamp)*sum_S_NE - S_NE

ggplot() +
    geom_line(aes(p_seq, bias_G_NE, color="Gill")) +
    geom_line(aes(p_seq, bias_E_NE, color="Efron")) +
    geom_line(aes(p_seq, bias_S_NE, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="Bias", title="Bias for X ~ Normal(150, 50), C ~ Exp(0.01)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

# MSE
mse_G_NE <- (1/nsamp)*sum2_G_NE - ((1/nsamp)*sum_G_NE)^2 + bias_G_NE^2
mse_E_NE <- (1/nsamp)*sum2_E_NE - ((1/nsamp)*sum_E_NE)^2 + bias_E_NE^2
mse_S_NE <- (1/nsamp)*sum2_S_NE - ((1/nsamp)*sum_S_NE)^2 + bias_S_NE^2

ggplot() +
    geom_line(aes(p_seq, mse_G_NE, color="Gill")) +
    geom_line(aes(p_seq, mse_E_NE, color="Efron")) +
    geom_line(aes(p_seq, mse_S_NE, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="MSE", title="MSE for X ~ Normal(150, 50), C ~ Exp(0.01)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

## X is Normal, C is Weibull
# C dominates for whole time
ggplot(data=data.frame(x=seq(0,300)), aes(x)) +
    stat_function(fun=pnorm, n=100, args=list(150, 50)) +
    stat_function(fun=pweibull, n=100, args=list(2, 100), color="blue", linetype="dashed")

# Gill's KM
KM_G_NW <- matrix(nrow=length(quant_norm), ncol=nsamp)
sum_G_NW <- rep(0, length(quant_norm))
sum2_G_NW <- rep(0, length(quant_norm))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[701:800, i], del[701:800, i]) ~ 1, type = "kaplan-meier"), times = quant_norm)$surv
    KM_G_NW[1:length(KM_init), i] <- KM_init
    KM_G_NW[, i] <- replace_na(KM_G_NW[, i], KM_G_NW[length(KM_init), i])
    
    sum_G_NW <- sum_G_NW + KM_G_NW[,i]
    sum2_G_NW <- sum2_G_NW + (KM_G_NW[,i])^2
}    

# Efron's KM
KM_E_NW <- matrix(nrow=length(quant_norm), ncol=nsamp)
sum_E_NW <- rep(0, length(quant_norm))
sum2_E_NW <- rep(0, length(quant_norm))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[701:800, i], del[701:800, i]) ~ 1, type = "kaplan-meier"), times = quant_norm)$surv
    KM_E_NW[1:length(KM_init), i] <- KM_init
    KM_E_NW[, i] <- replace_na(KM_E_NW[, i], 0)
    
    sum_E_NW <- sum_E_NW + KM_E_NW[,i]
    sum2_E_NW <- sum2_E_NW + (KM_E_NW[,i])^2
}  

# S*(t)
S_star_NW <- matrix(nrow=length(quant_norm), ncol=nsamp)
I_t <- matrix(nrow=100, ncol=length(quant_norm))
sum_S_NW <- rep(0, length(quant_norm))
sum2_S_NW <- rep(0, length(quant_norm))

for (i in 1:nsamp){
    for (j in 1:length(quant_norm)){
        I_t[, j] <- ifelse(Ti[701:800,i] > quant_norm[j], 1, 0)
    }
    S_star_NW[, i] <- colMeans(I_t)/pweibull(quant_norm, 2, 100, lower.tail = FALSE)
    
    sum_S_NW <- sum_S_NW + S_star_NW[,i]
    sum2_S_NW <- sum2_S_NW + (S_star_NW[,i])^2
}

# Bias
S_NW <- pnorm(quant_norm, 150, 50, lower.tail = FALSE)

bias_G_NW <- (1/nsamp)*sum_G_NW - S_NW
bias_E_NW <- (1/nsamp)*sum_E_NW - S_NW
bias_S_NW <- (1/nsamp)*sum_S_NW - S_NW

ggplot() +
    geom_line(aes(p_seq, bias_G_NW, color="Gill")) +
    geom_line(aes(p_seq, bias_E_NW, color="Efron")) +
    geom_line(aes(p_seq, bias_S_NW, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="Bias", title="Bias for X ~ Normal(150, 50), C ~ Weibull(2, 100)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

# MSE
mse_G_NW <- (1/nsamp)*sum2_G_NW - ((1/nsamp)*sum_G_NW)^2 + bias_G_NW^2
mse_E_NW <- (1/nsamp)*sum2_E_NW - ((1/nsamp)*sum_E_NW)^2 + bias_E_NW^2
mse_S_NW <- (1/nsamp)*sum2_S_NW - ((1/nsamp)*sum_S_NW)^2 + bias_S_NW^2

ggplot() +
    geom_line(aes(p_seq, mse_G_NW, color="Gill")) +
    geom_line(aes(p_seq, mse_E_NW, color="Efron")) +
    geom_line(aes(p_seq, mse_S_NW, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="MSE", title="MSE for X ~ Normal(150, 50), C ~ Weibull(2, 100)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

## X is Normal, C is Normal
# Gill's KM
KM_G_NN <- matrix(nrow=length(quant_norm), ncol=nsamp)
sum_G_NN <- rep(0, length(quant_norm))
sum2_G_NN <- rep(0, length(quant_norm))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[801:900, i], del[801:900, i]) ~ 1, type = "kaplan-meier"), times = quant_norm)$surv
    KM_G_NN[1:length(KM_init), i] <- KM_init
    KM_G_NN[, i] <- replace_na(KM_G_NN[, i], KM_G_NN[length(KM_init), i])
    
    sum_G_NN <- sum_G_NN + KM_G_NN[,i]
    sum2_G_NN <- sum2_G_NN + (KM_G_NN[,i])^2
}    

# Efron's KM
KM_E_NN <- matrix(nrow=length(quant_norm), ncol=nsamp)
sum_E_NN <- rep(0, length(quant_norm))
sum2_E_NN <- rep(0, length(quant_norm))

for (i in 1:nsamp){
    KM_init <- summary(survfit(Surv(Ti[801:900, i], del[801:900, i]) ~ 1, type = "kaplan-meier"), times = quant_norm)$surv
    KM_E_NN[1:length(KM_init), i] <- KM_init
    KM_E_NN[, i] <- replace_na(KM_E_NN[, i], 0)
    
    sum_E_NN <- sum_E_NN + KM_E_NN[,i]
    sum2_E_NN <- sum2_E_NN + (KM_E_NN[,i])^2
}  

# S*(t)
S_star_NN <- matrix(nrow=length(quant_norm), ncol=nsamp)
I_t <- matrix(nrow=100, ncol=length(quant_norm))
sum_S_NN <- rep(0, length(quant_norm))
sum2_S_NN <- rep(0, length(quant_norm))

for (i in 1:nsamp){
    for (j in 1:length(quant_norm)){
        I_t[, j] <- ifelse(Ti[801:900,i] > quant_norm[j], 1, 0)
    }
    S_star_NN[, i] <- colMeans(I_t)/pnorm(quant_norm, 150, 50, lower.tail = FALSE)
    
    sum_S_NN <- sum_S_NN + S_star_NN[,i]
    sum2_S_NN <- sum2_S_NN + (S_star_NN[,i])^2
}

# Bias
S_NN <- pnorm(quant_norm, 150, 50, lower.tail = FALSE)

bias_G_NN <- (1/nsamp)*sum_G_NN - S_NN
bias_E_NN <- (1/nsamp)*sum_E_NN - S_NN
bias_S_NN <- (1/nsamp)*sum_S_NN - S_NN

ggplot() +
    geom_line(aes(p_seq, bias_G_NN, color="Gill")) +
    geom_line(aes(p_seq, bias_E_NN, color="Efron")) +
    geom_line(aes(p_seq, bias_S_NN, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="Bias", title="Bias for X ~ Normal(150, 50), C ~ Normal(150, 50)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14)) 

# MSE
mse_G_NN <- (1/nsamp)*sum2_G_NN - ((1/nsamp)*sum_G_NN)^2 + bias_G_NN^2
mse_E_NN <- (1/nsamp)*sum2_E_NN - ((1/nsamp)*sum_E_NN)^2 + bias_E_NN^2
mse_S_NN <- (1/nsamp)*sum2_S_NN - ((1/nsamp)*sum_S_NN)^2 + bias_S_NN^2

ggplot() +
    geom_line(aes(p_seq, mse_G_NN, color="Gill")) +
    geom_line(aes(p_seq, mse_E_NN, color="Efron")) +
    geom_line(aes(p_seq, mse_S_NN, color="S*")) +
    geom_hline(yintercept=0, linetype="dotted") +
    labs(y="MSE", title="MSE for X ~ Normal(150, 50), C ~ Normal(150, 50)", color="Legend") +
    scale_x_continuous(breaks = p_seq) +
    scale_color_manual(values = hues) + 
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size=12),
          legend.text = element_text(size=12), plot.title = element_text(size=16),
          legend.title = element_text(size=14), axis.title = element_text(size=14))  


### rest of tables
bias_EW <- rbind(round(p_seq, 2), round(quant_exp,3), bias_G_EW, bias_E_EW, bias_S_EW)
rownames(bias_EW) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(bias_EW[,abb_ind], caption="Bias when X is Exp(0.01), C is Weibull(2, 100)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="bias_EW.tex")

mse_EW <- rbind(round(p_seq, 2), round(quant_exp,3), mse_G_EW, mse_E_EW, mse_S_EW)
rownames(mse_EW) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(mse_EW[,abb_ind], caption="MSE when X is Exp(0.01), C is Weibull(2, 100)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="mse_EW.tex")

bias_EN <- rbind(round(p_seq, 2), round(quant_exp,3), bias_G_EN, bias_E_EN, bias_S_EN)
rownames(bias_EN) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(bias_EN[,abb_ind], caption="Bias when X is Exp(0.01), C is Normal(150, 50)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="bias_EN.tex")

mse_EN <- rbind(round(p_seq, 2), round(quant_exp,3), mse_G_EN, mse_E_EN, mse_S_EN)
rownames(mse_EN) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(mse_EN[,abb_ind], caption="MSE when X is Exp(0.01), C is Normal(150, 50)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="mse_EN.tex")


bias_WE <- rbind(round(p_seq, 2), round(quant_weib,3), bias_G_WE, bias_E_WE, bias_S_WE)
rownames(bias_WE) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(bias_WE[,abb_ind], caption="Bias when X is Weibull(2, 100), C is Exp(0.01)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="bias_WE.tex")

mse_WE <- rbind(round(p_seq, 2), round(quant_weib,3), mse_G_WE, mse_E_WE, mse_S_WE)
rownames(mse_WE) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(mse_WE[,abb_ind], caption="MSE when X is Weibull(2, 100), C is Exp(0.01)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="mse_WE.tex")

bias_WW <- rbind(round(p_seq, 2), round(quant_weib,3), bias_G_WW, bias_E_WW, bias_S_WW)
rownames(bias_WW) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(bias_WW[,abb_ind], caption="Bias when X is Weibull(2, 100), C is Weibull(2, 100)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="bias_WW.tex")

mse_WW <- rbind(round(p_seq, 2), round(quant_weib,3), mse_G_WW, mse_E_WW, mse_S_WW)
rownames(mse_WW) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(mse_WW[,abb_ind], caption="MSE when X is Weibull(2, 100), C is Weibull(2, 100)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="mse_WW.tex")

bias_WN <- rbind(round(p_seq, 2), round(quant_weib,3), bias_G_WN, bias_E_WN, bias_S_WN)
rownames(bias_WN) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(bias_WN[,abb_ind], caption="Bias when X is Weibull(2, 100), C is Normal(150, 50)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="bias_WN.tex")

mse_WN <- rbind(round(p_seq, 2), round(quant_weib,3), mse_G_WN, mse_E_WN, mse_S_WN)
rownames(mse_WN) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(mse_WN[,abb_ind], caption="MSE when X is Weibull(2, 100), C is Normal(150, 50)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="mse_WN.tex")


bias_NE <- rbind(round(p_seq, 2), round(quant_norm,3), bias_G_NE, bias_E_NE, bias_S_NE)
rownames(bias_NE) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(bias_NE[,abb_ind], caption="Bias when X is Normal(150, 50), C is Exp(0.01)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="bias_NE.tex")

mse_NE <- rbind(round(p_seq, 2), round(quant_norm,3), mse_G_NE, mse_E_NE, mse_S_NE)
rownames(mse_NE) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(mse_NE[,abb_ind], caption="MSE when X is Normal(150, 50), C is Exp(0.01)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="mse_NE.tex")

bias_NW <- rbind(round(p_seq, 2), round(quant_norm,3), bias_G_NW, bias_E_NW, bias_S_NW)
rownames(bias_NW) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(bias_NW[,abb_ind], caption="Bias when X is Normal(150, 50), C is Weibull(2, 100)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="bias_NW.tex")

mse_NW <- rbind(round(p_seq, 2), round(quant_norm,3), mse_G_NW, mse_E_NW, mse_S_NW)
rownames(mse_NW) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(mse_NW[,abb_ind], caption="MSE when X is Normal(150, 50), C is Weibull(2, 100)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="mse_NW.tex")

bias_NN <- rbind(round(p_seq, 2), round(quant_norm,3), bias_G_NN, bias_E_NN, bias_S_NN)
rownames(bias_NN) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(bias_NN[,abb_ind], caption="Bias when X is Normal(150, 50), C is Normal(150, 50)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="bias_NN.tex")

mse_NN <- rbind(round(p_seq, 2), round(quant_norm,3), mse_G_NN, mse_E_NN, mse_S_NN)
rownames(mse_NN) <- c("Probability", "Quantile", "Gill", "Efron", "S*")
print(xtable(mse_NN[,abb_ind], caption="MSE when X is Normal(150, 50), C is Normal(150, 50)", digits=dmat, type="latex"), size = "\\fontsize{8}{10}\\selectfont", include.colnames = FALSE, table.placement = "H", file="mse_NN.tex")

