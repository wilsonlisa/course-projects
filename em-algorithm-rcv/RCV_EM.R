library(boot)
library(bootstrap)
library(tidyverse)
library(pander)
library(kableExtra)

# from page 5 of "FAIR0001 Toplines"
# https://fairvote.app.box.com/v/2020-FV-YouGov-Toplines
rcv_perc <- data.frame(c(.33, .18, .12, .15, .18), 
                       c(.08, .11, .23, .24, .26), 
                       c(.1, .18, .22, .25, .16), 
                       c(.2, .23, .16, .13, .22), 
                       c(.29, .27, .2, .13, .05))
colnames(rcv_perc) <- c("Biden", "Buttigieg", "Harris", "Sanders", "Warren")
# neither columns (candidates) nor rows (ranks) sum to 1
# --> people didn't use all 5 ranks
# colSums(rcv_perc)
# rowSums(rcv_perc)

# convert percentages to frequencies
rcv_freq <- round(1002*rcv_perc, 0)
# rcv_freq/1002

rcv_table <- rbind(rcv_freq, colSums(rcv_freq))
rownames(rcv_table) <- c("1", "2", "3", "4", "5", "Total")
kable(rcv_table, "latex", booktabs = TRUE, row.names = TRUE) %>%
    kable_styling(font_size = 10)

# shifted binomial pmf
pbinom.shift <- function(m, r, psi) {
    choose(m-1, r-1)*((1-psi)^(r-1))*psi^(m-r)
}

# EM algorithm function for frequency data
EM_rcv_freq <- function(current, data, m, n){
    r <- seq(1, m, 1)
    freq <- data
    pi <- current[1]
    psi <- current[2]
    
    # equivalent expression for tau_1 as in report
    tau <- (1 + (1-pi)/(m*pi*pbinom.shift(m, r, psi)))^(-1)
    
    R_n <- sum(r*freq*tau)/sum(freq*tau)
    
    pi_new <- (1/n)*sum(freq*tau)
    
    psi_new <- (m - R_n)/(m - 1)
    
    Out <- c(pi_new, psi_new)
}

# MUB pmf
pMUB <- function(pi, psi, m, r){
    pi*(choose(m-1, r-1)*((1-psi)^(r-1))*psi^(m-r)) + (1-pi)*(1/m)
} 

N <- 1002
tol <- 1E-10

# Joseph Biden
JB_freq <- rcv_freq[,1]

Rn_JB <- (seq(1, 5, 1)%*%JB_freq)/N
psi_JB <- (5-Rn_JB)/(5-1)

theta0 <- c(0.5, psi_JB) # starting values
theta_JB_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_JB_f[1,] <- theta0 # put starting value in theta vector

for(i in 2:1000){
    theta_JB_f[i,] = EM_rcv_freq(theta_JB_f[i-1, ], JB_freq, 5, N)
    if(abs(theta_JB_f[i,1]-theta_JB_f[i-1,1]) < tol & abs(theta_JB_f[i,2]-theta_JB_f[i-1,2]) < tol){
        est_JB <- theta_JB_f[i,]
        iter_JB <- i
        break
    }
}

# uncertainty share
unc_JB <- (1-est_JB[1])/5

# predicted probability of rank 1
r1_JB <- pMUB(est_JB[1], est_JB[2], 5, 1)

# good: probabilities of ranks sum to 1
# sum(pMUB(est_JB[1], est_JB[2], 5, seq(1,5,1)))

# Peter Buttigieg
PB_freq <- rcv_freq[,2]

Rn_PB <- (seq(1, 5, 1)%*%PB_freq)/N
psi_PB <- (5-Rn_PB)/(5-1)

theta0_PB <- c(0.5, psi_PB) # starting values
theta_PB_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_PB_f[1,] <- theta0_PB # put starting value in theta vector

for(i in 2:1000){
    theta_PB_f[i,] = EM_rcv_freq(theta_PB_f[i-1, ], PB_freq, 5, N)
    if(abs(theta_PB_f[i,1]-theta_PB_f[i-1,1]) < tol & abs(theta_PB_f[i,2]-theta_PB_f[i-1,2]) < tol){
        est_PB <- theta_PB_f[i,]
        iter_PB <- i
        break
    }
}

unc_PB <- (1-est_PB[1])/5

r1_PB <- pMUB(est_PB[1], est_PB[2], 5, 1)

# Kamala Harris
KH_freq <- rcv_freq[,3]

Rn_KH <- (seq(1, 5, 1)%*%KH_freq)/N
psi_KH <- (5-Rn_KH)/(5-1)

theta0_KH <- c(0.5, psi_KH) # starting values
theta_KH_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_KH_f[1,] <- theta0_KH # put starting value in theta vector

for(i in 2:1000){
    theta_KH_f[i,] = EM_rcv_freq(theta_KH_f[i-1, ], KH_freq, 5, N)
    if(abs(theta_KH_f[i,1]-theta_KH_f[i-1,1]) < tol & abs(theta_KH_f[i,2]-theta_KH_f[i-1,2]) < tol){
        est_KH <- theta_KH_f[i,]
        iter_KH <- i
        break
    }
}

unc_KH <- (1-est_KH[1])/5

r1_KH <- pMUB(est_KH[1], est_KH[2], 5, 1)

# Bernard Sanders
BS_freq <- rcv_freq[,4]

Rn_BS <- (seq(1, 5, 1)%*%BS_freq)/N
psi_BS <- (5-Rn_BS)/(5-1)

theta0_BS <- c(0.5, psi_BS) # starting values
theta_BS_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_BS_f[1,] <- theta0_BS # put starting value in theta vector

for(i in 2:1000){
    theta_BS_f[i,] = EM_rcv_freq(theta_BS_f[i-1, ], BS_freq, 5, N)
    if(abs(theta_BS_f[i,1]-theta_BS_f[i-1,1]) < tol & abs(theta_BS_f[i,2]-theta_BS_f[i-1,2]) < tol){
        est_BS <- theta_BS_f[i,]
        iter_BS <- i
        break
    }
}

unc_BS <- (1-est_BS[1])/5

# all ranks around 20% 
# --> low psi indicates not much preference for 1st
r1_BS <- pMUB(est_BS[1], est_BS[2], 5, 1)

# Elizabeth Warren
EW_freq <- rcv_freq[,5]

Rn_EW <- (seq(1, 5, 1)%*%EW_freq)/N
psi_EW <- (5-Rn_EW)/(5-1)

theta0_EW <- c(0.5, psi_EW) # starting values
theta_EW_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_EW_f[1,] <- theta0_EW # put starting value in theta vector

for(i in 2:1000){
    theta_EW_f[i,] = EM_rcv_freq(theta_EW_f[i-1, ], EW_freq, 5, N)
    if(abs(theta_EW_f[i,1]-theta_EW_f[i-1,1]) < tol & abs(theta_EW_f[i,2]-theta_EW_f[i-1,2]) < tol){
        est_EW <- theta_EW_f[i,]
        iter_EW <- i
        break
    }
}

unc_EW <- (1-est_EW[1])/5

r1_EW <- pMUB(est_EW[1], est_EW[2], 5, 1)

# rank sum is not bounded by 1
# r1_JB + r1_PB + r1_KH + r1_BS + r1_EW

### boot function
# EM algorithm function adapted for boot function
EM_rcv_bs2 <- function(data, vec, ranktot, n, indices){
    for(i in 2:1000){
        vec[i,] = EM_rcv_freq(vec[i-1,], data[indices], 5, N)
        if(abs(vec[i,1]-vec[i-1,1]) < tol & abs(vec[i,2]-vec[i-1,2]) < tol){
            est <- vec[i,]
            iter <- i
            break
        }
    }
    return(est)
}

# Joe Biden
theta0_JB <- c(0.5, psi_JB) # starting values
theta_JB_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_JB_f[1,] <- theta0 # put starting value in theta vector

set.seed(42)
JB_boot <- boot(JB_freq, EM_rcv_bs2, R=1000, vec=theta_JB_f, ranktot=5, n=N)

JB_basic_lpi <- boot.ci(JB_boot, index = 1)$basic[4]
JB_basic_upi <- boot.ci(JB_boot, index = 1)$basic[5]
JB_basic_lxi <- boot.ci(JB_boot, index = 2)$basic[4]
JB_basic_uxi <- boot.ci(JB_boot, index = 2)$basic[5]
# est_JB

# bootstrap(x=JB_freq, nboot=1000, theta=EM_rcv_bs, current=theta0_JB, ranktot=5, n=N)

# Pete Buttigieg
theta0_PB <- c(0.5, psi_PB) # starting values
theta_PB_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_PB_f[1,] <- theta0_PB # put starting value in theta vector

set.seed(42)
PB_boot <- boot(PB_freq, EM_rcv_bs2, R=1000, vec=theta_PB_f, ranktot=5, n=N)

PB_basic_lpi <- boot.ci(PB_boot, index = 1)$basic[4]
PB_basic_upi <- boot.ci(PB_boot, index = 1)$basic[5]
PB_basic_lxi <- boot.ci(PB_boot, index = 2)$basic[4]
PB_basic_uxi <- boot.ci(PB_boot, index = 2)$basic[5]
# est_PB

# Kamala Harris
theta0_KH <- c(0.5, psi_KH) # starting values
theta_KH_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_KH_f[1,] <- theta0_KH # put starting value in theta vector

set.seed(42)
KH_boot <- boot(KH_freq, EM_rcv_bs2, R=1000, vec=theta_KH_f, ranktot=5, n=N)

KH_basic_lpi <- boot.ci(KH_boot, index = 1)$basic[4]
KH_basic_upi <- boot.ci(KH_boot, index = 1)$basic[5]
KH_basic_lxi <- boot.ci(KH_boot, index = 2)$basic[4]
KH_basic_uxi <- boot.ci(KH_boot, index = 2)$basic[5]
# est_KH

# Bernie Sanders
theta0_BS <- c(0.5, psi_BS) # starting values
theta_BS_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_BS_f[1,] <- theta0_BS # put starting value in theta vector

set.seed(42)
BS_boot <- boot(BS_freq, EM_rcv_bs2, R=1000, vec=theta_BS_f, ranktot=5, n=N)

BS_basic_lpi <- boot.ci(BS_boot, index = 1)$basic[4]
BS_basic_upi <- boot.ci(BS_boot, index = 1)$basic[5]
BS_basic_lxi <- boot.ci(BS_boot, index = 2)$basic[4]
BS_basic_uxi <- boot.ci(BS_boot, index = 2)$basic[5]
# est_BS

# Elizabeth Warren
theta0_EW <- c(0.5, psi_EW) # starting values
theta_EW_f <- matrix(data = rep(0, 2000), nrow = 1000, ncol = 2) # store the values of theta
theta_EW_f[1,] <- theta0_EW # put starting value in theta vector

set.seed(42)
EW_boot <- boot(EW_freq, EM_rcv_bs2, R=1000, vec=theta_EW_f, ranktot=5, n=N)

EW_basic_lpi <- boot.ci(EW_boot, index = 1)$basic[4]
EW_basic_upi <- boot.ci(EW_boot, index = 1)$basic[5]
EW_basic_lxi <- boot.ci(EW_boot, index = 2)$basic[4]
EW_basic_uxi <- boot.ci(EW_boot, index = 2)$basic[5]
# est_EW

bootCI <- c(JB_basic_lpi, JB_basic_upi, JB_basic_lxi, JB_basic_uxi,
            PB_basic_lpi, PB_basic_upi, PB_basic_lxi, PB_basic_uxi,
            KH_basic_lpi, KH_basic_upi, KH_basic_lxi, KH_basic_uxi,
            BS_basic_lpi, BS_basic_upi, BS_basic_lxi, BS_basic_uxi,
            EW_basic_lpi, EW_basic_upi, EW_basic_lxi, EW_basic_uxi)