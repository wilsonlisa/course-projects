## data import
# initial selection of relevant variables in usable form
library(foreign)

setwd("ST 623/")
prc <- data.frame(read.spss("W17_May16/ATP W17.sav"))
str(prc)
head(prc)

colnames(prc)

# BIO33A_W17 through BIO46_W17 are on vaccines
# LOCALD_W17 through LOCALF_W17, ENV1_W17 through ENV34_W17 are environmental
# Relevant demo: F_CREGION_FINAL, F_AGECAT_FINAL, F_SEX_FINAL, F_EDUCCAT_FINAL (+ another),
# F_HISP_RECRUITMENT, F_RACECMB_RECRUITMENT, F_RACETHN_RECRUITMENT, F_RELIG_FINAL, F_BORN_FINAL, 
# F_RELCOM3CAT_FINAL, F_PARTY_FINAL, F_PARTYSUM_FINAL, F_INCOME_RECODE_FINAL
# F_IDEO_FINAL, F_SNSUSER_FINAL
prc_vacc <- prc[,c(16, 17, 25:27, 58, 60:62, 68:93, 286:314)]
head(prc_vacc)
summary(prc_vacc[,1])

# included in Github folder
write.csv(prc_vacc, "prc_vacc", row.names=FALSE)

library(MASS)
library(tidyverse)
library(sure)
library(pander)
library(gdata)
library(kableExtra)

## data cleaning
vacc <- read_csv("prc_vacc")
# get rid of confidence questions and how much have used alternative med
# so that na.omit doesn't nullify dataset
vacc_full <- data.frame(na.omit(vacc[,-c(1:2, 9)]))

for(i in 1:ncol(vacc_full)){
    vacc_full[,i] <- as.factor(vacc_full[,i])
}

vacc_full$BIO21_W17 <- relevel(vacc_full$BIO21_W17, "No, was not")
# vacc_full$F_PARTYSUM_FINAL <- relevel(vacc_full$F_PARTYSUM_FINAL, "Independent/No Lean")
vacc_full$F_IDEO_FINAL <- fct_collapse(vacc_full$F_IDEO_FINAL, 
                                       Conservative = c("Very conservative", "Conservative"), 
                                       Moderate = "Moderate", 
                                       Liberal = c("Very liberal", "Liberal"))
vacc_full$F_IDEO_FINAL <- factor(vacc_full$F_IDEO_FINAL, levels(vacc_full$F_IDEO_FINAL)[c(3,1,2)])
vacc_full$F_INT_FREQCOMB_FINAL <- fct_collapse(vacc_full$F_INT_FREQCOMB_FINAL, 
                                               Low = c("Never use the Internet", 
                                                       "Use the Internet less than once a month", 
                                                       "Use the Internet once a month", 
                                                       "Use the Internet once a week", 
                                                       "Use the Internet at least once a week but not every day"), 
                                               Medium = c("Use the Internet about once a day", 
                                                          "Use the Internet a few times a day"), 
                                               High = c("Use the Internet many times a day", 
                                                        "Use the Internet constantly"))
vacc_full$F_RACECMB_RECRUITMENT <- relevel(vacc_full$F_RACECMB_RECRUITMENT, "White")
vacc_full$F_BORN_FINAL <- factor(vacc_full$F_BORN_FINAL, levels(vacc_full$F_BORN_FINAL)[c(2,3)])
vacc_full$F_EDUCCAT_FINAL <- factor(vacc_full$F_EDUCCAT_FINAL, 
                                    levels(vacc_full$F_EDUCCAT_FINAL)[c(2,3,1)])
vacc_full$BIO33A_W17 <- factor(vacc_full$BIO33A_W17, levels(vacc_full$BIO33A_W17)[c(6,2,3,1,5,4)])
vacc_full$BIO33B_W17 <- factor(vacc_full$BIO33B_W17, levels(vacc_full$BIO33B_W17)[c(5,1,3,2,6,4)])
vacc_full$BIO36_W17 <- relevel(vacc_full$BIO36_W17, "Some")
vacc_full$BIO42_W17 <- relevel(vacc_full$BIO42_W17, "Somewhat closely")
vacc_full$BIO43_W17 <- relevel(vacc_full$BIO43_W17, "Somewhat good job")

use_A <- c("BIO21_W17", "BIO33A_W17", "BIO38_W17", "BIO40_W17", "BIO42_W17", "F_CREGION_FINAL", 
           "F_AGECAT_FINAL", "F_SEX_FINAL", "F_EDUCCAT_FINAL", "F_RACECMB_RECRUITMENT", "F_BORN_FINAL", 
           "F_PARTYSUM_FINAL", "F_INCOME_RECODE_FINAL", "F_IDEO_FINAL", "F_INT_FREQCOMB_FINAL", 
           "F_SNSUSER_FINAL")

vacc_A <- na.omit(vacc_full[, use_A])
refuse <- vacc_A %>%
    filter_all(any_vars(str_detect(., pattern = "Refused")))
vacc_A <- drop.levels(anti_join(vacc_A, refuse), reorder = FALSE)

## full model
modA <- polr(BIO33A_W17~., data = vacc_A)
ctableA <- coef(summary(modA))
pA <- round(pnorm(abs(ctableA[, "t value"]), lower.tail = FALSE) * 2, 4)
ctableA <- cbind(ctableA, "p value" = pA)
ctableA_abb <- ctableA[ctableA[,4] <= 0.05, ]
ctableA_abb <- ctableA_abb[-c(10:11),]
rownames(ctableA_abb) <- c("Vaccinated", "Understand: Not too well", "Understand: Very well", 
                           "Scientists: Almost all", "Northeast", "Asian", "Black", "Evangelical", 
                           "Conservative")

kable(round(ctableA_abb, 4)) %>%
    kable_styling(font_size = 10)

oddsA <- exp(ctableA_abb[,1])

## predictive model
stepAIC(polr(BIO33A_W17~., data=vacc_A))
stepAIC(polr(BIO33A_W17~., data=vacc_A, method = "probit"))
stepAIC(polr(BIO33A_W17~., data=vacc_A, method = "loglog"))

# model identified from stepAIC
modA_step <- polr(BIO33A_W17 ~ BIO21_W17 + BIO38_W17 + BIO40_W17 + 
                      BIO42_W17 + F_CREGION_FINAL + F_RACECMB_RECRUITMENT + F_BORN_FINAL + 
                      F_IDEO_FINAL + F_INT_FREQCOMB_FINAL, data = vacc_A)
# round(pnorm(abs(coef(summary(modA_step))[, "t value"]), lower.tail = FALSE) * 2, 3)

modAstep_log <- polr(BIO33A_W17 ~ BIO21_W17 + BIO38_W17 + BIO40_W17 + 
                         BIO42_W17 + F_CREGION_FINAL + F_RACECMB_RECRUITMENT + F_BORN_FINAL + 
                         F_IDEO_FINAL + F_INT_FREQCOMB_FINAL, data = vacc_A, method = "logistic")
modAstep_log.aic <- extractAIC(modAstep_log)[[2]]

modAstep_probit <- polr(BIO33A_W17 ~ BIO21_W17 + BIO38_W17 + BIO40_W17 + 
                            BIO42_W17 + F_CREGION_FINAL + F_RACECMB_RECRUITMENT + F_BORN_FINAL + 
                            F_IDEO_FINAL + F_INT_FREQCOMB_FINAL, data = vacc_A, method = "probit")
modAstep_probit.aic <- extractAIC(modAstep_probit)[[2]]

modAstep_loglog <- polr(BIO33A_W17 ~ BIO21_W17 + BIO38_W17 + BIO40_W17 + 
                            BIO42_W17 + F_CREGION_FINAL + F_RACECMB_RECRUITMENT + F_BORN_FINAL + 
                            F_IDEO_FINAL + F_INT_FREQCOMB_FINAL, data = vacc_A, method = "loglog")
modAstep_loglog.aic <- extractAIC(modAstep_loglog)[[2]]

ctableA2 <- coef(summary(modA_step))
pA2 <- round(pnorm(abs(ctableA2[, "t value"]), lower.tail = FALSE) * 2, 4)
ctableA2 <- cbind(ctableA2, "p value" = pA2)[-c(25:28),]
rownames(ctableA2) <- c("Don't remember", "Vaccinated", "Understand: Not at all well", 
                        "Understand: Not too well", "Understand: Very well", "Scientists: Almost all", 
                        "Scientists: Almost none", "Scientists: Fewer than half", 
                        "Scientists: More than half", 
                        "News: Not at all closely", "News: Not too closely", "News: Very closely", 
                        "Northeast", "South", "West", "Asian", 
                        "Black", "Mixed race", "Other race", 
                        "Evangelical", "Conservative", "Liberal", 
                        "Internet: Medium", "Internet: High")

kable(round(ctableA2, 4), longtable = TRUE) %>%
    kable_styling(font_size = 10)

# Using "mode" to predict
modA_pred <- predict(modAstep_log, 
                     newdata = data.frame(BIO21_W17 = "Yes, was vaccinated for the major childhood diseases", 
                                          BIO38_W17 = "Very well", BIO40_W17 = "Almost all", 
                                          BIO42_W17 = "Somewhat closely", F_CREGION_FINAL = "South", 
                                          F_AGECAT_FINAL = "50-64", F_SEX_FINAL = "Female", 
                                          F_EDUCCAT_FINAL = "College graduate+", F_RACECMB_RECRUITMENT = "White", 
                                          F_BORN_FINAL = "No, not born-again or evangelical Christian", 
                                          F_PARTYSUM_FINAL = "Rep/Rep Lean", 
                                          F_INCOME_RECODE_FINAL = "$75,000+", 
                                          F_IDEO_FINAL = "Moderate", F_INT_FREQCOMB_FINAL = "High", 
                                          F_SNSUSER_FINAL = "Social Media Users"),
                     type = "p")

pander(modA_pred)

# mode observation doesn't exist in dataset
mode_data <- vacc_A %>%
    filter(BIO21_W17 == "Yes, was vaccinated for the major childhood diseases", 
           BIO38_W17 == "Very well", BIO40_W17 == "Almost all", BIO42_W17 == "Somewhat closely", 
           F_CREGION_FINAL == "South", F_AGECAT_FINAL == "50-64", 
           F_SEX_FINAL == "Female", F_EDUCCAT_FINAL == "College graduate+", 
           F_RACECMB_RECRUITMENT == "White", 
           F_BORN_FINAL == "No, not born-again or evangelical Christian", 
           F_PARTYSUM_FINAL == "Rep/Rep Lean", 
           F_INCOME_RECODE_FINAL == "$75,000+", 
           F_IDEO_FINAL == "Moderate", 
           F_INT_FREQCOMB_FINAL == "High", 
           F_SNSUSER_FINAL == "Social Media Users")

## Residual checking
# Obtain surrogate residuals
set.seed(101) # for reproducibility
sresA <- resids(modAstep_log)
fitA <- unique(c(modAstep_log$fit[,1], modAstep_log$fit[,2], modAstep_log$fit[,3], 
                 modAstep_log$fit[,4], modAstep_log$fit[,5]))
fitA.samp <- sample(fitA, length(sresA))

# Residual-vs-fitted plot
ggplot() +
    geom_point(aes(x=fitA.samp, y=sresA), size = 2) +
    geom_abline(intercept = 0, slope = 0, color = "grey", size = 1) +
    labs(x = "Fitted values", y = "Surrogate residuals", title = "Residuals vs. Fitted") +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
          plot.title = element_text(size = 16))

# normal enough
ggplot() +
    geom_qq_line(aes(sample = sresA)) +
    geom_qq(aes(sample = sresA)) +
    labs(title="Q-Q plot", x="Theoretical", y="Sample (Deviance)") +
    theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
          plot.title = element_text(size = 16))

## collapse categories and look at difference in residuals
vacc_A3 <- vacc_A
vacc_A3$BIO33A_W17 <- fct_collapse(vacc_A3$BIO33A_W17, High = c("Very high", "High"), 
                                   Medium = "Medium", Low = c("Low", "Very low"))

modA_collapse <- polr(BIO33A_W17 ~ BIO21_W17 + BIO38_W17 + BIO40_W17 + 
                          BIO42_W17 + F_CREGION_FINAL + F_RACECMB_RECRUITMENT + F_BORN_FINAL + 
                          F_IDEO_FINAL + F_INT_FREQCOMB_FINAL, data = vacc_A3)

modA_c.aic <- extractAIC(modA_collapse)[[2]]

# Using "mode" to predict with collapsed categories
modA_c_pred <- predict(modA_collapse, 
                       newdata = data.frame(BIO21_W17 = "Yes, was vaccinated for the major childhood diseases", 
                                            BIO38_W17 = "Very well", BIO40_W17 = "Almost all", 
                                            BIO42_W17 = "Somewhat closely", F_CREGION_FINAL = "South", 
                                            F_AGECAT_FINAL = "50-64", F_SEX_FINAL = "Female", 
                                            F_EDUCCAT_FINAL = "College graduate+", F_RACECMB_RECRUITMENT = "White", 
                                            F_BORN_FINAL = "No, not born-again or evangelical Christian", 
                                            F_PARTYSUM_FINAL = "Rep/Rep Lean", 
                                            F_INCOME_RECODE_FINAL = "$75,000+", 
                                            F_IDEO_FINAL = "Moderate", F_INT_FREQCOMB_FINAL = "High", 
                                            F_SNSUSER_FINAL = "Social Media Users"),
                       type = "p")

pander(modA_c_pred)