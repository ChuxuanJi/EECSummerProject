rm(list = ls())
library(MuMIn)
library(lmerTest)
library(dplyr)
library(EMAtools)

#2022 ATPSW Linear Mixed Model
#Loading files
ATPSW_Treat <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/3YearDataSummary/ATPSW_3Year_Treatment_C.txt',header = T)
ATPSW_Treat$Year <- as.factor(ATPSW_Treat$Year)

#Linear Mixed Model

##############2019
#Model for ATPSW_Treat#########
ATPSW_Treat_lmm <- lmer(ATP ~ Chemical * Temperature + Year + (1 | Pond), data=ATPSW_Treat)

summary(ATPSW_Treat_lmm)
anova(ATPSW_Treat_lmm)
plot(ATPSW_Treat_lmm)
hist(resid(ATPSW_Treat_lmm))


#Checking residual distribution and assumptions

res_ATPSW_Treat_lmm <- resid(ATPSW_Treat_lmm)

qqnorm(res_ATPSW_Treat_lmm)
qqline(res_ATPSW_Treat_lmm)

plot(density(res_ATPSW_Treat_lmm))

plot(fitted(ATPSW_Treat_lmm), res_ATPSW_Treat_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(ATPSW_Treat_lmm)

#Getting Cohen's d - effect size
lme.dscore(ATPSW_Treat_lmm, data = ATPSW_Treat, type="lme4")

#2022 MR Linear Mixed Model
#Loading files
MR <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/3YearDataSummary/MicroResp_chem_Summary.csv',sep = ',')
MR$Year <- as.factor(MR$Year)

#Linear Mixed Model

##############2019
#Model for MR#########
MR_lmm <- lmer(MicroResp_SW ~ Chemical * Temperature + Year + (1 | Ponds), data=MR)

summary(MR_lmm)
anova(MR_lmm)
plot(MR_lmm)
hist(resid(MR_lmm))


#Checking residual distribution and assumptions

res_MR_lmm <- resid(MR_lmm)

qqnorm(res_MR_lmm)
qqline(res_MR_lmm)

plot(density(res_MR_lmm))

plot(fitted(MR_lmm), res_MR_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(MR_lmm)

#Getting Cohen's d - effect size
lme.dscore(MR_lmm, data = MR, type="lme4")

#3Year leaf decomposition

#Set factor levels so control is selected as the reference level
LD <- read.delim('C:/Users/lenovo/Desktop/Leaf/LD201920212022.txt',header = T)
LD$Treatment = factor(LD$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
LD$Chemical = factor(LD$Chemical, levels=c('NONE','UR','RU','UR+RU'))
LD$Temperature <- factor(LD$Temperature, levels = c("0°C","4°C"))
LD$Year <- as.factor(LD$Year)

#Separate assays for individual modeling
fine<- LD %>%
  filter(Type == "Fine")

coarse<-LD%>%
  filter(Type == "Coarse")


#Linear Mixed Model

##############Fine bag
#Model for KD#########
KD_fine_lmm <- lmer(K ~ Chemical * Temperature + Year + (1 | Pond), data=fine)

summary(KD_fine_lmm)
anova(KD_fine_lmm)
plot(KD_fine_lmm)
hist(resid(KD_fine_lmm))

#Checking residual distribution and assumptions

res_KD_fine_lmm <- resid(KD_fine_lmm)

qqnorm(res_KD_fine_lmm)
qqline(res_KD_fine_lmm)

plot(density(res_KD_fine_lmm))

plot(fitted(KD_fine_lmm), res_KD_fine_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KD_fine_lmm)

#Getting Cohen's d - effect size
lme.dscore(KD_fine_lmm, data = fine, type="lme4")

#Separate model for temperature corrected KDD
KDD_fine_lmm <- lmer(KDD ~ Chemical * Temperature + Year + (1 | Pond), data=fine)

summary(KDD_fine_lmm)
plot(KDD_fine_lmm)
hist(resid(KDD_fine_lmm))

#Checking residual distribution and assumptions

res_KDD_fine_lmm <- resid(KDD_fine_lmm)

qqnorm(res_KDD_fine_lmm)
qqline(res_KDD_fine_lmm)

plot(density(res_KDD_fine_lmm))

plot(fitted(KDD_fine_lmm), res_KDD_fine_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KDD_fine_lmm)

#Getting Cohen's d - effect size
lme.dscore(KDD_fine_lmm, data = fine, type="lme4")

#############Coarse bag
#Model for KD##########
KD_coarse_lmm <- lmer(K ~ Chemical * Temperature + Year + (1 | Pond), data=coarse)

summary(KD_coarse_lmm)
plot(KD_coarse_lmm)
hist(resid(KD_coarse_lmm))

#Checking residual distribution and assumptions

res_KD_coarse_lmm <- resid(KD_coarse_lmm)

qqnorm(res_KD_coarse_lmm)
qqline(res_KD_coarse_lmm)

plot(density(res_KD_coarse_lmm))

plot(fitted(KD_coarse_lmm), res_KD_coarse_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KD_coarse_lmm)

#Getting Cohen's d - effect size
lme.dscore(KD_coarse_lmm, data = coarse, type="lme4")

#Separate model for temperature corrected KDD
KDD_coarse_lmm <- lmer(KDD ~ Chemical * Temperature + Year + (1 | Pond), data=coarse)

summary(KDD_coarse_lmm)
plot(KDD_coarse_lmm)
hist(resid(KDD_coarse_lmm))

#Checking residual distribution and assumptions

res_KDD_coarse_lmm <- resid(KDD_coarse_lmm)

qqnorm(res_KDD_coarse_lmm)
qqline(res_KDD_coarse_lmm)

plot(density(res_KDD_coarse_lmm))

plot(fitted(KDD_coarse_lmm), res_KDD_coarse_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KDD_coarse_lmm)

#Getting Cohen's d - effect size
lme.dscore(KDD_coarse_lmm, data = coarse, type="lme4")
