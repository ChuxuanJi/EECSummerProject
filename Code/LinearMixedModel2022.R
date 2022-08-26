rm(list = ls())
library(MuMIn)
library(lmerTest)
library(dplyr)
library(EMAtools)

#2022 Decomposition Linear Mixed Model
#Loading files
ATPSW2022 <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPSedimentWash2022.txt',header = T)

#Separate assays for individual modeling
ATPSWA<- ATPSW2022 %>%
  filter(Ammonia == "Ammonia")

ATPSWNA<- ATPSW2022 %>%
  filter(Ammonia == "No Ammonia")

#Linear Mixed Model

##############No Ammonia
#Model for ATPSW#########
ATPSWNA_lmm <- lmer(ATP ~ Chemical * Temp + Dosing + (1 | Pond), data=ATPSWNA)

summary(ATPSWNA_lmm)
anova(ATPSWNA_lmm)
plot(ATPSWNA_lmm)
hist(resid(ATPSWNA_lmm))

#Checking residual distribution and assumptions

res_ATPSWNA_lmm <- resid(ATPSWNA_lmm)

qqnorm(res_ATPSWNA_lmm)
qqline(res_ATPSWNA_lmm)

plot(density(res_ATPSWNA_lmm))

plot(fitted(ATPSWNA_lmm), res_ATPSWNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(ATPSWNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(ATPSWNA_lmm, data = ATPSWNA, type="lme4")

#Linear Mixed Model

##############Ammonia
#Model for ATPSW#########
ATPSWA_lmm <- lmer(ATP ~ Chemical * Temp + Dosing + (1 | Pond), data=ATPSWA)

summary(ATPSWA_lmm)
anova(ATPSWA_lmm)
plot(ATPSWA_lmm)
hist(resid(ATPSWA_lmm))

#Checking residual distribution and assumptions

res_ATPSWA_lmm <- resid(ATPSWA_lmm)

qqnorm(res_ATPSWA_lmm)
qqline(res_ATPSWA_lmm)

plot(density(res_ATPSWA_lmm))

plot(fitted(ATPSWA_lmm), res_ATPSWA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(ATPSWA_lmm)

#Getting Cohen's d - effect size
lme.dscore(ATPSWA_lmm, data = ATPSWA, type="lme4")

##################################################################################
#Loading files
ATPS2022 <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPSediment2022.txt',header = T)

#Separate assays for individual modeling
ATPSA<- ATPS2022 %>%
  filter(Ammonia == "Ammonia")

ATPSNA<- ATPS2022 %>%
  filter(Ammonia == "No Ammonia")

#Linear Mixed Model

##############No Ammonia
#Model for ATPS#########
ATPSNA_lmm <- lmer(ATP ~ Chemical * Temp + Dosing + (1 | Pond), data=ATPSNA)

summary(ATPSNA_lmm)
anova(ATPSNA_lmm)
plot(ATPSNA_lmm)
hist(resid(ATPSNA_lmm))

#Checking residual distribution and assumptions

res_ATPSNA_lmm <- resid(ATPSNA_lmm)

qqnorm(res_ATPSNA_lmm)
qqline(res_ATPSNA_lmm)

plot(density(res_ATPSNA_lmm))

plot(fitted(ATPSNA_lmm), res_ATPSNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(ATPSNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(ATPSNA_lmm, data = ATPSNA, type="lme4")

#Linear Mixed Model

##############Ammonia
#Model for ATPS#########
ATPSA_lmm <- lmer(ATP ~ Chemical * Temp + Dosing + (1 | Pond), data=ATPSA)

summary(ATPSA_lmm)
anova(ATPSA_lmm)
plot(ATPSA_lmm)
hist(resid(ATPSA_lmm))

#Checking residual distribution and assumptions

res_ATPSA_lmm <- resid(ATPSA_lmm)

qqnorm(res_ATPSA_lmm)
qqline(res_ATPSA_lmm)

plot(density(res_ATPSA_lmm))

plot(fitted(ATPSA_lmm), res_ATPSA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(ATPSA_lmm)

#Getting Cohen's d - effect size
lme.dscore(ATPSA_lmm, data = ATPSA, type="lme4")

##################################################################################
ATPW2022 <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPWater2022.txt',header = T)

#Separate assays for individual modeling
ATPWA<- ATPW2022 %>%
  filter(Ammonia == "Ammonia")

ATPWNA<- ATPW2022 %>%
  filter(Ammonia == "No Ammonia")

#Linear Mixed Model

##############No Ammonia
#Model for ATPW#########
ATPWNA_lmm <- lmer(ATP ~ Chemical * Temp + Dosing + (1 | Pond), data=ATPWNA)

summary(ATPWNA_lmm)
anova(ATPWNA_lmm)
plot(ATPWNA_lmm)
hist(resid(ATPWNA_lmm))

#Checking residual distribution and assumptions

res_ATPWNA_lmm <- resid(ATPWNA_lmm)

qqnorm(res_ATPWNA_lmm)
qqline(res_ATPWNA_lmm)

plot(density(res_ATPWNA_lmm))

plot(fitted(ATPWNA_lmm), res_ATPWNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(ATPWNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(ATPWNA_lmm, data = ATPWNA, type="lme4")

#Linear Mixed Model

##############Ammonia
#Model for ATPW#########
ATPWA_lmm <- lmer(ATP ~ Chemical * Temp + Dosing + (1 | Pond), data=ATPWA)

summary(ATPWA_lmm)
anova(ATPWA_lmm)
plot(ATPWA_lmm)
hist(resid(ATPWA_lmm))

#Checking residual distribution and assumptions

res_ATPWA_lmm <- resid(ATPWA_lmm)

qqnorm(res_ATPWA_lmm)
qqline(res_ATPWA_lmm)

plot(density(res_ATPWA_lmm))

plot(fitted(ATPWA_lmm), res_ATPWA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(ATPWA_lmm)

#Getting Cohen's d - effect size
lme.dscore(ATPWA_lmm, data = ATPWA, type="lme4")

##########################################################################################
#Loading files
ATPWPC2022 <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPpercell2022.txt',header = T)

#Separate assays for individual modeling
ATPWPCA<- ATPWPC2022 %>%
  filter(Ammonia == "Ammonia")

ATPWPCNA<- ATPWPC2022 %>%
  filter(Ammonia == "No Ammonia")

#Linear Mixed Model

##############No Ammonia
#Model for ATPWPC#########
ATPWPCNA_lmm <- lmer(ATP ~ Chemical * Temp + Dosing + (1 | Pond), data=ATPWPCNA)

summary(ATPWPCNA_lmm)
anova(ATPWPCNA_lmm)
plot(ATPWPCNA_lmm)
hist(resid(ATPWPCNA_lmm))

#Checking residual distribution and assumptions

res_ATPWPCNA_lmm <- resid(ATPWPCNA_lmm)

qqnorm(res_ATPWPCNA_lmm)
qqline(res_ATPWPCNA_lmm)

plot(density(res_ATPWPCNA_lmm))

plot(fitted(ATPWPCNA_lmm), res_ATPWPCNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(ATPWPCNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(ATPWPCNA_lmm, data = ATPWPCNA, type="lme4")

#Linear Mixed Model

##############Ammonia
#Model for ATPWPC#########
ATPWPCA_lmm <- lmer(ATP ~ Chemical * Temp + Dosing + (1 | Pond), data=ATPWPCA)

summary(ATPWPCA_lmm)
anova(ATPWPCA_lmm)
plot(ATPWPCA_lmm)
hist(resid(ATPWPCA_lmm))

#Checking residual distribution and assumptions

res_ATPWPCA_lmm <- resid(ATPWPCA_lmm)

qqnorm(res_ATPWPCA_lmm)
qqline(res_ATPWPCA_lmm)

plot(density(res_ATPWPCA_lmm))

plot(fitted(ATPWPCA_lmm), res_ATPWPCA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(ATPWPCA_lmm)

#Getting Cohen's d - effect size
lme.dscore(ATPWPCA_lmm, data = ATPWPCA, type="lme4")

#2022 MR Linear Mixed Model
#Loading files
MR2022 <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/MicroResp/MicroResp2022.txt',header = T)

#Separate assays for individual modeling
MRA<- MR2022 %>%
  filter(Ammonia == "Ammonia")

MRNA<- MR2022 %>%
  filter(Ammonia == "No Ammonia")

#Linear Mixed Model

##############No Ammonia
#Model for MR#########
MRNA_lmm <- lmer(MicroResp ~ Chemical * Temp + Dosing + (1 | Ponds), data=MRNA)

summary(MRNA_lmm)
anova(MRNA_lmm)
plot(MRNA_lmm)
hist(resid(MRNA_lmm))

#Checking residual distribution and assumptions

res_MRNA_lmm <- resid(MRNA_lmm)

qqnorm(res_MRNA_lmm)
qqline(res_MRNA_lmm)

plot(density(res_MRNA_lmm))

plot(fitted(MRNA_lmm), res_MRNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(MRNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(MRNA_lmm, data = MRNA, type="lme4")

#Linear Mixed Model

##############Ammonia
#Model for MR#########
MRA_lmm <- lmer(MicroResp ~ Chemical * Temp + Dosing + (1 | Ponds), data=MRA)

summary(MRA_lmm)
anova(MRA_lmm)
plot(MRA_lmm)
hist(resid(MRA_lmm))

#Checking residual distribution and assumptions

res_MRA_lmm <- resid(MRA_lmm)

qqnorm(res_MRA_lmm)
qqline(res_MRA_lmm)

plot(density(res_MRA_lmm))

plot(fitted(MRA_lmm), res_MRA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(MRA_lmm)

#Getting Cohen's d - effect size
lme.dscore(MRA_lmm, data = MRA, type="lme4")

#2022 BOD Linear Mixed Model
#Loading files
BOD2022 <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/BOD/2022BOD.txt',header = T)

#Separate assays for individual modeling
BODA<- BOD2022 %>%
  filter(Ammonia == "Ammonia")

BODNA<- BOD2022 %>%
  filter(Ammonia == "No Ammonia")

#Linear Mixed Model

##############No Ammonia
#Model for BOD#########
BODNA_lmm <- lmer(BOD ~ Chemical * Temp + Dosing + (1 | Pond), data=BODNA)

summary(BODNA_lmm)
anova(BODNA_lmm)
plot(BODNA_lmm)
hist(resid(BODNA_lmm))

#Checking residual distribution and assumptions

res_BODNA_lmm <- resid(BODNA_lmm)

qqnorm(res_BODNA_lmm)
qqline(res_BODNA_lmm)

plot(density(res_BODNA_lmm))

plot(fitted(BODNA_lmm), res_BODNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(BODNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(BODNA_lmm, data = BODNA, type="lme4")

#Linear Mixed Model

##############Ammonia
#Model for BOD#########
BODA_lmm <- lmer(BOD ~ Chemical * Temp + Dosing + (1 | Pond), data=BODA)

summary(BODA_lmm)
anova(BODA_lmm)
plot(BODA_lmm)
hist(resid(BODA_lmm))

#Checking residual distribution and assumptions

res_BODA_lmm <- resid(BODA_lmm)

qqnorm(res_BODA_lmm)
qqline(res_BODA_lmm)

plot(density(res_BODA_lmm))

plot(fitted(BODA_lmm), res_BODA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(BODA_lmm)

#Getting Cohen's d - effect size
lme.dscore(BODA_lmm, data = BODA, type="lme4")


#2022 Decomposition Linear Mixed Model
#Loading files
D2022 <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/Decomposition/Decomposition2022_forLMM.txt',header = T)

#Separate assays for individual modeling

redA<- D2022 %>%
  filter(Type == "RedA")

redNA<- D2022 %>%
  filter(Type == "RedNA")

greenA<- D2022 %>%
  filter(Type == "GreenA")

greenNA<- D2022 %>%
  filter(Type == "GreenNA")


#Linear Mixed Model

##############Green bag
#Model for KD#########
KD_greenNA_lmm <- lmer(K ~ Chemical * Temp + Dosing + (1 | Pond), data=greenNA)

summary(KD_greenNA_lmm)
anova(KD_greenNA_lmm)
plot(KD_greenNA_lmm)
hist(resid(KD_greenNA_lmm))

#Checking residual distribution and assumptions

res_KD_greenNA_lmm <- resid(KD_greenNA_lmm)

qqnorm(res_KD_greenNA_lmm)
qqline(res_KD_greenNA_lmm)

plot(density(res_KD_greenNA_lmm))

plot(fitted(KD_greenNA_lmm), res_KD_greenNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KD_greenNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(KD_greenNA_lmm, data = greenNA, type="lme4")

#Model for KDD#########
KDD_greenNA_lmm <- lmer(KDD ~ Chemical * Temp + Dosing + (1 | Pond), data=greenNA)

summary(KDD_greenNA_lmm)
anova(KDD_greenNA_lmm)
plot(KDD_greenNA_lmm)
hist(resid(KDD_greenNA_lmm))

#Checking residual distribution and assumptions

res_KDD_greenNA_lmm <- resid(KDD_greenNA_lmm)

qqnorm(res_KDD_greenNA_lmm)
qqline(res_KDD_greenNA_lmm)

plot(density(res_KDD_greenNA_lmm))

plot(fitted(KDD_greenNA_lmm), res_KDD_greenNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KDD_greenNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(KDD_greenNA_lmm, data = greenNA, type="lme4")


#Model for KD#########
KD_greenA_lmm <- lmer(K ~ Chemical * Temp + Dosing + (1 | Pond), data=greenA)

summary(KD_greenA_lmm)
anova(KD_greenA_lmm)
plot(KD_greenA_lmm)
hist(resid(KD_greenA_lmm))

#Checking residual distribution and assumptions

res_KD_greenA_lmm <- resid(KD_greenA_lmm)

qqnorm(res_KD_greenA_lmm)
qqline(res_KD_greenA_lmm)

plot(density(res_KD_greenA_lmm))

plot(fitted(KD_greenA_lmm), res_KD_greenA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KD_greenA_lmm)

#Getting Cohen's d - effect size
lme.dscore(KD_greenA_lmm, data = greenA, type="lme4")


#Model for KDD#########
KDD_greenA_lmm <- lmer(KDD ~ Chemical * Temp + Dosing + (1 | Pond), data=greenA)

summary(KDD_greenA_lmm)
anova(KDD_greenA_lmm)
plot(KDD_greenA_lmm)
hist(resid(KDD_greenA_lmm))

#Checking residual distribution and assumptions

res_KDD_greenA_lmm <- resid(KDD_greenA_lmm)

qqnorm(res_KDD_greenA_lmm)
qqline(res_KDD_greenA_lmm)

plot(density(res_KDD_greenA_lmm))

plot(fitted(KDD_greenA_lmm), res_KDD_greenA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KDD_greenA_lmm)

#Getting Cohen's d - effect size
lme.dscore(KDD_greenA_lmm, data = greenA, type="lme4")


##############Red bag
#Model for KD#########
KD_redNA_lmm <- lmer(K ~ Chemical * Temp + Dosing + (1 | Pond), data=redNA)

summary(KD_redNA_lmm)
anova(KD_redNA_lmm)
plot(KD_redNA_lmm)
hist(resid(KD_redNA_lmm))

#Checking residual distribution and assumptions

res_KD_redNA_lmm <- resid(KD_redNA_lmm)

qqnorm(res_KD_redNA_lmm)
qqline(res_KD_redNA_lmm)

plot(density(res_KD_redNA_lmm))

plot(fitted(KD_redNA_lmm), res_KD_redNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KD_redNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(KD_redNA_lmm, data = redNA, type="lme4")

#Model for KDD#########
KDD_redNA_lmm <- lmer(KDD ~ Chemical * Temp + Dosing + (1 | Pond), data=redNA)

summary(KDD_redNA_lmm)
anova(KDD_redNA_lmm)
plot(KDD_redNA_lmm)
hist(resid(KDD_redNA_lmm))

#Checking residual distribution and assumptions

res_KDD_redNA_lmm <- resid(KDD_redNA_lmm)

qqnorm(res_KDD_redNA_lmm)
qqline(res_KDD_redNA_lmm)

plot(density(res_KDD_redNA_lmm))

plot(fitted(KDD_redNA_lmm), res_KDD_redNA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KDD_redNA_lmm)

#Getting Cohen's d - effect size
lme.dscore(KDD_redNA_lmm, data = redNA, type="lme4")


#Model for KD#########
KD_redA_lmm <- lmer(K ~ Chemical * Temp + Dosing + (1 | Pond), data=redA)

summary(KD_redA_lmm)
anova(KD_redA_lmm)
plot(KD_redA_lmm)
hist(resid(KD_redA_lmm))

#Checking residual distribution and assumptions

res_KD_redA_lmm <- resid(KD_redA_lmm)

qqnorm(res_KD_redA_lmm)
qqline(res_KD_redA_lmm)

plot(density(res_KD_redA_lmm))

plot(fitted(KD_redA_lmm), res_KD_redA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KD_redA_lmm)

#Getting Cohen's d - effect size
lme.dscore(KD_redA_lmm, data = redA, type="lme4")


#Model for KDD#########
KDD_redA_lmm <- lmer(KDD ~ Chemical * Temp + Dosing + (1 | Pond), data=redA)

summary(KDD_redA_lmm)
anova(KDD_redA_lmm)
plot(KDD_redA_lmm)
hist(resid(KDD_redA_lmm))

#Checking residual distribution and assumptions

res_KDD_redA_lmm <- resid(KDD_redA_lmm)

qqnorm(res_KDD_redA_lmm)
qqline(res_KDD_redA_lmm)

plot(density(res_KDD_redA_lmm))

plot(fitted(KDD_redA_lmm), res_KDD_redA_lmm)
abline(0,0)

#testing variance explained
r.squaredGLMM(KDD_redA_lmm)

#Getting Cohen's d - effect size
lme.dscore(KDD_redA_lmm, data = redA, type="lme4")

