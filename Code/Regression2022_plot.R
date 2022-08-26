rm(list = ls())
#This script is for 2022 ATP regression plots

#Loading packages
library(ggplot2)
library(ggpubr)

#Loading files
ATPSedimentWash2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPSedimentWash2022_Temperature.txt',header = T)
ATPSediment2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPSediment2022_Temperature.txt',header = T)
ATPWater2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPWater2022_Temperature.txt',header = T)
ATPWaterpc2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPpercell2022_Temperature.txt',header = T)

ATPSedimentWash2022_Temp$Dosing = factor(ATPSedimentWash2022_Temp$Dosing, levels=c('Pre','Post'))
ATPSediment2022_Temp$Dosing = factor(ATPSediment2022_Temp$Dosing, levels=c('Pre','Post'))
ATPWater2022_Temp$Dosing = factor(ATPWater2022_Temp$Dosing, levels=c('Pre','Post'))
ATPWaterpc2022_Temp$Dosing = factor(ATPWaterpc2022_Temp$Dosing, levels=c('Pre','Post'))

ATPSedimentWash2022_Temp$Ammonia = factor(ATPSedimentWash2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))
ATPSediment2022_Temp$Ammonia = factor(ATPSediment2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))
ATPWater2022_Temp$Ammonia = factor(ATPWater2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))
ATPWaterpc2022_Temp$Ammonia = factor(ATPWaterpc2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Seperate

ATPSWNA<- ATPSedimentWash2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "No Ammonia")

ATPSWA<- ATPSedimentWash2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "Ammonia")

ATPSNA<- ATPSediment2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "No Ammonia")

ATPSA<- ATPSediment2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "Ammonia")

ATPWNA<- ATPWater2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "No Ammonia")

ATPWA<- ATPWater2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "Ammonia")

ATPWpcNA<- ATPWaterpc2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "No Ammonia")

ATPWpcA<- ATPWaterpc2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "Ammonia")

#Plot
ATPSWNA_temp_reg <- 
  ggscatter(ATPSWNA, 
            x = 'Temp', y = 'ATP', 
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/g)" ,
            title = "Sediment Wash ATP (No Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
ATPSWNA_temp_reg

ATPSWA_temp_reg <- 
  ggscatter(ATPSWA, 
            x = 'Temp', y = 'ATP', 
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/g)" ,
            title = "Sediment Wash ATP (Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
ATPSWA_temp_reg

ATPSNA_temp_reg <- 
  ggscatter(ATPSNA, 
            x = 'Temp', y = 'ATP', 
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/g)" ,
            title = "Sediment ATP (No Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
ATPSNA_temp_reg

ATPSA_temp_reg <- 
  ggscatter(ATPSA, 
            x = 'Temp', y = 'ATP', 
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/g)" ,
            title = "Sediment ATP (Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
ATPSA_temp_reg

ATPWNA_temp_reg <- 
  ggscatter(ATPWNA, 
            x = 'Temp', y = 'ATP', 
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/g)" ,
            title = "Water ATP (No Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
ATPWNA_temp_reg

ATPWA_temp_reg <- 
  ggscatter(ATPWA, 
            x = 'Temp', y = 'ATP', 
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/g)" ,
            title = "Water ATP (Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
ATPWA_temp_reg

ATPWpcNA_temp_reg <- 
  ggscatter(ATPWpcNA, 
            x = 'Temp', y = 'ATP', 
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/ g·cell)" ,
            title = "Water ATP (No Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
ATPWpcNA_temp_reg

ATPWpcA_temp_reg <- 
  ggscatter(ATPWpcA, 
            x = 'Temp', y = 'ATP', 
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/ g·cell)" ,
            title = "Water ATP (Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
ATPWpcA_temp_reg

ggarrange(ATPSWNA_temp_reg, ATPSWA_temp_reg,
          ncol = 2, nrow = 1,
          labels = c("A","B"),
          font.label = list(size = 14))

ggarrange(ATPSNA_temp_reg, ATPSA_temp_reg,
          ATPWNA_temp_reg, ATPWA_temp_reg,
          ATPWpcNA_temp_reg, ATPWpcA_temp_reg,
          ncol = 2, nrow = 3,
          labels = c("A","B","C","D","E","F"),
          font.label = list(size = 14))


ATPSW_temp_reg <- 
  ggscatter(ATPSedimentWash2022_Temp, 
            x = 'Temp', y = 'ATP', 
            facet.by = "Ammonia",
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/g)" ,
            title = "Sediment Wash ATP (No Ammonia)",
            color = "Dosing", # Points color, shape and size
            palette = c("#000000","#7DB9DE"),
            add = "reg.line",  # Add regression line
            add.params = list(fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            #cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw()
  )+
  stat_cor(aes(color = Dosing), label.x = 3)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(legend.position="bottom")
ATPSW_temp_reg

ATPS_temp_reg <- 
  ggscatter(ATPSediment2022_Temp, 
            x = 'Temp', y = 'ATP', 
            facet.by = "Ammonia",
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/g)" ,
            title = "2022 Sediment ATP (Warming)",
            color = "Dosing", # Points color, shape and size
            palette = c("#000000","#7DB9DE"),
            add = "reg.line",  # Add regression line
            add.params = list(fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            #cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw()
  )+
  stat_cor(aes(color = Dosing), label.x = 3)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(legend.position="none")
ATPS_temp_reg


ATPW_temp_reg <- 
  ggscatter(ATPWater2022_Temp, 
            x = 'Temp', y = 'ATP', 
            facet.by = "Ammonia",
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/g)" ,
            title = "2022 Water ATP (Warming)",
            color = "Dosing", # Points color, shape and size
            palette = c("#000000","#7DB9DE"),
            add = "reg.line",  # Add regression line
            add.params = list(fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            #cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw()
  )+
  stat_cor(aes(color = Dosing), label.x = 3)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(legend.position="none")
ATPW_temp_reg

ATPWpc_temp_reg <- 
  ggscatter(ATPWaterpc2022_Temp, 
            x = 'Temp', y = 'ATP', 
            facet.by = "Ammonia",
            xlab = "Temperature above ambient (°C)", ylab = "ATP (nmol/ g·cell)" ,
            title = "2022 Water ATP (Warming)",
            color = "Dosing", # Points color, shape and size
            palette = c("#000000","#7DB9DE"),
            add = "reg.line",  # Add regression line
            add.params = list(fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            #cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw()
  )+
  stat_cor(aes(color = Dosing), label.x = 3)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(legend.position="bottom")
ATPWpc_temp_reg

ggarrange(ATPS_temp_reg, ATPW_temp_reg,ATPWpc_temp_reg,
          ncol = 1, nrow = 3,
          labels = c("A","B","C"),
          font.label = list(size = 14),
          legend = "bottom",
          common.legend = T)


rm(list = ls())
#This script is for 2022 MicroResp regression plots

#Loading files
MicroResp2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/MicroResp/MicroResp2022_Temperature.txt',header = T)
MicroResp2022_Temp$Dosing = factor(MicroResp2022_Temp$Dosing, levels=c('Pre','Post'))
MicroResp2022_Temp$Ammonia = factor(MicroResp2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Seperate

MRNA<- MicroResp2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "No Ammonia")

MRA<- MicroResp2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "Ammonia")

#Plot
MRNA_temp_reg <- 
  ggscatter(MRNA, 
            x = 'Temp', y = 'MicroResp', 
            xlab = "Temperature above ambient (°C)", ylab = "Respiration (mg)" ,
            title = "MicroResp (No Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
MRNA_temp_reg

MRA_temp_reg <- 
  ggscatter(MRA, 
            x = 'Temp', y = 'MicroResp', 
            xlab = "Temperature above ambient (°C)", ylab = "Respiration (mg)" ,
            title = "MicroResp (Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
MRA_temp_reg

ggarrange(MRNA_temp_reg, MRA_temp_reg,
          ncol = 2, nrow = 1,
          labels = c("A","B"),
          font.label = list(size = 14))

MR_temp_reg <- 
  ggscatter(MicroResp2022_Temp, 
            x = 'Temp', y = 'MicroResp', 
            facet.by = "Ammonia",
            xlab = "Temperature above ambient (°C)", ylab = "Respiration (mg)" ,
            title = "2022 MicroResp (Warming)",
            color = "Dosing", # Points color, shape and size
            palette = c("#000000","#7DB9DE"),
            add = "reg.line",  # Add regression line
            add.params = list(fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            #cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw()
  )+
  stat_cor(aes(color = Dosing), label.x = 3)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(legend.position="bottom")
MR_temp_reg

#This script is for 2022 BOD regression plots

#Loading files
BOD2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/BOD/2022BOD_Temperature.txt',header = T)
BOD2022_Temp$Dosing = factor(BOD2022_Temp$Dosing, levels=c('Pre','Post'))
BOD2022_Temp$Ammonia = factor(BOD2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Seperate

BODNA<- BOD2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "No Ammonia")

BODA<- BOD2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Ammonia == "Ammonia")

#Plot
BODNA_temp_reg <- 
  ggscatter(BODNA, 
            x = 'Temp', y = 'BOD', 
            xlab = "Temperature above ambient (°C)", ylab = "BOD (mg/L)" ,
            title = "BOD (No Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
BODNA_temp_reg

BODA_temp_reg <- 
  ggscatter(BODA, 
            x = 'Temp', y = 'BOD', 
            xlab = "Temperature above ambient (°C)", ylab = "BOD (mg/L)" ,
            title = "BOD (Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
BODA_temp_reg

ggarrange(BODNA_temp_reg, BODA_temp_reg,
          ncol = 2, nrow = 1,
          labels = c("A","B"),
          font.label = list(size = 14))

BOD_temp_reg <- 
  ggscatter(BOD2022_Temp, 
            x = 'Temp', y = 'BOD', 
            facet.by = "Ammonia",
            xlab = "Temperature above ambient (°C)", ylab = "BOD (mg/L)" ,
            title = "2022 BOD (Warming)",
            color = "Dosing", # Points color, shape and size
            palette = c("#000000","#7DB9DE"),
            add = "reg.line",  # Add regression line
            add.params = list(fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            #cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw()
  )+
  stat_cor(aes(color = Dosing), label.x = 3)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(legend.position="bottom")
BOD_temp_reg

#This script is for 2022 Tea bag regression plots

#Loading files
Tea2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/Decomposition/Tea/Tea2022_Temperature.txt',header = T)
Tea2022_Temp$Dosing = factor(Tea2022_Temp$Dosing, levels=c('Pre','Post'))
Tea2022_Temp$Ammonia = factor(Tea2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Seperate

greenNA<- Tea2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Tea == "Green") %>% 
  filter(Ammonia == "No Ammonia")

greenA<- Tea2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Tea == "Green") %>% 
  filter(Ammonia == "Ammonia")

redNA<- Tea2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Tea == "Rooibos") %>% 
  filter(Ammonia == "No Ammonia")

redA<- Tea2022_Temp %>%
  filter(Dosing == "Post") %>%
  filter(Tea == "Rooibos") %>% 
  filter(Ammonia == "Ammonia")


greenNA_temp_reg <- 
  ggscatter(greenNA, 
            x = 'Temp', y = 'K', 
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[D]*")"),
            title = "Green Tea (No Ammonia)",
            color = "black", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "#FFC425", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
greenNA_temp_reg

greenNAkdd_temp_reg <- 
  ggscatter(greenNA, 
            x = 'Temp', y = 'KDD', 
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[DD]*")"),
            title = "Green Tea (No Ammonia, TC)",
            color = "black", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "#FFC425", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
greenNAkdd_temp_reg

greenA_temp_reg <- 
  ggscatter(greenA, 
            x = 'Temp', y = 'K', 
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[D]*")"),
            title = "Green Tea (Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
greenA_temp_reg

greenAkdd_temp_reg <- 
  ggscatter(greenA, 
            x = 'Temp', y = 'KDD', 
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[DD]*")"),
            title = "Green Tea (Ammonia, TC)",
            color = "black", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "#FFC425", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
greenAkdd_temp_reg

redNA_temp_reg <- 
  ggscatter(redNA, 
            x = 'Temp', y = 'K', 
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[D]*")"),
            title = "Rooibos Tea (No Ammonia)",
            color = "black", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "#FFC425", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
redNA_temp_reg

redNAkdd_temp_reg <- 
  ggscatter(redNA, 
            x = 'Temp', y = 'KDD', 
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[DD]*")"),
            title = "Rooibos Tea (No Ammonia, TC)",
            color = "black", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "#FFC425", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
redNAkdd_temp_reg

redA_temp_reg <- 
  ggscatter(redA, 
            x = 'Temp', y = 'K', 
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[D]*")"),
            title = "Rooibos Tea (Ammonia)",
            color = "gray", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "lightgray", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
redA_temp_reg

redAkdd_temp_reg <- 
  ggscatter(redA, 
            x = 'Temp', y = 'KDD', 
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[DD]*")"),
            title = "Rooibos Tea (Ammonia, TC)",
            color = "black", # Points color, shape and size
            add = "reg.line",  # Add regression line
            add.params = list(color = "#FFC425", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw(),
  )
redAkdd_temp_reg

ggarrange(greenNA_temp_reg, greenNAkdd_temp_reg,
          redNA_temp_reg, redNAkdd_temp_reg,
          greenA_temp_reg, greenAkdd_temp_reg,
          redA_temp_reg, redAkdd_temp_reg,
          ncol = 2, nrow = 4,
          labels = c("A","B","C","D","E","F","G","H"),
          font.label = list(size = 14))


Tea_temp_reg <- 
  ggscatter(Tea2022_Temp, 
            x = 'Temp', y = 'K', 
            facet.by = c("Tea","Ammonia"), scales = "free_y",
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[D]*")") ,
            title = "2022 Tea Bag Decomposition (Warming)",
            color = "Dosing", # Points color, shape and size
            palette = c("#000000","#7DB9DE"),
            add = "reg.line",  # Add regression line
            add.params = list(fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            #cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw()
  )+
  stat_cor(aes(color = Dosing), label.x = 2.5)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(strip.text.y = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(legend.position="bottom")
Tea_temp_reg

Tea_tempTC_reg <- 
  ggscatter(Tea2022_Temp, 
            x = 'Temp', y = 'KDD', 
            facet.by = c("Tea","Ammonia"), scales = "free_y",
            xlab = "Temperature above ambient (°C)", ylab = expression("Decomposition rate (K"[DD]*")") ,
            title = "2022 Tea Bag Decomposition (Warming, Temperature Corrected)",
            color = "Dosing", # Points color, shape and size
            palette = c("#000000","#7DB9DE"),
            add = "reg.line",  # Add regression line
            add.params = list(fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            #cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
            #cor.coeff.args = list(method = "pearson", label.x = 6, label.sep = "\n"),
            ggtheme =  theme_bw()
  )+
  stat_cor(aes(color = Dosing), label.x = 2.5)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(strip.text.y = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(legend.position="bottom")
Tea_tempTC_reg

ggarrange(Tea_temp_reg, Tea_tempTC_reg,
          ncol = 1, nrow = 2,
          labels = c("A","B"),
          font.label = list(size = 14),
          legend = "bottom",
          common.legend = T)

