rm(list = ls())
#This script is for 2022 Sediment Wash ATP barplots

#Loading packages
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(car)
library(gcookbook)
library(ggpattern)
library(reshape2)

#Sediment Wash Bar Plot

#Loading files
ATPSedimentWash2022_Treat <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPSedimentWash2022_Treatment.txt',header = T)
ATPSedimentWash2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPSedimentWash2022_Temperature.txt',header = T)

#Treatment Plot

ATPSedimentWash2022_Treat$Treatment = factor(ATPSedimentWash2022_Treat$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
ATPSedimentWash2022_Treat$Dosing = factor(ATPSedimentWash2022_Treat$Dosing, levels=c('Pre','Post'))
ATPSedimentWash2022_Treat$Ammonia = factor(ATPSedimentWash2022_Treat$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
ATPSedimentWash2022_Treat_mean <- aggregate(ATPSedimentWash2022_Treat$ATP, by=list(ATPSedimentWash2022_Treat$Treatment, ATPSedimentWash2022_Treat$Dosing, ATPSedimentWash2022_Treat$Ammonia), FUN=mean)
ATPSedimentWash2022_Treat_sd <- aggregate(ATPSedimentWash2022_Treat$ATP, by=list(ATPSedimentWash2022_Treat$Treatment, ATPSedimentWash2022_Treat$Dosing, ATPSedimentWash2022_Treat$Ammonia), FUN=sd)
ATPSedimentWash2022_Treat_len <- aggregate(ATPSedimentWash2022_Treat$ATP, by=list(ATPSedimentWash2022_Treat$Treatment, ATPSedimentWash2022_Treat$Dosing, ATPSedimentWash2022_Treat$Ammonia), FUN=length)
ATPSedimentWash2022_Treat_res <- data.frame(ATPSedimentWash2022_Treat_mean, ATPSedimentWash2022_Treat_sd=ATPSedimentWash2022_Treat_sd$x, ATPSedimentWash2022_Treat_len=ATPSedimentWash2022_Treat_len$x)
colnames(ATPSedimentWash2022_Treat_res) = c("Treatment", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(ATPSedimentWash2022_Treat_res)
ATPSedimentWash2022_Treat_res$Se <- ATPSedimentWash2022_Treat_res$Sd/sqrt(ATPSedimentWash2022_Treat_res$Count)

#Bar plot
ATPSedimentWash2022_Treat_bar<-ggplot(ATPSedimentWash2022_Treat_res, 
                                      aes(x = Treatment, y = Mean, col = Dosing, fill = Treatment)) +
  facet_grid(~Ammonia) +
  #geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_col_pattern(
    #aes(),
    pattern = c('none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_angle = c(45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45
    ),
    pattern_fill = "black",
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single',width = 0.9),
    show.legend = F
  )+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'))+
  labs(x = "Chemical Treatment",y = "ATP (nmol/g)", title = "2022 Sediment Wash ATP (Chemical)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="bottom") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ATPSedimentWash2022_Treat_bar

#Temperature plot

ATPSedimentWash2022_Temp$Temperature = factor(ATPSedimentWash2022_Temp$Temperature, levels=c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))
ATPSedimentWash2022_Temp$Dosing = factor(ATPSedimentWash2022_Temp$Dosing, levels=c('Pre','Post'))
ATPSedimentWash2022_Temp$Ammonia = factor(ATPSedimentWash2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
ATPSedimentWash2022_Temp_mean <- aggregate(ATPSedimentWash2022_Temp$ATP, by=list(ATPSedimentWash2022_Temp$Temperature, ATPSedimentWash2022_Temp$Dosing, ATPSedimentWash2022_Temp$Ammonia), FUN=mean)
ATPSedimentWash2022_Temp_sd <- aggregate(ATPSedimentWash2022_Temp$ATP, by=list(ATPSedimentWash2022_Temp$Temperature, ATPSedimentWash2022_Temp$Dosing, ATPSedimentWash2022_Temp$Ammonia), FUN=sd)
ATPSedimentWash2022_Temp_len <- aggregate(ATPSedimentWash2022_Temp$ATP, by=list(ATPSedimentWash2022_Temp$Temperature, ATPSedimentWash2022_Temp$Dosing, ATPSedimentWash2022_Temp$Ammonia), FUN=length)
ATPSedimentWash2022_Temp_res <- data.frame(ATPSedimentWash2022_Temp_mean, ATPSedimentWash2022_Temp_sd=ATPSedimentWash2022_Temp_sd$x, ATPSedimentWash2022_Temp_len=ATPSedimentWash2022_Temp_len$x)
colnames(ATPSedimentWash2022_Temp_res) = c("Temperature", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(ATPSedimentWash2022_Temp_res)
ATPSedimentWash2022_Temp_res$Se <- ATPSedimentWash2022_Temp_res$Sd/sqrt(ATPSedimentWash2022_Temp_res$Count)


ATPSedimentWash2022_Temp_bar <- ggplot(ATPSedimentWash2022_Temp_res, aes(x = Temperature, y = Mean, col = Dosing, fill = Temperature)) + 
  facet_grid(~Ammonia) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Temperature (°C)",y = "ATP (nmol/g)", title = "2022 Sediment Wash ATP (Temperature)")+
  scale_fill_manual(values=c("#FFFFBF", "#FFFF40", "#FFFF00","#FFD500", "#FFC425","#FF8000", "#FF5500", "#FF2A00","#FF0000"),guide = "none")+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.6), width=.2)+
  theme_bw(base_size = 20) +
  theme(legend.position="bottom") +
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ATPSedimentWash2022_Temp_bar

#Sediment Bar Plot

#Loading files
ATPSediment2022_Treat <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPSediment2022_Treatment.txt',header = T)
ATPSediment2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPSediment2022_Temperature.txt',header = T)

#Treatment Plot

ATPSediment2022_Treat$Treatment = factor(ATPSediment2022_Treat$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
ATPSediment2022_Treat$Dosing = factor(ATPSediment2022_Treat$Dosing, levels=c('Pre','Post'))
ATPSediment2022_Treat$Ammonia = factor(ATPSediment2022_Treat$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
ATPSediment2022_Treat_mean <- aggregate(ATPSediment2022_Treat$ATP, by=list(ATPSediment2022_Treat$Treatment, ATPSediment2022_Treat$Dosing, ATPSediment2022_Treat$Ammonia), FUN=mean)
ATPSediment2022_Treat_sd <- aggregate(ATPSediment2022_Treat$ATP, by=list(ATPSediment2022_Treat$Treatment, ATPSediment2022_Treat$Dosing, ATPSediment2022_Treat$Ammonia), FUN=sd)
ATPSediment2022_Treat_len <- aggregate(ATPSediment2022_Treat$ATP, by=list(ATPSediment2022_Treat$Treatment, ATPSediment2022_Treat$Dosing, ATPSediment2022_Treat$Ammonia), FUN=length)
ATPSediment2022_Treat_res <- data.frame(ATPSediment2022_Treat_mean, ATPSediment2022_Treat_sd=ATPSediment2022_Treat_sd$x, ATPSediment2022_Treat_len=ATPSediment2022_Treat_len$x)
colnames(ATPSediment2022_Treat_res) = c("Treatment", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(ATPSediment2022_Treat_res)
ATPSediment2022_Treat_res$Se <- ATPSediment2022_Treat_res$Sd/sqrt(ATPSediment2022_Treat_res$Count)

#Bar plot
ATPSediment2022_Treat_bar<-ggplot(ATPSediment2022_Treat_res, 
                                  aes(x = Treatment, y = Mean, col = Dosing, fill = Treatment)) +
  facet_grid(~Ammonia) +
  #geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_col_pattern(
    #aes(),
    pattern = c('none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_angle = c(45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45
    ),
    pattern_fill = "black",
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single',width = 0.9),
    show.legend = F
  )+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'))+
  labs(x = "Chemical Treatment",y = "ATP (nmol/g)", title = "2022 Sediment ATP (Chemical)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="bottom") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ATPSediment2022_Treat_bar

#Temperature plot

ATPSediment2022_Temp$Temperature = factor(ATPSediment2022_Temp$Temperature, levels=c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))
ATPSediment2022_Temp$Dosing = factor(ATPSediment2022_Temp$Dosing, levels=c('Pre','Post'))
ATPSediment2022_Temp$Ammonia = factor(ATPSediment2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
ATPSediment2022_Temp_mean <- aggregate(ATPSediment2022_Temp$ATP, by=list(ATPSediment2022_Temp$Temperature, ATPSediment2022_Temp$Dosing, ATPSediment2022_Temp$Ammonia), FUN=mean)
ATPSediment2022_Temp_sd <- aggregate(ATPSediment2022_Temp$ATP, by=list(ATPSediment2022_Temp$Temperature, ATPSediment2022_Temp$Dosing, ATPSediment2022_Temp$Ammonia), FUN=sd)
ATPSediment2022_Temp_len <- aggregate(ATPSediment2022_Temp$ATP, by=list(ATPSediment2022_Temp$Temperature, ATPSediment2022_Temp$Dosing, ATPSediment2022_Temp$Ammonia), FUN=length)
ATPSediment2022_Temp_res <- data.frame(ATPSediment2022_Temp_mean, ATPSediment2022_Temp_sd=ATPSediment2022_Temp_sd$x, ATPSediment2022_Temp_len=ATPSediment2022_Temp_len$x)
colnames(ATPSediment2022_Temp_res) = c("Temperature", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(ATPSediment2022_Temp_res)
ATPSediment2022_Temp_res$Se <- ATPSediment2022_Temp_res$Sd/sqrt(ATPSediment2022_Temp_res$Count)


ATPSediment2022_Temp_bar <- ggplot(ATPSediment2022_Temp_res, aes(x = Temperature, y = Mean, col = Dosing, fill = Temperature)) + 
  facet_grid(~Ammonia) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Temperature (°C)",y = "ATP (nmol/g)", title = "2022 Sediment ATP (Temperature)")+
  scale_fill_manual(values=c("#FFFFBF", "#FFFF40", "#FFFF00","#FFD500", "#FFC425","#FF8000", "#FF5500", "#FF2A00","#FF0000"),guide = "none")+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.6), width=.2)+
  theme_bw(base_size = 20) +
  theme(legend.position="bottom") +
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ATPSediment2022_Temp_bar

#Water Bar Plot

#Loading files
ATPWater2022_Treat <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPWater2022_Treatment.txt',header = T)
ATPWater2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPWater2022_Temperature.txt',header = T)

#Treatment Plot

ATPWater2022_Treat$Treatment = factor(ATPWater2022_Treat$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
ATPWater2022_Treat$Dosing = factor(ATPWater2022_Treat$Dosing, levels=c('Pre','Post'))
ATPWater2022_Treat$Ammonia = factor(ATPWater2022_Treat$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
ATPWater2022_Treat_mean <- aggregate(ATPWater2022_Treat$ATP, by=list(ATPWater2022_Treat$Treatment, ATPWater2022_Treat$Dosing, ATPWater2022_Treat$Ammonia), FUN=mean)
ATPWater2022_Treat_sd <- aggregate(ATPWater2022_Treat$ATP, by=list(ATPWater2022_Treat$Treatment, ATPWater2022_Treat$Dosing, ATPWater2022_Treat$Ammonia), FUN=sd)
ATPWater2022_Treat_len <- aggregate(ATPWater2022_Treat$ATP, by=list(ATPWater2022_Treat$Treatment, ATPWater2022_Treat$Dosing, ATPWater2022_Treat$Ammonia), FUN=length)
ATPWater2022_Treat_res <- data.frame(ATPWater2022_Treat_mean, ATPWater2022_Treat_sd=ATPWater2022_Treat_sd$x, ATPWater2022_Treat_len=ATPWater2022_Treat_len$x)
colnames(ATPWater2022_Treat_res) = c("Treatment", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(ATPWater2022_Treat_res)
ATPWater2022_Treat_res$Se <- ATPWater2022_Treat_res$Sd/sqrt(ATPWater2022_Treat_res$Count)

#Bar plot
ATPWater2022_Treat_bar<-ggplot(ATPWater2022_Treat_res, 
                               aes(x = Treatment, y = Mean, col = Dosing, fill = Treatment)) +
  facet_grid(~Ammonia) +
  #geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_col_pattern(
    #aes(),
    pattern = c('none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_angle = c(45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45
    ),
    pattern_fill = "black",
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single',width = 0.9),
    show.legend = F
  )+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'))+
  labs(x = "Chemical Treatment",y = "ATP (nmol/g)", title = "2022 Water ATP (Chemical)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="bottom") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ATPWater2022_Treat_bar

#Temperature plot

ATPWater2022_Temp$Temperature = factor(ATPWater2022_Temp$Temperature, levels=c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))
ATPWater2022_Temp$Dosing = factor(ATPWater2022_Temp$Dosing, levels=c('Pre','Post'))
ATPWater2022_Temp$Ammonia = factor(ATPWater2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
ATPWater2022_Temp_mean <- aggregate(ATPWater2022_Temp$ATP, by=list(ATPWater2022_Temp$Temperature, ATPWater2022_Temp$Dosing, ATPWater2022_Temp$Ammonia), FUN=mean)
ATPWater2022_Temp_sd <- aggregate(ATPWater2022_Temp$ATP, by=list(ATPWater2022_Temp$Temperature, ATPWater2022_Temp$Dosing, ATPWater2022_Temp$Ammonia), FUN=sd)
ATPWater2022_Temp_len <- aggregate(ATPWater2022_Temp$ATP, by=list(ATPWater2022_Temp$Temperature, ATPWater2022_Temp$Dosing, ATPWater2022_Temp$Ammonia), FUN=length)
ATPWater2022_Temp_res <- data.frame(ATPWater2022_Temp_mean, ATPWater2022_Temp_sd=ATPWater2022_Temp_sd$x, ATPWater2022_Temp_len=ATPWater2022_Temp_len$x)
colnames(ATPWater2022_Temp_res) = c("Temperature", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(ATPWater2022_Temp_res)
ATPWater2022_Temp_res$Se <- ATPWater2022_Temp_res$Sd/sqrt(ATPWater2022_Temp_res$Count)


ATPWater2022_Temp_bar <- ggplot(ATPWater2022_Temp_res, aes(x = Temperature, y = Mean, col = Dosing, fill = Temperature)) + 
  facet_grid(~Ammonia) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Temperature (°C)",y = "ATP (nmol/g)", title = "2022 Water ATP (Temperature)")+
  scale_fill_manual(values=c("#FFFFBF", "#FFFF40", "#FFFF00","#FFD500", "#FFC425","#FF8000", "#FF5500", "#FF2A00","#FF0000"),guide = "none")+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.6), width=.2)+
  theme_bw(base_size = 20) +
  theme(legend.position="bottom") +
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ATPWater2022_Temp_bar

#Water/cell Bar Plot

#Loading files
ATPPercell2022_Treat <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPpercell2022_Treatment.txt',header = T)
ATPPercell2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/ATP/ATPpercell2022_Temperature.txt',header = T)

#Treatment Plot

ATPPercell2022_Treat$Treatment = factor(ATPPercell2022_Treat$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
ATPPercell2022_Treat$Dosing = factor(ATPPercell2022_Treat$Dosing, levels=c('Pre','Post'))
ATPPercell2022_Treat$Ammonia = factor(ATPPercell2022_Treat$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
ATPPercell2022_Treat_mean <- aggregate(ATPPercell2022_Treat$ATP, by=list(ATPPercell2022_Treat$Treatment, ATPPercell2022_Treat$Dosing, ATPPercell2022_Treat$Ammonia), FUN=mean)
ATPPercell2022_Treat_sd <- aggregate(ATPPercell2022_Treat$ATP, by=list(ATPPercell2022_Treat$Treatment, ATPPercell2022_Treat$Dosing, ATPPercell2022_Treat$Ammonia), FUN=sd)
ATPPercell2022_Treat_len <- aggregate(ATPPercell2022_Treat$ATP, by=list(ATPPercell2022_Treat$Treatment, ATPPercell2022_Treat$Dosing, ATPPercell2022_Treat$Ammonia), FUN=length)
ATPPercell2022_Treat_res <- data.frame(ATPPercell2022_Treat_mean, ATPPercell2022_Treat_sd=ATPPercell2022_Treat_sd$x, ATPPercell2022_Treat_len=ATPPercell2022_Treat_len$x)
colnames(ATPPercell2022_Treat_res) = c("Treatment", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(ATPPercell2022_Treat_res)
ATPPercell2022_Treat_res$Se <- ATPPercell2022_Treat_res$Sd/sqrt(ATPPercell2022_Treat_res$Count)

#Bar plot
ATPPercell2022_Treat_bar<-ggplot(ATPPercell2022_Treat_res, 
                                 aes(x = Treatment, y = Mean, col = Dosing, fill = Treatment)) +
  facet_grid(~Ammonia) +
  #geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_col_pattern(
    #aes(),
    pattern = c('none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_angle = c(45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45
    ),
    pattern_fill = "black",
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single',width = 0.9),
  )+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'))+
  labs(x = "Treatment",y = "ATP (nmol/g·cell)", title = "2022 Water ATP (Chemical)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ATPPercell2022_Treat_bar

#Temperature plot

ATPPercell2022_Temp$Temperature = factor(ATPPercell2022_Temp$Temperature, levels=c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))
ATPPercell2022_Temp$Dosing = factor(ATPPercell2022_Temp$Dosing, levels=c('Pre','Post'))
ATPPercell2022_Temp$Ammonia = factor(ATPPercell2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
ATPPercell2022_Temp_mean <- aggregate(ATPPercell2022_Temp$ATP, by=list(ATPPercell2022_Temp$Temperature, ATPPercell2022_Temp$Dosing, ATPPercell2022_Temp$Ammonia), FUN=mean)
ATPPercell2022_Temp_sd <- aggregate(ATPPercell2022_Temp$ATP, by=list(ATPPercell2022_Temp$Temperature, ATPPercell2022_Temp$Dosing, ATPPercell2022_Temp$Ammonia), FUN=sd)
ATPPercell2022_Temp_len <- aggregate(ATPPercell2022_Temp$ATP, by=list(ATPPercell2022_Temp$Temperature, ATPPercell2022_Temp$Dosing, ATPPercell2022_Temp$Ammonia), FUN=length)
ATPPercell2022_Temp_res <- data.frame(ATPPercell2022_Temp_mean, ATPPercell2022_Temp_sd=ATPPercell2022_Temp_sd$x, ATPPercell2022_Temp_len=ATPPercell2022_Temp_len$x)
colnames(ATPPercell2022_Temp_res) = c("Temperature", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(ATPPercell2022_Temp_res)
ATPPercell2022_Temp_res$Se <- ATPPercell2022_Temp_res$Sd/sqrt(ATPPercell2022_Temp_res$Count)


ATPPercell2022_Temp_bar <- ggplot(ATPPercell2022_Temp_res, aes(x = Temperature, y = Mean, col = Dosing, fill = Temperature)) + 
  facet_grid(~Ammonia) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Temperature (°C)",y = "ATP (nmol/ g·cell)", title = "2022 Water ATP (Temperature)")+
  scale_fill_manual(values=c("#FFFFBF", "#FFFF40", "#FFFF00","#FFD500", "#FFC425","#FF8000", "#FF5500", "#FF2A00","#FF0000"),guide = "none")+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.6), width=.2)+
  theme_bw(base_size = 20) +
  theme(legend.position="bottom") +
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ATPPercell2022_Temp_bar




