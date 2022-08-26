rm(list = ls())
#This script is for 2022 BOD barplots

#Loading packages
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(car)
library(gcookbook)
library(ggpattern)
library(reshape2)

#Loading files
BOD2022_Treat <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/BOD/2022BOD_Treatment.txt',header = T)
BOD2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/BOD/2022BOD_Temperature.txt',header = T)

#Treatment Plot

BOD2022_Treat$Treatment = factor(BOD2022_Treat$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
BOD2022_Treat$Dosing = factor(BOD2022_Treat$Dosing, levels=c('Pre','Post'))
BOD2022_Treat$Ammonia = factor(BOD2022_Treat$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
BOD2022_Treat_mean <- aggregate(BOD2022_Treat$BOD, by=list(BOD2022_Treat$Treatment, BOD2022_Treat$Dosing, BOD2022_Treat$Ammonia), FUN=mean)
BOD2022_Treat_sd <- aggregate(BOD2022_Treat$BOD, by=list(BOD2022_Treat$Treatment, BOD2022_Treat$Dosing, BOD2022_Treat$Ammonia), FUN=sd)
BOD2022_Treat_len <- aggregate(BOD2022_Treat$BOD, by=list(BOD2022_Treat$Treatment, BOD2022_Treat$Dosing, BOD2022_Treat$Ammonia), FUN=length)
BOD2022_Treat_res <- data.frame(BOD2022_Treat_mean, BOD2022_Treat_sd=BOD2022_Treat_sd$x, BOD2022_Treat_len=BOD2022_Treat_len$x)
colnames(BOD2022_Treat_res) = c("Treatment", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(BOD2022_Treat_res)
BOD2022_Treat_res$Se <- BOD2022_Treat_res$Sd/sqrt(BOD2022_Treat_res$Count)

#Bar plot
BOD2022_Treat_bar<-ggplot(BOD2022_Treat_res, 
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
  labs(x = "Chemical Treatment",y = "BOD (mg/L)", title = "2022 BOD (Chemical)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="bottom") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

BOD2022_Treat_bar

#Temperature plot

BOD2022_Temp$Temperature = factor(BOD2022_Temp$Temperature, levels=c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))
BOD2022_Temp$Dosing = factor(BOD2022_Temp$Dosing, levels=c('Pre','Post'))
BOD2022_Temp$Ammonia = factor(BOD2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
BOD2022_Temp_mean <- aggregate(BOD2022_Temp$BOD, by=list(BOD2022_Temp$Temperature, BOD2022_Temp$Dosing, BOD2022_Temp$Ammonia), FUN=mean)
BOD2022_Temp_sd <- aggregate(BOD2022_Temp$BOD, by=list(BOD2022_Temp$Temperature, BOD2022_Temp$Dosing, BOD2022_Temp$Ammonia), FUN=sd)
BOD2022_Temp_len <- aggregate(BOD2022_Temp$BOD, by=list(BOD2022_Temp$Temperature, BOD2022_Temp$Dosing, BOD2022_Temp$Ammonia), FUN=length)
BOD2022_Temp_res <- data.frame(BOD2022_Temp_mean, BOD2022_Temp_sd=BOD2022_Temp_sd$x, BOD2022_Temp_len=BOD2022_Temp_len$x)
colnames(BOD2022_Temp_res) = c("Temperature", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(BOD2022_Temp_res)
BOD2022_Temp_res$Se <- BOD2022_Temp_res$Sd/sqrt(BOD2022_Temp_res$Count)


BOD2022_Temp_bar <- ggplot(BOD2022_Temp_res, aes(x = Temperature, y = Mean, col = Dosing, fill = Temperature)) + 
  facet_grid(~Ammonia) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Temperature (°C)",y = "BOD (mg/L)", title = "2022 BOD (Temperature)")+
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

BOD2022_Temp_bar
