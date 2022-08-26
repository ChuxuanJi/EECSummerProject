rm(list = ls())
#This script is for 2022 MicroResp barplots

#Loading packages
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(car)
library(gcookbook)
library(ggpattern)
library(reshape2)

#Loading files
MicroResp2022_Treat <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/MicroResp/MicroResp2022_Treatment.txt',header = T)
MicroResp2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/ATP-enzymes-microresp/MicroResp/MicroResp2022_Temperature.txt',header = T)

#Treatment Plot

MicroResp2022_Treat$Treatment = factor(MicroResp2022_Treat$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
MicroResp2022_Treat$Dosing = factor(MicroResp2022_Treat$Dosing, levels=c('Pre','Post'))
MicroResp2022_Treat$Ammonia = factor(MicroResp2022_Treat$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
MicroResp2022_Treat_mean <- aggregate(MicroResp2022_Treat$MicroResp, by=list(MicroResp2022_Treat$Treatment, MicroResp2022_Treat$Dosing, MicroResp2022_Treat$Ammonia), FUN=mean)
MicroResp2022_Treat_sd <- aggregate(MicroResp2022_Treat$MicroResp, by=list(MicroResp2022_Treat$Treatment, MicroResp2022_Treat$Dosing, MicroResp2022_Treat$Ammonia), FUN=sd)
MicroResp2022_Treat_len <- aggregate(MicroResp2022_Treat$MicroResp, by=list(MicroResp2022_Treat$Treatment, MicroResp2022_Treat$Dosing, MicroResp2022_Treat$Ammonia), FUN=length)
MicroResp2022_Treat_res <- data.frame(MicroResp2022_Treat_mean, MicroResp2022_Treat_sd=MicroResp2022_Treat_sd$x, MicroResp2022_Treat_len=MicroResp2022_Treat_len$x)
colnames(MicroResp2022_Treat_res) = c("Treatment", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(MicroResp2022_Treat_res)
MicroResp2022_Treat_res$Se <- MicroResp2022_Treat_res$Sd/sqrt(MicroResp2022_Treat_res$Count)

#Bar plot
MicroResp2022_Treat_bar<-ggplot(MicroResp2022_Treat_res, 
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
  labs(x = "Chemical Treatment",y = "Respiration (mg)", title = "2022 MicroResp (Chemical)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="bottom") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

MicroResp2022_Treat_bar

#Temperature plot

MicroResp2022_Temp$Temperature = factor(MicroResp2022_Temp$Temperature, levels=c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))
MicroResp2022_Temp$Dosing = factor(MicroResp2022_Temp$Dosing, levels=c('Pre','Post'))
MicroResp2022_Temp$Ammonia = factor(MicroResp2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
MicroResp2022_Temp_mean <- aggregate(MicroResp2022_Temp$MicroResp, by=list(MicroResp2022_Temp$Temperature, MicroResp2022_Temp$Dosing, MicroResp2022_Temp$Ammonia), FUN=mean)
MicroResp2022_Temp_sd <- aggregate(MicroResp2022_Temp$MicroResp, by=list(MicroResp2022_Temp$Temperature, MicroResp2022_Temp$Dosing, MicroResp2022_Temp$Ammonia), FUN=sd)
MicroResp2022_Temp_len <- aggregate(MicroResp2022_Temp$MicroResp, by=list(MicroResp2022_Temp$Temperature, MicroResp2022_Temp$Dosing, MicroResp2022_Temp$Ammonia), FUN=length)
MicroResp2022_Temp_res <- data.frame(MicroResp2022_Temp_mean, MicroResp2022_Temp_sd=MicroResp2022_Temp_sd$x, MicroResp2022_Temp_len=MicroResp2022_Temp_len$x)
colnames(MicroResp2022_Temp_res) = c("Temperature", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(MicroResp2022_Temp_res)
MicroResp2022_Temp_res$Se <- MicroResp2022_Temp_res$Sd/sqrt(MicroResp2022_Temp_res$Count)


MicroResp2022_Temp_bar <- ggplot(MicroResp2022_Temp_res, aes(x = Temperature, y = Mean, col = Dosing, fill = Temperature)) + 
  facet_grid(~Ammonia) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Temperature (°C)",y = "Respiration (mg)", title = "2022 MicroResp (Temperature)")+
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

MicroResp2022_Temp_bar
