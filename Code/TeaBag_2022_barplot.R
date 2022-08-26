rm(list = ls())
#This script is for 2022 Tea bag barplots

#Loading packages
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(car)
library(gcookbook)
library(ggpattern)
library(reshape2)

#Loading files
Tea2022_Treat <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/Decomposition/Tea/Tea2022_Treatment.txt',header = T)
Tea2022_Temp <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/2022/2022_Sandbox/Decomposition/Tea/Tea2022_Temperature.txt',header = T)

#Treatment Plot

Tea2022_Treat$Treatment = factor(Tea2022_Treat$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
Tea2022_Treat$Dosing = factor(Tea2022_Treat$Dosing, levels=c('Pre','Post'))
Tea2022_Treat$Ammonia = factor(Tea2022_Treat$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
Tea2022_Treat_mean <- aggregate(Tea2022_Treat$K, by=list(Tea2022_Treat$Treatment, Tea2022_Treat$Tea, Tea2022_Treat$Dosing, Tea2022_Treat$Ammonia), FUN=mean)
Tea2022_Treat_sd <- aggregate(Tea2022_Treat$K, by=list(Tea2022_Treat$Treatment, Tea2022_Treat$Tea, Tea2022_Treat$Dosing, Tea2022_Treat$Ammonia), FUN=sd)
Tea2022_Treat_len <- aggregate(Tea2022_Treat$K, by=list(Tea2022_Treat$Treatment, Tea2022_Treat$Tea, Tea2022_Treat$Dosing, Tea2022_Treat$Ammonia), FUN=length)
Tea2022_Treat_res <- data.frame(Tea2022_Treat_mean, Tea2022_Treat_sd=Tea2022_Treat_sd$x, Tea2022_Treat_len=Tea2022_Treat_len$x)
colnames(Tea2022_Treat_res) = c("Treatment", "Tea", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(Tea2022_Treat_res)
Tea2022_Treat_res$Se <- Tea2022_Treat_res$Sd/sqrt(Tea2022_Treat_res$Count)

#Bar plot
Tea2022_Treat_bar<-ggplot(Tea2022_Treat_res, 
                          aes(x = Treatment, y = Mean, col = Dosing, fill = Treatment)) +
  facet_grid(Tea~Ammonia, scales = "free_y") +
  #geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_col_pattern(
    #aes(),
    pattern = c('none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_angle = c(45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45
    ),
    pattern_fill = "black",
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single',width = 0.9),
  )+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'))+
  labs(x = "Treatment",y = expression("Decomposition rate (K"[D]*")"), title = "2022 Tea Bag Decomposition (Chemical)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(strip.text.y = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

Tea2022_Treat_bar

#decomposition rate per degree day 

#Calculate mean sd
Tea2022kdd_Treat_mean <- aggregate(Tea2022_Treat$KDD, by=list(Tea2022_Treat$Treatment, Tea2022_Treat$Tea, Tea2022_Treat$Dosing, Tea2022_Treat$Ammonia), FUN=mean)
Tea2022kdd_Treat_sd <- aggregate(Tea2022_Treat$KDD, by=list(Tea2022_Treat$Treatment, Tea2022_Treat$Tea, Tea2022_Treat$Dosing, Tea2022_Treat$Ammonia), FUN=sd)
Tea2022kdd_Treat_len <- aggregate(Tea2022_Treat$KDD, by=list(Tea2022_Treat$Treatment, Tea2022_Treat$Tea, Tea2022_Treat$Dosing, Tea2022_Treat$Ammonia), FUN=length)
Tea2022kdd_Treat_res <- data.frame(Tea2022kdd_Treat_mean, Tea2022kdd_Treat_sd=Tea2022kdd_Treat_sd$x, Tea2022kdd_Treat_len=Tea2022kdd_Treat_len$x)
colnames(Tea2022kdd_Treat_res) = c("Treatment", "Tea", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(Tea2022kdd_Treat_res)
Tea2022kdd_Treat_res$Se <- Tea2022_Treatkdd_res$Sd/sqrt(Tea2022kdd_Treat_res$Count)

#Bar plot
Tea2022kdd_Treat_bar<-ggplot(Tea2022kdd_Treat_res, 
                          aes(x = Treatment, y = Mean, col = Dosing, fill = Treatment)) +
  facet_grid(Tea~Ammonia, scales = "free_y") +
  #geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_col_pattern(
    #aes(),
    pattern = c('none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'stripe', 'stripe', 'stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_angle = c(45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45,
                      45, 45, 45, 45, 45, 45, 135, 135, 45, 45, 45, 45
    ),
    pattern_fill = "black",
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single',width = 0.9),
  )+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'))+
  labs(x = "Treatment",y = expression("Decomposition rate (K"[DD]*")"), title = "2022 Tea Bag Decomposition (Chemical)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(strip.text.y = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

Tea2022kdd_Treat_bar


#Temperature plot

Tea2022_Temp$Temperature = factor(Tea2022_Temp$Temperature, levels=c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))
Tea2022_Temp$Dosing = factor(Tea2022_Temp$Dosing, levels=c('Pre','Post'))
Tea2022_Temp$Ammonia = factor(Tea2022_Temp$Ammonia, levels=c('No Ammonia','Ammonia'))

#Calculate mean sd
Tea2022_Temp_mean <- aggregate(Tea2022_Temp$K, by=list(Tea2022_Temp$Temperature, Tea2022_Temp$Tea, Tea2022_Temp$Dosing, Tea2022_Temp$Ammonia), FUN=mean)
Tea2022_Temp_sd <- aggregate(Tea2022_Temp$K, by=list(Tea2022_Temp$Temperature, Tea2022_Temp$Tea, Tea2022_Temp$Dosing, Tea2022_Temp$Ammonia), FUN=sd)
Tea2022_Temp_len <- aggregate(Tea2022_Temp$K, by=list(Tea2022_Temp$Temperature, Tea2022_Temp$Tea, Tea2022_Temp$Dosing, Tea2022_Temp$Ammonia), FUN=length)
Tea2022_Temp_res <- data.frame(Tea2022_Temp_mean, Tea2022_Temp_sd=Tea2022_Temp_sd$x, Tea2022_Temp_len=Tea2022_Temp_len$x)
colnames(Tea2022_Temp_res) = c("Temperature", "Tea", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(Tea2022_Temp_res)
Tea2022_Temp_res$Se <- Tea2022_Temp_res$Sd/sqrt(Tea2022_Temp_res$Count)


Tea2022_Temp_bar <- ggplot(Tea2022_Temp_res, aes(x = Temperature, y = Mean, col = Dosing, fill = Temperature)) + 
  facet_grid(Tea~Ammonia, scales = "free_y") +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Temperature (°C)",y = expression("Decomposition rate (K"[D]*")"), title = "2022 Tea Bag Decomposition (Temperature)")+
  scale_fill_manual(values=c("#FFFFBF", "#FFFF40", "#FFFF00","#FFD500", "#FFC425","#FF8000", "#FF5500", "#FF2A00","#FF0000"),guide = "none")+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.6), width=.2)+
  theme_bw(base_size = 20) +
  theme(legend.position="none") +
  theme(strip.text.x = element_text(colour = "white")) +
  theme(strip.text.y = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

Tea2022_Temp_bar


#Calculate kdd mean sd
Tea2022kdd_Temp_mean <- aggregate(Tea2022_Temp$KDD, by=list(Tea2022_Temp$Temperature, Tea2022_Temp$Tea, Tea2022_Temp$Dosing, Tea2022_Temp$Ammonia), FUN=mean)
Tea2022kdd_Temp_sd <- aggregate(Tea2022_Temp$KDD, by=list(Tea2022_Temp$Temperature, Tea2022_Temp$Tea, Tea2022_Temp$Dosing, Tea2022_Temp$Ammonia), FUN=sd)
Tea2022kdd_Temp_len <- aggregate(Tea2022_Temp$KDD, by=list(Tea2022_Temp$Temperature, Tea2022_Temp$Tea, Tea2022_Temp$Dosing, Tea2022_Temp$Ammonia), FUN=length)
Tea2022kdd_Temp_res <- data.frame(Tea2022kdd_Temp_mean, Tea2022kdd_Temp_sd=Tea2022kdd_Temp_sd$x, Tea2022kdd_Temp_len=Tea2022kdd_Temp_len$x)
colnames(Tea2022kdd_Temp_res) = c("Temperature", "Tea", "Dosing", "Ammonia", "Mean", "Sd", "Count")
str(Tea2022kdd_Temp_res)
Tea2022kdd_Temp_res$Se <- Tea2022kdd_Temp_res$Sd/sqrt(Tea2022kdd_Temp_res$Count)


Tea2022kdd_Temp_bar <- ggplot(Tea2022kdd_Temp_res, aes(x = Temperature, y = Mean, col = Dosing, fill = Temperature)) + 
  facet_grid(Tea~Ammonia, scales = "free_y") +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Temperature (°C)",y = expression("Decomposition rate (K"[DD]*")"), title = "2022 Tea Bag Decomposition (Temperature)")+
  scale_fill_manual(values=c("#FFFFBF", "#FFFF40", "#FFFF00","#FFD500", "#FFC425","#FF8000", "#FF5500", "#FF2A00","#FF0000"),guide = "none")+
  scale_color_manual(values=c("#000000","#7DB9DE"))+
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.6), width=.2)+
  theme_bw(base_size = 20) +
  theme(legend.position="bottom") +
  theme(strip.text.x = element_text(colour = "white")) +
  theme(strip.text.y = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

Tea2022kdd_Temp_bar

