rm(list=ls())
#ATP
#Loading packages
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(car)
library(gcookbook)
library(ggpattern)
library(reshape2)
############
#Cauculate ATP
#Dilution Factor Water&SedimentWash: 50, Sediment: 2500
#Water
ATPSW <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/3YearDataSummary/ATPSW_3Year_Correct.txt',header = T)

ATPSW$ATP <- (ATPSW$ATP_max)/1693.1*50
write.table(ATPSW,'C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/3YearDataSummary/ATPSW_3Year_Correct.txt',row.names = F,sep = '\t',quote = F)

#Loading files
ATPSW_Treat <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/3YearDataSummary/ATPSW_3Year_Treatment_C.txt',header = T)

ATPSW_Treat$Treatment = factor(ATPSW_Treat$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
ATPSW_Treat$Year <- as.factor(ATPSW_Treat$Year)


#Calculate mean sd
ATPSW_Treat_mean <- aggregate(ATPSW_Treat$ATP, by=list(ATPSW_Treat$Treatment, ATPSW_Treat$Year), FUN=mean)
ATPSW_Treat_sd <- aggregate(ATPSW_Treat$ATP, by=list(ATPSW_Treat$Treatment, ATPSW_Treat$Year), FUN=sd)
ATPSW_Treat_len <- aggregate(ATPSW_Treat$ATP, by=list(ATPSW_Treat$Treatment, ATPSW_Treat$Year), FUN=length)
ATPSW_Treat_res <- data.frame(ATPSW_Treat_mean, ATPSW_Treat_sd=ATPSW_Treat_sd$x, ATPSW_Treat_len=ATPSW_Treat_len$x)
colnames(ATPSW_Treat_res) = c("Treatment", "Year", "Mean", "Sd", "Count")
str(ATPSW_Treat_res)
ATPSW_Treat_res$Se <- ATPSW_Treat_res$Sd/sqrt(ATPSW_Treat_res$Count)

#Bar plot
ATPSW_Treat_bar<-ggplot(ATPSW_Treat_res, 
                        aes(x = Treatment, y = Mean, col = Year, fill = Treatment)) +
  #geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_col_pattern(
    #aes(),
    pattern = c('none', 'none', 'none', 'none', 'none', 'none', 'stripe', 'stripe','stripe', 'stripe','stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_fill = "black",
    pattern_angle = c(45, 45, 45, 45, 45, 45, 45, 45, 45, 135, 135, 135, 45, 45,45, 45,45, 45
    ),
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single',width = 0.9),
  )+
  scale_color_viridis_d(begin = 0, end = 0.5)+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'),guide = "none")+
  labs(x = "Treatment",y = "ATP (nmol/g)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="bottom") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ATPSW_Treat_bar

#This script is for Three Years MicroResp plots

#Loading files

MR <- read.delim('C:/Users/lenovo/Desktop/Imperial/EEC Summer Project/Data/3YearDataSummary/MicroResp_chem_Summary.csv',sep = ',')

MR$Treatment = factor(MR$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
MR$Year <- as.factor(MR$Year)

MR_treatment_group_box <- ggplot(MR, aes(x = Treatment, y = MicroResp_SW, col = Year, fill = Treatment)) + 
  geom_boxplot() +
  scale_color_viridis_d(begin = 0, end = 0.5)+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'),guide = "none")+
  #scale_fill_manual(values=c('#828282', '#C0C0C0', '#FFFFFF', '#6E552F', '#B68C2A', '#FFC425', '#828282', '#C0C0C0', '#FFFFFF', '#828282', '#C0C0C0', '#FFFFFF', '#828282', '#C0C0C0', '#FFFFFF', '#6E552F', '#B68C2A', '#FFC425', 
  #  '#828282', '#C0C0C0', '#FFFFFF', '#6E552F', '#B68C2A', '#FFC425', '#828282', '#C0C0C0', '#FFFFFF', '#828282', '#C0C0C0', '#FFFFFF', '#828282', '#C0C0C0', '#FFFFFF', '#6E552F', '#B68C2A', '#FFC425'))+
  labs(x = "Treatment",y = "Respiration (mg)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="bottom") +
  geom_boxplot_pattern(
    aes(),
    pattern = c('none', 'none', 'none', 'none', 'none', 'none', 'stripe', 'stripe','stripe', 'stripe','stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_fill = "black",
    pattern_angle = c(45, 45, 45, 45, 45, 45, 45, 45, 45, 135, 135, 135, 45, 45,45, 45,45, 45
    ),
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single'),
  )+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


MR_treatment_group_box

#This script is for 2019-2022 leaf decomposition barplots

#Loading files
LD <- read.delim('C:/Users/lenovo/Desktop/Leaf/LD201920212022.txt',header = T)

LD$Treatment = factor(LD$Treatment, levels=c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))
LD$Year <- as.factor(LD$Year)


#Calculate mean sd
LD_mean <- aggregate(LD$K, by=list(LD$Treatment, LD$Year, LD$Type), FUN=mean)
LD_sd <- aggregate(LD$K, by=list(LD$Treatment, LD$Year, LD$Type), FUN=sd)
LD_len <- aggregate(LD$K, by=list(LD$Treatment, LD$Year, LD$Type), FUN=length)
LD_res <- data.frame(LD_mean, LD_sd=LD_sd$x, LD_len=LD_len$x)
colnames(LD_res) = c("Treatment", "Year", "Type", "Mean", "Sd", "Count")
str(LD_res)
LD_res$Se <- LD_res$Sd/sqrt(LD_res$Count)

#Bar plot
LD_bar<-ggplot(LD_res, 
               aes(x = Treatment, y = Mean, col = Year, fill = Treatment)) +
  facet_grid(~Type) +
  #geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_col_pattern(
    #aes(),
    pattern = c('none', 'none', 'none', 'none', 'none', 'none', 'stripe', 'stripe','stripe', 'stripe','stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'none', 'none', 'stripe', 'stripe','stripe', 'stripe','stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_angle = c(45, 45, 45, 45, 45, 45, 45, 45, 45, 135, 135, 135, 45, 45,45, 45,45, 45,
                      45, 45, 45, 45, 45, 45, 45, 45, 45, 135, 135, 135, 45, 45,45, 45,45, 45
    ),
    pattern_fill = "black",
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single',width = 0.9),
  )+
  scale_color_viridis_d(begin = 0, end = 0.5)+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'),guide = "none")+
  labs(x = "Treatment",y = expression("Decomposition rate (K"[D]*")"), title = "Leaf decomposition (Not corrected)")+
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

LD_bar

#decomposition rate per degree day 

#Calculate mean sd
LDkdd_mean <- aggregate(LD$KDD, by=list(LD$Treatment, LD$Year, LD$Type), FUN=mean)
LDkdd_sd <- aggregate(LD$KDD, by=list(LD$Treatment, LD$Year, LD$Type), FUN=sd)
LDkdd_len <- aggregate(LD$KDD, by=list(LD$Treatment, LD$Year, LD$Type), FUN=length)
LDkdd_res <- data.frame(LDkdd_mean, LDkdd_sd=LDkdd_sd$x, LDkdd_len=LDkdd_len$x)
colnames(LDkdd_res) = c("Treatment", "Year", "Type", "Mean", "Sd", "Count")
str(LDkdd_res)
LDkdd_res$Se <- LDkdd_res$Sd/sqrt(LDkdd_res$Count)

#Bar plot
LDkdd_bar<-ggplot(LDkdd_res, 
                  aes(x = Treatment, y = Mean, col = Year, fill = Treatment)) +
  facet_grid(~Type) +
  #geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_col_pattern(
    #aes(),
    pattern = c('none', 'none', 'none', 'none', 'none', 'none', 'stripe', 'stripe','stripe', 'stripe','stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch',
                'none', 'none', 'none', 'none', 'none', 'none', 'stripe', 'stripe','stripe', 'stripe','stripe', 'stripe','crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch', 'crosshatch'
    ),
    pattern_fill = "black",
    pattern_angle = c(45, 45, 45, 45, 45, 45, 45, 45, 45, 135, 135, 135, 45, 45,45, 45,45, 45,
                      45, 45, 45, 45, 45, 45, 45, 45, 45, 135, 135, 135, 45, 45,45, 45,45, 45
    ),
    pattern_density = 0.04,
    pattern_spacing = 0.02,
    position = position_dodge2(preserve = 'single',width = 0.9),
  )+
  scale_color_viridis_d(begin = 0, end = 0.5)+
  scale_fill_manual(values=c('white', '#FFC425','white', 'white', 'white', '#FFC425'),guide = "none")+
  labs(x = "Treatment",y = expression("Decomposition rate (K"[DD]*")"), title = "Leaf decomposition (Temperature corrected)")+
  theme_bw(base_size = 20 ) +
  theme(legend.position="bottom") +
  geom_errorbar(aes(ymin=Mean-Sd, ymax=Mean +Sd), position=position_dodge(.9), width=.2)+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(strip.text.y = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

LDkdd_bar
