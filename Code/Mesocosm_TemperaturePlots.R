rm(list=ls())
##Install packages
library(tidyverse)
library(ggpubr)
#Warming ponds temperature plots 
#Loading files 
temp_A <- read.delim("F:/Mesocosm Temperature/Mesocosm_temp_daily_DeepA.csv",sep = ',')
temp_B <- read.delim("F:/Mesocosm Temperature/Mesocosm_temp_daily_DeepB.csv",sep = ',')
treatments<-read.csv("F:/Mesocosm Temperature/Treatment.csv",sep = ',')

str(temp_A)
temp_A$Date<-as.Date(temp_A$Date)
temp_A$Date<-as.POSIXct(temp_A$Date, format = "%d/%m/%Y")
temp_A$Month = format(temp_A$Date, format = "%b")
temp_A$Year = format(temp_A$Date, format = "%Y")
head(temp_A)

final_A<-merge(temp_A, treatments, by=c("Pond"))
str(final_A)

##Create plots for average monthly temperatures

temp_year_A<-final_A %>%
  #filter(year=="2020")%>%
  group_by(Month, Temperature, Year)%>%
  summarise(mean_month_temp=mean(Mean_temp),
            se=sd(Mean_temp)/sqrt(n()),
            max_day_temp=max(Mean_temp),
            min_day_temp=min(Mean_temp),
            var_day_temp=var(Mean_temp))
head(temp_year_A)

temp_year_A$Month<-factor(temp_year_A$Month, levels = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月"))
temp_year_A$Temperature<-factor(temp_year_A$Temperature, levels = c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))

levels(temp_year_A$Month)[levels(temp_year_A$Month)=="1月"] <- "Jan"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="2月"] <- "Feb"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="3月"] <- "Mar"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="4月"] <- "Apr"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="5月"] <- "May"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="6月"] <- "Jun"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="7月"] <- "Jul"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="8月"] <- "Aug"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="9月"] <- "Sep"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="10月"] <- "Oct"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="11月"] <- "Nov"
levels(temp_year_A$Month)[levels(temp_year_A$Month)=="12月"] <- "Dec"

temp_year_A_plot <- ggplot(temp_year_A, aes(x=Month, y=mean_month_temp, group=Year, fill=Year))+
  geom_line(size=1)+
  geom_point(shape=21, size=3, color="black")+
  #geom_errorbar(aes(ymin=mean_month_temp-se, ymax=mean_month_temp+se))+
  facet_wrap(~Temperature)+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  labs(x="Month", y="Mean temperature (°C)", title = "2019-2022 Mean Month Temperature (Deep A)")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))



temp_year_A_plot


str(temp_B)
temp_B$Date<-as.Date(temp_B$Date)
temp_B$Date<-as.POSIXct(temp_B$Date, format = "%d/%m/%Y")
temp_B$Month = format(temp_B$Date, format = "%b")
temp_B$Year = format(temp_B$Date, format = "%Y")
head(temp_B)

final_B<-merge(temp_B, treatments, by=c("Pond"))
str(final_B)

##Create plots for average monthly temperatures

temp_year_B<-final_B %>%
  #filter(year=="2020")%>%
  group_by(Month, Temperature, Year)%>%
  summarise(mean_month_temp=mean(Mean_temp),
            se=sd(Mean_temp)/sqrt(n()),
            max_day_temp=max(Mean_temp),
            min_day_temp=min(Mean_temp),
            var_day_temp=var(Mean_temp))
head(temp_year_B)

temp_year_B$Month<-factor(temp_year_B$Month, levels = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月"))
temp_year_B$Temperature<-factor(temp_year_B$Temperature, levels = c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))

levels(temp_year_B$Month)[levels(temp_year_B$Month)=="1月"] <- "Jan"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="2月"] <- "Feb"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="3月"] <- "Mar"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="4月"] <- "Apr"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="5月"] <- "May"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="6月"] <- "Jun"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="7月"] <- "Jul"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="8月"] <- "Aug"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="9月"] <- "Sep"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="10月"] <- "Oct"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="11月"] <- "Nov"
levels(temp_year_B$Month)[levels(temp_year_B$Month)=="12月"] <- "Dec"

temp_year_B_plot <- ggplot(temp_year_B, aes(x=Month, y=mean_month_temp, group=Year, fill=Year))+
  geom_line(size=1)+
  geom_point(shape=21, size=3, color="black")+
  #geom_errorbar(aes(ymin=mean_month_temp-se, ymax=mean_month_temp+se))+
  facet_wrap(~Temperature)+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  labs(x="Month", y="Mean temperature (°C)", title = "2019-2022 Mean Month Temperature (Deep B)")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))



temp_year_B_plot


ggarrange(temp_year_A_plot, temp_year_B_plot, ncol = 1, nrow = 2,
          labels = c("A","B"),
          font.label = list(size = 14),common.legend = T,legend = "bottom")


### Create plots for min, max and mean daily temperatures

temp_daily_A<-final_A %>%
  #filter(year=="2020")%>%
  #filter(temp=="Ambient") %>%
  group_by(Month, Temperature, Year)%>%
  summarise(mean_day_temp=mean(Mean_temp),
            se=sd(Mean_temp)/sqrt(n()),
            max_day_temp=max(Mean_temp),
            min_day_temp=min(Mean_temp),
            var_day_temp=var(Mean_temp))
head(temp_daily_A)

final_A$Month<-factor(final_A$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
final_A$Temperature<-factor(final_A$Temperature, levels = c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))



head(final_A)

temp_daily_A_plot <- ggplot(final_A, aes(x=Temperature, y=Mean_temp, fill=Year))+
  geom_boxplot(aes(fill=Year))+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black", 
               position = position_dodge2(width = 0.75), aes(group=Year)) +
  labs(x="Treatment Temperature", y="Mean Daily Temperature (°C)", colour="Year", title = "2019-2022 Mean Daily Temperature (Deep A)")+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))

temp_daily_A_plot

temp_daily_A_max_plot <- ggplot(final_A, aes(x=Temperature, y=Max_temp, fill=Year))+
  geom_boxplot(aes(fill=Year))+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black", 
               position = position_dodge2(width = 0.75), aes(group=Year)) +
  labs(x="Treatment Temperature", y="Max Daily Temperature (°C)", colour="Year", title = "2019-2022 Max Daily Temperature (Deep A)")+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))

temp_daily_A_max_plot

temp_daily_A_min_plot <- ggplot(final_A, aes(x=Temperature, y=Min_temp, fill=Year))+
  geom_boxplot(aes(fill=Year))+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black", 
               position = position_dodge2(width = 0.75), aes(group=Year)) +
  labs(x="Treatment Temperature", y="Min Daily Temperature (°C)", colour="Year", title = "2019-2022 Min Daily Temperature (Deep A)")+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))

temp_daily_A_min_plot


final_B$Month<-factor(final_B$Month, levels = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月"))
final_B$Temperature<-factor(final_B$Temperature, levels = c('CONTROL','1°C','2°C','3°C','4°C','5°C','6°C','7°C','8°C'))

levels(final_B$Month)[levels(final_B$Month)=="1月"] <- "Jan"
levels(final_B$Month)[levels(final_B$Month)=="2月"] <- "Feb"
levels(final_B$Month)[levels(final_B$Month)=="3月"] <- "Mar"
levels(final_B$Month)[levels(final_B$Month)=="4月"] <- "Apr"
levels(final_B$Month)[levels(final_B$Month)=="5月"] <- "May"
levels(final_B$Month)[levels(final_B$Month)=="6月"] <- "Jun"
levels(final_B$Month)[levels(final_B$Month)=="7月"] <- "Jul"
levels(final_B$Month)[levels(final_B$Month)=="8月"] <- "Aug"
levels(final_B$Month)[levels(final_B$Month)=="9月"] <- "Sep"
levels(final_B$Month)[levels(final_B$Month)=="10月"] <- "Oct"
levels(final_B$Month)[levels(final_B$Month)=="11月"] <- "Nov"
levels(final_B$Month)[levels(final_B$Month)=="12月"] <- "Dec"


temp_daily_B_plot <- ggplot(final_B, aes(x=Temperature, y=Mean_temp, fill=Year))+
  geom_boxplot(aes(fill=Year))+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black", 
               position = position_dodge2(width = 0.75), aes(group=Year)) +
  labs(x="Treatment Temperature", y="Mean Daily Temperature (°C)", colour="Year", title = "2019-2022 Mean Daily Temperature (Deep B)")+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))

temp_daily_B_plot

temp_daily_B_max_plot <- ggplot(final_B, aes(x=Temperature, y=Max_temp, fill=Year))+
  geom_boxplot(aes(fill=Year))+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black", 
               position = position_dodge2(width = 0.75), aes(group=Year)) +
  labs(x="Treatment Temperature", y="Max Daily Temperature (°C)", colour="Year", title = "2019-2022 Max Daily Temperature (Deep B)")+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))

temp_daily_B_max_plot

temp_daily_B_min_plot <- ggplot(final_B, aes(x=Temperature, y=Min_temp, fill=Year))+
  geom_boxplot(aes(fill=Year))+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black", 
               position = position_dodge2(width = 0.75), aes(group=Year)) +
  labs(x="Treatment Temperature", y="Min Daily Temperature (°C)", colour="Year", title = "2019-2022 Min Daily Temperature (Deep B)")+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))

temp_daily_B_min_plot

ggarrange(temp_daily_A_plot, temp_daily_B_plot, temp_daily_A_max_plot, temp_daily_B_max_plot, temp_daily_A_min_plot, temp_daily_B_min_plot, ncol = 2, nrow = 3,
          labels = c("A","B","C","D","E","F"),
          font.label = list(size = 14),common.legend = T,legend = "bottom")

#Chemical ponds temperature plots 
#Loading files 
temp_A_treat <- read.delim("F:/Mesocosm Temperature/Mesocosm_temp_daily_DeepA_treat.csv",sep = ',')
temp_B_treat <- read.delim("F:/Mesocosm Temperature/Mesocosm_temp_daily_DeepB_treat.csv",sep = ',')
treatments<-read.csv("F:/Mesocosm Temperature/Treatment_chem.csv",sep = ',')

str(temp_A_treat)
temp_A_treat $Date<-as.Date(temp_A_treat$Date)
temp_A_treat$Date<-as.POSIXct(temp_A_treat$Date, format = "%d/%m/%Y")
temp_A_treat$Month = format(temp_A_treat$Date, format = "%b")
temp_A_treat$Year = format(temp_A_treat$Date, format = "%Y")
head(temp_A_treat)

final_A_treat<-merge(temp_A_treat, treatments, by=c("Pond"))
str(final_A_treat)

str(temp_B_treat)
temp_B_treat $Date<-as.Date(temp_B_treat$Date)
temp_B_treat$Date<-as.POSIXct(temp_B_treat$Date, format = "%d/%m/%Y")
temp_B_treat$Month = format(temp_B_treat$Date, format = "%b")
temp_B_treat$Year = format(temp_B_treat$Date, format = "%Y")
head(temp_B_treat)

final_B_treat<-merge(temp_B_treat, treatments, by=c("Pond"))
str(final_B_treat)

temp_year_A_treat<-final_A_treat %>%
  #filter(year=="2020")%>%
  group_by(Month, Treatment, Year)%>%
  summarise(mean_month_temp=mean(Mean_temp),
            se=sd(Mean_temp)/sqrt(n()),
            max_day_temp=max(Mean_temp),
            min_day_temp=min(Mean_temp),
            var_day_temp=var(Mean_temp))
head(temp_year_A_treat)

temp_year_A_treat$Month<-factor(temp_year_A_treat$Month, levels = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月"))
temp_year_A_treat$Treatment<-factor(temp_year_A_treat$Treatment, levels = c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))

levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="1月"] <- "Jan"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="2月"] <- "Feb"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="3月"] <- "Mar"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="4月"] <- "Apr"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="5月"] <- "May"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="6月"] <- "Jun"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="7月"] <- "Jul"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="8月"] <- "Aug"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="9月"] <- "Sep"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="10月"] <- "Oct"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="11月"] <- "Nov"
levels(temp_year_A_treat$Month)[levels(temp_year_A_treat$Month)=="12月"] <- "Dec"

temp_year_B_treat<-final_B_treat %>%
  #filter(year=="2020")%>%
  group_by(Month, Treatment, Year)%>%
  summarise(mean_month_temp=mean(Mean_temp),
            se=sd(Mean_temp)/sqrt(n()),
            max_day_temp=max(Mean_temp),
            min_day_temp=min(Mean_temp),
            var_day_temp=var(Mean_temp))
head(temp_year_B_treat)

temp_year_B_treat$Month<-factor(temp_year_B_treat$Month, levels = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月"))
temp_year_B_treat$Treatment<-factor(temp_year_B_treat$Treatment, levels = c('CONTROL','4°C','UR','RU','UR+RU','UR+RU+4°C'))

levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="1月"] <- "Jan"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="2月"] <- "Feb"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="3月"] <- "Mar"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="4月"] <- "Apr"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="5月"] <- "May"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="6月"] <- "Jun"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="7月"] <- "Jul"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="8月"] <- "Aug"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="9月"] <- "Sep"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="10月"] <- "Oct"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="11月"] <- "Nov"
levels(temp_year_B_treat$Month)[levels(temp_year_B_treat$Month)=="12月"] <- "Dec"

temp_year_A_treat_plot <- ggplot(temp_year_A_treat, aes(x=Month, y=mean_month_temp, group=Year, fill=Year))+
  geom_line(size=1)+
  geom_point(shape=21, size=3, color="black")+
  #geom_errorbar(aes(ymin=mean_month_temp-se, ymax=mean_month_temp+se))+
  facet_wrap(~Treatment)+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  labs(x="Month", y="Mean temperature (°C)", title = "2019-2022 Mean Month Temperature (Deep A)")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))



temp_year_A_treat_plot

temp_year_B_treat_plot <- ggplot(temp_year_B_treat, aes(x=Month, y=mean_month_temp, group=Year, fill=Year))+
  geom_line(size=1)+
  geom_point(shape=21, size=3, color="black")+
  #geom_errorbar(aes(ymin=mean_month_temp-se, ymax=mean_month_temp+se))+
  facet_wrap(~Treatment)+
  scale_fill_viridis_d(begin=0, end=1, name="Year")+
  labs(x="Month", y="Mean temperature (°C)", title = "2019-2022 Mean Month Temperature (Deep B)")+
  theme_bw()+
  theme(strip.text.x = element_text(colour = "white")) +
  theme(
    strip.background = element_rect(
      fill = "black"),
  )+
  theme(axis.text.x=element_text( angle = 45, hjust = 1, vjust = 1))



temp_year_B_treat_plot

ggarrange(temp_year_A_treat_plot, temp_year_B_treat_plot, ncol = 1, nrow = 2,
          labels = c("A","B"),
          font.label = list(size = 14),common.legend = T,legend = "bottom")
