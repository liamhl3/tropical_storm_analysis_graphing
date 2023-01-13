library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
#library(plotly)
library(corrgram)
library(corrplot)
library(sp)
library(sf)
library(leaflet)
library(tidyverse)
library(stringr)
library(here)
library(widgetframe)
library(htmltools)
library(htmlwidgets)
library(tmap)
library(rgdal)
library(caret)
library(ggpubr)
library(car)
library(PerformanceAnalytics)
library(ggcorrplot)
library(naniar)
library(grid)
library(mice)
library(egg)
library(shiny)
library(lubridate)
library(devtools)
library(ncaahoopR)
library(scales)

#load data
boone.precip <- read_csv('Boone_daily_precip_1980-present_HUC_050500010201_dayMet_split-dates-columns.csv')
greensboro.precip <- read_csv('Greensboro_daily_precip_1980-present_HUC_030300020105_dayMet_split-dates-columns.csv')
greenville.precip <- read_csv('Greenville_daily_precip_1980-present_HUC_030201030403_dayMet_split-dates-columns.csv')

#change Precipiration column name
boone.precip <- boone.precip %>%
  rename(precip = `Area_Weighted_Mean_Precipitation_(mm per day)`)
greensboro.precip <- greensboro.precip %>%
  rename(precip = `Area_Weighted_Mean_Precipitation_mm_per_day`)
greenville.precip <- greenville.precip %>%
  rename(precip = `Area Weighted Mean Precipitation (mm per day)`)

#data summary
summary(boone.precip)
summary(greensboro.precip)
summary(greenville.precip)

#histograms of precipipation
ggplot(boone.precip, aes(x=precip))+
  geom_histogram(binwidth = 1)
ggplot(greensboro.precip, aes(x=precip))+
  geom_histogram(binwidth = 1)
ggplot(greenville.precip, aes(x=precip))+
  geom_histogram(binwidth = 1)

#add site names
boone.precip1 <- boone.precip%>%
  mutate(Site= "Boone")
greensboro.precip1 <- greensboro.precip%>%
  mutate(Site= "Greenboro")
greenville.precip1 <- greenville.precip%>%
  mutate(Site= "Greenville")

#Join all sites
sites.total <- boone.precip1 %>% 
  full_join(greensboro.precip1) %>% 
  full_join(greenville.precip1)

#view outliers in a boxplot
ggplot(sites.total, aes(x=Site, y=precip))+
  geom_boxplot()

#see the 98th percentile precipitation cutoff value
upper.bound.boone <- quantile(boone.precip1$precip, 0.98, na.rm = TRUE)
upper.bound.boone
upper.bound.greensboro <- quantile(greensboro.precip1$precip, 0.98, na.rm = TRUE)
upper.bound.greensboro
upper.bound.greenville <- quantile(greenville.precip1$precip, 0.98, na.rm = TRUE)
upper.bound.greenville

#see how many precipitation values are in the 98th percentile
outlier.98.boone <- which(boone.precip1$precip > upper.bound.boone)
boone.precip1[outlier.98.boone, "precip"]
outlier.98.greensboro <- which(greensboro.precip1$precip > upper.bound.greensboro)
greensboro.precip1[outlier.98.greensboro, "precip"]
outlier.98.greenville <- which(greenville.precip1$precip > upper.bound.greenville)
greenville.precip1[outlier.98.greenville, "precip"]

#create a df of 98th percentile full values 
boone.98.percent.events <- boone.precip1[outlier.98.boone, ]
greensboro.98.percent.events <- greensboro.precip1[outlier.98.greensboro, ]
greenville.98.percent.events <- greenville.precip1[outlier.98.greenville, ]

#see the 99th percentile precipitation cutoff value
upper.bound.boone2 <- quantile(boone.precip1$precip, 0.99, na.rm = TRUE)
upper.bound.boone2
upper.bound.greensboro2 <- quantile(greensboro.precip1$precip, 0.99, na.rm = TRUE)
upper.bound.greensboro2
upper.bound.greenville2 <- quantile(greenville.precip1$precip, 0.99, na.rm = TRUE)
upper.bound.greenville2

#see how many precipitation values are in the 98th percentile
outlier.99.boone <- which(boone.precip1$precip > upper.bound.boone2)
boone.precip1[outlier.99.boone, "precip"]
outlier.99.greensboro <- which(greensboro.precip1$precip > upper.bound.greensboro2)
greensboro.precip1[outlier.99.greensboro, "precip"]
outlier.99.greenville <- which(greenville.precip1$precip > upper.bound.greenville2)
greenville.precip1[outlier.99.greenville, "precip"]

#create a df of 99th percentile full values
boone.99.percent.events <- boone.precip1[outlier.99.boone, ]
greensboro.99.percent.events <- greensboro.precip1[outlier.99.greensboro, ]
greenville.99.percent.events <- greenville.precip1[outlier.99.greenville, ]

#see the 99.5 percentile precipitation cutoff value
upper.bound.boone3 <- quantile(boone.precip1$precip, 0.995, na.rm = TRUE)
upper.bound.boone3
upper.bound.greensboro3 <- quantile(greensboro.precip1$precip, 0.995, na.rm = TRUE)
upper.bound.greensboro3
upper.bound.greenville3 <- quantile(greenville.precip1$precip, 0.995, na.rm = TRUE)
upper.bound.greenville3

#see how many precipitation values are in the 99.5 percentile
outlier.995.boone <- which(boone.precip1$precip > upper.bound.boone3)
boone.precip1[outlier.995.boone, "precip"]
outlier.995.greensboro <- which(greensboro.precip1$precip > upper.bound.greensboro3)
greensboro.precip1[outlier.995.greensboro, "precip"]
outlier.995.greenville <- which(greenville.precip1$precip > upper.bound.greenville3)
greenville.precip1[outlier.995.greenville, "precip"]

#create a df of 99.5 percentile full values
boone.995.percent.events <- boone.precip1[outlier.995.boone, ]
greensboro.995.percent.events <- greensboro.precip1[outlier.995.greensboro, ]
greenville.995.percent.events <- greenville.precip1[outlier.995.greenville, ]


#add columns for hurricane season
boone.995.events.data <- boone.995.percent.events %>%
  mutate(after_2000=year>date_threshold) %>% 
  mutate(in_hurricane_season = (month>6 & month<11 )) %>% 
  mutate(in_hurricane_season_summer = (month>6 & month<9 )) %>% 
  mutate(in_hurricane_season_fall = (month>8 & month<11 ))

#export to view data in excel
write_csv(boone.995.events.data, 'boone_data_test.csv')

#add a date threshold
date_threshold1 <- 2000

#boone thresholds
boone_98_thresh <- 33
boone_99_thresh <- 45
boone_99.5_thresh <- 57

#greensboro thresholds
greensboro_98_thresh <- 26
greensboro_99_thresh <- 33
greensboro_99.5_thresh <- 41

#greenville thresholds
greenville_98_thresh <- 30
greenville_99_thresh <- 39
greenville_99.5_thresh <- 49

#mutate data
boone_data<- boone.precip1 %>% 
  mutate(after_2000=year>date_threshold1) %>%
  mutate(after_2010=year>date_threshold2)%>%
  mutate(after_2015=year>date_threshold3)%>%
  mutate(above_98_threshold=`precip` > boone_98_thresh) %>% 
  mutate(above_99_threshold=`precip` > boone_99_thresh) %>%
  mutate(above_995_threshold=`precip` > boone_99.5_thresh) %>%
  mutate(in_hurricane_season = (month>6 & month<11 )) %>% 
  mutate(in_hurricane_season_summer = (month>6 & month<9 )) %>% 
  mutate(in_hurricane_season_fall = (month>8 & month<11 ))

greensboro_data<- greensboro.precip1 %>% 
  mutate(after_2000=year>date_threshold1) %>%
  mutate(after_2010=year>date_threshold2)%>%
  mutate(after_2015=year>date_threshold3)%>%
  mutate(above_98_threshold=`precip` > greensboro_98_thresh) %>% 
  mutate(above_99_threshold=`precip` > greensboro_99_thresh) %>%
  mutate(above_995_threshold=`precip` > greensboro_99.5_thresh) %>%
  mutate(in_hurricane_season = (month>6 & month<11 )) %>% 
  mutate(in_hurricane_season_summer = (month>6 & month<9 )) %>% 
  mutate(in_hurricane_season_fall = (month>8 & month<11 )) 

greenville_data<- greenville.precip1 %>% 
  mutate(after_2000=year>date_threshold1) %>%
  mutate(after_2010=year>date_threshold2)%>%
  mutate(after_2015=year>date_threshold3)%>%
  mutate(above_98_threshold=`precip` > greenville_98_thresh) %>% 
  mutate(above_99_threshold=`precip` > greenville_99_thresh) %>%
  mutate(above_995_threshold=`precip` > greenville_99.5_thresh) %>%
  mutate(in_hurricane_season = (month>6 & month<11 )) %>% 
  mutate(in_hurricane_season_summer = (month>6 & month<9 )) %>% 
  mutate(in_hurricane_season_fall = (month>8 & month<11 ))

##########################################################################

#pivot test
boone_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold, na.rm = T), #I used this in class
    percent_high_event=sum(above_995_threshold, na.rm = T)/n() # note here the n()! That takes a count of the members of the group
  ) 

#graphing

boone.99.2000 <-boone_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 1% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 1% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.99.2000

boone.99.2000.fall <-boone_data %>% 
  group_by(after_2000,in_hurricane_season_fall) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_fall)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 1% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 1% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.99.2000.fall

boone.99.2000.summer <-boone_data %>% 
  group_by(after_2000,in_hurricane_season_summer) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_summer)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 1% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 1% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.99.2000.summer

boone.98.2000 <-boone_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 2% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 2% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.98.2000

boone.98.2000.fall <-boone_data %>% 
  group_by(after_2000,in_hurricane_season_fall) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_fall)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 2% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 2% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.98.2000.fall

boone.98.2000.summer <-boone_data %>% 
  group_by(after_2000,in_hurricane_season_summer) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_summer)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 2% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 2% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.98.2000.summer


##########################################################################
#greensboro


greensboro.99.2000 <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 1% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 1% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.99.2000

greensboro.99.2000.fall <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season_fall) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_fall)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 1% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 1% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.99.2000.fall

greensboro.99.2000.summer <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season_summer) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_summer)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 1% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 1% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.99.2000.summer

greensboro.98.2000 <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 2% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 2% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.98.2000

greensboro.98.2000.fall <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season_fall) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_fall)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 2% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 2% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.98.2000.fall

greensboro.98.2000.summer <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season_summer) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_summer)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 2% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 2% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.98.2000.summer


#######################################################################
##
greenville.99.2000 <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 1% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 1% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.99.2000

greenville.99.2000.fall <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season_fall) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_fall)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 1% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 1% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.99.2000.fall

greenville.99.2000.summer <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season_summer) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_summer)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 1% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 1% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.99.2000.summer

greenville.98.2000 <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 2% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 2% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.98.2000

greenville.98.2000.fall <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season_fall) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_fall)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 2% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 2% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.98.2000.fall

greenville.98.2000.summer <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season_summer) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_summer)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 2% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 2% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.98.2000.summer

#########################################################################
#Create combine plots
jpeg("sites_combined.jpeg", units="in", width=25, height=15, res=300)
ggarrange(boone.995.2000, boone.995.2000.fall,boone.995.2000.summer, 
          greensboro.995.2000, greensboro.995.2000.fall,greensboro.995.2000.summer,
          greenville.995.2000, greenville.995.2000.fall,greenville.995.2000.summer,
          labels = c("a)", "b)", "c)",
                     "d)", "e)", "f)",
                     "g)", "h)", "i)"),
          ncol=3, nrow=3)
dev.off()

jpeg("sites_combined.99.jpeg", units="in", width=25, height=15, res=300)
ggarrange(boone.99.2000, boone.99.2000.fall,boone.99.2000.summer, 
          greensboro.99.2000, greensboro.99.2000.fall,greensboro.99.2000.summer,
          greenville.99.2000, greenville.99.2000.fall,greenville.99.2000.summer,
          labels = c("a)", "b)", "c)",
                     "d)", "e)", "f)",
                     "g)", "h)", "i)"),
          ncol=3, nrow=3)
dev.off()

jpeg("sites_combined.98.jpeg", units="in", width=25, height=15, res=300)
ggarrange(boone.98.2000, boone.98.2000.fall,boone.98.2000.summer, 
          greensboro.98.2000, greensboro.98.2000.fall,greensboro.98.2000.summer,
          greenville.98.2000, greenville.98.2000.fall,greenville.98.2000.summer,
          labels = c("a)", "b)", "c)",
                     "d)", "e)", "f)",
                     "g)", "h)", "i)"),
          ncol=3, nrow=3)
dev.off()
































































#data I did not use
##################################################################
#create a df of 98th percentile full values 
boone.98.percent.events <- boone.precip1[outlier.98.boone, ]
greensboro.98.percent.events <- greensboro.precip1[outlier.98.greensboro, ]
greenville.98.percent.events <- greenville.precip1[outlier.98.greenville, ]

#create a df of 99.5 percentile full values
boone.995.percent.events <- boone.precip1[outlier.995.boone, ]
greensboro.995.percent.events <- greensboro.precip1[outlier.995.greensboro, ]
greenville.995.percent.events <- greenville.precip1[outlier.995.greenville, ]

#create a df of 99th percentile full values
boone.99.percent.events <- boone.precip1[outlier.99.boone, ]
greensboro.99.percent.events <- greensboro.precip1[outlier.99.greensboro, ]
greenville.99.percent.events <- greenville.precip1[outlier.99.greenville, ]

####################################################################
#0.5% Precipitation Events

boone.995.2000 <-boone_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 0.5% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 0.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.995.2000

boone.995.2000.fall <-boone_data %>% 
  group_by(after_2000,in_hurricane_season_fall) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_fall)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 0.5% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 0.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.995.2000.fall

boone.995.2000.summer <-boone_data %>% 
  group_by(after_2000,in_hurricane_season_summer) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_summer)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 0.5% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 0.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.995.2000.summer

greensboro.995.2000 <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 0.5% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 0.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.995.2000


greensboro.995.2000.fall <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season_fall) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_fall)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 0.5% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 0.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.995.2000.fall

greensboro.995.2000.summer <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season_summer) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_summer)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 0.5% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 0.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.995.2000.summer

greenville.995.2000 <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 0.5% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 0.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.995.2000

greenville.995.2000.fall <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season_fall) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_fall)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 0.5% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 0.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.995.2000.fall

greenville.995.2000.summer <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season_summer) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=after_2000,y=percent_high_event,fill=in_hurricane_season_summer)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 0.5% Precipitation Events')+
  xlab('After 2000')+
  ylab('Chance of a 0.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.995.2000.summer







####################
#Time Data 

greenville.995.2010 <-greenville_data %>% 
  group_by(after_2010,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2010)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 99.5% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.995.2010

greenville.99.2010 <-greenville_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2000)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 99% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.99.2010

greenville.98.2010 <-greenville_data %>% 
  group_by(after_2010,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2010)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 98% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 98% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.98.2010

greenville.995.2015 <-greenville_data %>% 
  group_by(after_2015,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2015)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 99.5% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.995.2015

greenville.99.2015 <-greenville_data %>% 
  group_by(after_2015,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2015)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 99% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.99.2015

greenville.98.2015 <-greenville_data %>% 
  group_by(after_2015,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2015)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E09F3E', '#7209B7'))+
  ggtitle('Greenville 98% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 98% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greenville.98.2015

greensboro.995.2010 <-greensboro_data %>% 
  group_by(after_2010,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2010)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 99.5% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.995.2010

greensboro.99.2010 <-greensboro_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2000)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 99% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.99.2010

greensboro.98.2010 <-greensboro_data %>% 
  group_by(after_2010,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2010)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 98% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 98% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.98.2010

greensboro.995.2015 <-greensboro_data %>% 
  group_by(after_2015,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2015)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 99.5% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.995.2015

greensboro.99.2015 <-greensboro_data %>% 
  group_by(after_2015,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2015)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 99% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.99.2015

greensboro.98.2015 <-greensboro_data %>% 
  group_by(after_2015,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2015)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#FEE440', '#00BBF9'))+
  ggtitle('Greensboro 98% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 98% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
greensboro.98.2015

boone.995.2010 <-boone_data %>% 
  group_by(after_2010,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2010)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#999999', '#E69F00', '#999999'))+
  ggtitle('Boone 99.5% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.995.2010

boone.99.2010 <-boone_data %>% 
  group_by(after_2000,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2000)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 99% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.99.2010

boone.98.2010 <-boone_data %>% 
  group_by(after_2010,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2010)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 98% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 98% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.98.2010

boone.995.2015 <-boone_data %>% 
  group_by(after_2015,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_995_threshold,na.rm = T), 
    percent_high_event=sum(above_995_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2015)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 99.5% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99.5% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.995.2015

boone.99.2015 <-boone_data %>% 
  group_by(after_2015,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_99_threshold,na.rm = T), 
    percent_high_event=sum(above_99_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2015)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 99% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 99% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.99.2015

boone.98.2015 <-boone_data %>% 
  group_by(after_2015,in_hurricane_season) %>% 
  summarise(
    high_event_total=sum(above_98_threshold,na.rm = T), 
    percent_high_event=sum(above_98_threshold,na.rm = T)/n() 
  ) %>% 
  ggplot(aes(x=in_hurricane_season,y=percent_high_event,fill=after_2015)) +
  geom_bar(stat="identity", color = "black", position=position_dodge())+
  scale_fill_manual(values = c('#E69F00', '#999999'))+
  ggtitle('Boone 98% Precipitation Events')+
  xlab('In Hurricane Season')+
  ylab('Chance of a 98% Precipitation Event')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
boone.98.2015
