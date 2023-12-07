rm(list=ls(all=TRUE))
Sys.setenv(TZ='GMT')
setwd("D:/GitHub/Sargassum_H2S_emission")

#Libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(ggbiplot)
library(tidyr) # to convert from wide to long format
library(ggpubr)
library(stats)
library(devtools)
library(Rmisc)

#Temporal analysis
All= read.csv("H2S-All.csv", sep = ",")
str(All)

#Transformations
All$Date= as.Date(All$Date, format = "%d/%m/%Y", tz = "GMT")
All$Month_Year=factor(All$Month_Year,levels = c('2022_Jul', '2022_Aug', '2022_Sep', '2023_Feb','2023_Mar',
                                                '2023_Apr', '2023_May', '2023_Jun', '2023_Jul', 
                                                '2023_Aug',"2023_Sep"))

All$Variable=as.factor(All$Variable)
All$Category=as.factor (All$Category)

TableAll<- ddply(All, .(Variable), summarise, 
                 N    = length(Value),
                 Min = round(min(Value), 1),
                 Max = round(max(Value), 1),
                 Median=round(median(Value), 1),
                 Q1 = quantile(Value, 0.25, na.rm = T),
                 Q3 = quantile(Value, 0.75, na.rm = T),
                 Mean = round (mean(Value), 1),
                 sd   =round (sd(Value), 1),
                 se   = (sd(Value) / sqrt(length(Value))))

TableAll


#By month
TableAll2<- ddply(All, .(Variable, Month_Year), summarise, 
                  N    = length(Value),
                  Min = round(min(Value), 1),
                  Max = round(max(Value), 1),
                  Median = round (median(Value), 1),
                  Q1 = quantile(Value, 0.25, na.rm = T),
                  Q3 = quantile(Value, 0.75, na.rm = T),
                   Mean = round (mean(Value), 1),
                  sd   =round (sd(Value), 1),
                  se   = (sd(Value) / sqrt(length(Value))))

TableAll2


#Plots - temporal analysis

#Supplementary Figure 1
EnvVar<- All [All$Variable=="Temp"|All$Variable=="WD"|All$Variable=="WS"|
                All$Variable=="SarVol",]

EnvVar$Variable=factor(EnvVar$Variable,levels = c("SarVol","Temp","WD","WS"))

ggboxplot(EnvVar, x = "Month_Year", y = "Value", color = "Month_Year",
          palette="set1", size=0.5)+ 
  labs(x="Year/Month")+
  facet_wrap(~Variable, scales = 'free_y')+
  theme_grey(base_size=11) + theme(axis.text.x = element_text(angle=90), legend.position = "none") 

#Figure 2 - H2S concentrations at different distances from the shore
H2S<- All [All$Variable=="X0m_digging"|All$Variable=="X0m_0m"|All$Variable=="X0m_1.5m"
           |All$Variable=="X10m_0m"|All$Variable== "X10m_1.5m"|All$Variable== "X30m",]

H2S$Variable=factor(H2S$Variable,levels = c("X0m_digging","X0m_0m","X10m_0m",
                                            "X0m_1.5m","X10m_1.5m","X30m"))

ggboxplot(H2S, x = "Month_Year", y = "Value", color = "Month_Year",
          palette="set1", size=0.5)+ 
  labs(x="Year/Month")+
  facet_wrap(~Variable, scales = 'free_y')+
  theme_grey(base_size=11) + theme(axis.text.x = element_text(angle=90), legend.position = "none") 


# Spatial Analysis
setwd("D:/Articulos/En proceso/001 - Artículo H2S/Para R")
H2STran= read.csv("H2STran.csv", sep = ",")

str(H2STran)

#Transformations
H2STran$Date= as.Date(H2STran$Date, format = "%d/%m/%Y", tz = "GMT")
H2STran$Condition=as.factor(H2STran$Condition)
H2STran$Humidity=as.factor(H2STran$Humidity)
H2STran$Trans=as.factor(H2STran$Trans)

#Convert to long format
require(reshape2)
Tranlong<-melt(H2STran, id = c("Date", "Time", "Trans", "Sample", "Condition", "Humidity","Rain", "Obs"))
str (Tranlong)
summary (Tranlong)

#Tables
Table1<- ddply(Tranlong, .(variable), summarise, 
               N    = length(value),
               Min = round(min(value), 1),
               Max = round(max(value), 1),
               Mean = round (mean(value), 1),
               Median = round (median(value), 1),
               Q1 = quantile(value, 0.25, na.rm = T),
               Q3 = quantile(value, 0.75, na.rm = T),
               sd   =round (sd(value), 1),
               se   = (sd(value) / sqrt(length(value))))

Table1

#Figure 3 PCA
dat= read.csv("PCA_data_tran.csv", header =T, sep = ",")
dat
nombres_variables <- colnames(dat)
dat.pca=prcomp(dat[,1:7],scale.=TRUE)

ggbiplot(dat.pca,obs.scale = 1, 
         var.scale=1,groups=dat$nombres_variables,ellipse=TRUE,circle=TRUE)
dat2=dat[,1:7]
mean=apply(dat2,2,mean)
sd=apply(dat2,2,mean)
for(i in 1:ncol(dat2)){
  dat2[,i]=(dat2[,i]-mean[i])/sd[i]
}
dat2.pca=prcomp(dat2,scale.=TRUE)
ggbiplot(dat2.pca,obs.scale=1, var.scale=1,groups=dat$nombres_variables, ellipse=TRUE,circle=TRUE)+
  geom_point(aes(color = dat$H2S, size = dat$H2S), alpha = 0.5)+
  scale_size(range=c(0,15),
             breaks=c(0,5,10,25,50,75),
             labels=c("0","5","10","25","50","75+"),
             name = "H2S (ppm)",
             guide="legend")+
            xlim(-4, 5)+
    scale_color_gradient(low = "lightblue", high = "darkblue",
                       name = "H2S (ppm)", breaks=c(0,75),labels=c("0", "75+"))

#Correlations
corr <- cor.test(x=H2STran$TempBelow, y=H2STran$H2S, method = 'spearman')
corr

corr <- cor.test(x=H2STran$Height, y=H2STran$H2S, method = 'spearman')
corr

corr <- cor.test(x=H2STran$ws, y=H2STran$H2S, method = 'spearman')
corr

corr <- cor.test(x=H2STran$Width, y=H2STran$H2S, method = 'spearman')
corr
  
corr <- cor.test(x=H2STran$wdSAMMO, y=H2STran$H2S, method = 'spearman')
corr

corr <- cor.test(x=H2STran$TemEnv, y=H2STran$H2S, method = 'spearman')
corr
