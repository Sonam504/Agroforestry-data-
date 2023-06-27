library(tidyverse)
library(multcompView)
library(dplyr)
library(ggplot2)
library(ggthemes)

setwd("C:/SONAM GAKI/Semester III/Junior Research Lab Documents/AF data")

mydata <-agroforestry_rawdata

levels(mydata$genotype)

kwTest <- kruskal.test(mydata)
print(kwTest)
#significant difference 

# Report: H = 5233 (11) p= 0.001

kruskal.test(tiller ~ genotype, data = mydata) 
#significant # Report: H = 75.496 (45) p= 0.001

pairwise.wilcox.test(mydata$tiller, mydata$genotype,
                     p.adjust.method = "BH")

kruskal.test(tiller ~ system, data = mydata)
#no significance 

kruskal.test(head_length ~ genotype, data = mydata) 
#significant

pairwise.wilcox.test(mydata$head_length, mydata$genotype,
                     p.adjust.method = "BH")



ggplot(mydata, aes(x= system, y=tiller))+
  geom_boxplot(outlier.shape= NA)+
  theme_classic()

ggplot(mydata, aes(x= genotype, y=tiller))+
  geom_jitter(outlier.shape= NA)+
  theme_classic()

ggplot(mydata, aes(x= system, y= head_length))+
  geom_boxplot(outlier.shape= NA)+
  theme_classic()

ggplot(mydata, aes(x= genotype, y= head_length))+
  geom_jitter(outlier.shape= NA)+
  theme_classic()

ggplot(mydata, aes(x= genotype, y= head_length))+
  geom_jitter(outlier.shape= NA)+
  theme_classic()

# Tiller numbergraphical representation

# Boxplot

ggplot(mydata, aes(x= system, y= tiller, col= system))+
  geom_boxplot(outlier.shape= NA)+
  scale_y_continuous(breaks= seq(0, 8, by=1 ), labels= seq(0,8, by= 1))+
  xlab("System") +
  ylab("Tiller number")+
  theme_classic()

# Jitter

ggplot(mydata, aes(x= system, y= tiller, shape= system, col= system))+
  geom_jitter(outlier.shape= NA)+
  scale_color_manual(values= c("black", "red"))+
  scale_y_continuous(breaks= seq(0, 8, by=1 ), labels= seq(0,8, by= 1))+
  xlab("System") +
  ylab("Tiller number")+
  theme_classic()


# Head length graphical presentation 

# Boxplot

ggplot(mydata, aes(x= system, y= head_length))+
  geom_boxplot(outlier.shape= NA)+
  scale_color_manual(values= c("black", "red"))+
  scale_y_continuous(breaks= seq(0, 25, by=2 ), labels= seq(0,25, by= 2))+
  xlab("System") +
  ylab("Head length (cm)")+
  theme_classic()

# Jitter

ggplot(mydata, aes(x= system, y= head_length, shape= system, col=system))+
  geom_jitter(outlier.shape= NA)+
  scale_color_manual(values= c("black", "red"))+
  scale_y_continuous(breaks= seq(0, 25, by=2 ), labels= seq(0,25, by= 2))+
  xlab("System") +
  ylab("Head length (cm)")+
  theme_classic()