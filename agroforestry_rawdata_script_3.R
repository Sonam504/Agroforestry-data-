library(tidyverse)
library(multcompView)
library(dplyr)
library(ggplot2)
library(ggthemes)

setwd("C:/SONAM GAKI/Semester III/Junior Research Lab Documents/AF data")

mydata <-agroforestry_rawdata


# HEIGHT

# Perform two-way ANOVA [height]
model <- aov(height ~ system + genotype + system:genotype, data = mydata)
summary(model)

# System 1   7492    7492  87.448  < 2e-16 *** [p value: 2 to the power of -16 = small i.e., >0.05]
# There is a significant difference between the two systems, significant level (‘***’ 0.001)
# Factor(genotype) 45  15184     337   3.938 3.33e-14 *** 
# [p value: 3.33 to the power of -14 = small i.e., >0.05]
# There is a significant difference between the genotypes (‘***’ 0.001)
# system:factor(genotype)  43  14352     334   3.896 1.49e-13 ***
# There is a significant difference in the interaction between system and genotype (‘***’ 0.001)



# Check normality assumption
residuals <- resid(model)

# Shapiro-Wilk test
shapiro.test(residuals)

# Perform Tukey test
tukey_results <- TukeyHSD(model)

# Print the Tukey test results
print(tukey_results)

#Test of the interaction

model1 <- lm(height ~ system + genotype + system*genotype, data=mydata)
model2 <- lm(height ~ system + genotype, data=mydata)
anova(model1)

#Analysis of Variance Table

anova(model1,model2)

# model1 not significant

drop1(model1,.~.,test="F")

model3 <- lm(height ~ system + genotype, data=mydata)
model4 <- lm(height ~ system, data=mydata)
model5 <- lm(height ~ genotype, data=mydata)
anova(model3,model4)

#Analysis of Variance Table

anova(model3,model5)

drop1(model3,.~.,test="F")

anova(model3)

# Modelisation of the problem: Graphical representation

library(cowplot)

plot_grid(nrow = 2, ncol = 2,
          ggplot(mydata, aes(y = height, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE)+
            xlab("System") +
            ylab("Height (cm)")+
            theme_classic(),
          ggplot(mydata, aes(y = neck_height, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE)+
            xlab("System") +
            ylab("Vegetative height (cm)")+
            theme_classic(),
          ggplot(mydata, aes(y = head_length, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE)+
            xlab("System") +
            ylab("Head length (cm)")+
            theme_classic(), 
          ggplot(mydata, aes(y = head_length, x = neck_height)) + 
            geom_point() +
            xlab("Vegetative height (cm)") +
            ylab("Head length (cm)")+
            theme_classic()


# Change axis range [Revisit]

plot_grid(nrow = 2, ncol = 2,
          ggplot(mydata, aes(y = height, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE)+
            scale_y_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))+
            xlab("System") +
            ylab("Height (cm)")+
            theme_classic(),
          ggplot(mydata, aes(y = neck_height, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE)+
            scale_y_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))+
            xlab("System") +
            ylab("Vegetative height (cm)")+
            theme_classic(),
          ggplot(mydata, aes(y = head_length, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE)+
            xlab("System") +
            ylab("Head length (cm)")+
            theme_classic(), 
          ggplot(mydata, aes(y = head_length, x = neck_height)) + 
            geom_point() +
            geom_smooth(se=F, method = "lm", size= 0.5)+
            xlab("Vegetative height (cm)") +
            ylab("Head length (cm)")+
            theme_classic()


 # Separate graphs with          
          
  ggplot(mydata, aes(y = height, x = system, fill = system)) +
    geom_boxplot(show.legend = FALSE)+
    scale_y_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))+
    xlab("System") +
    ylab("Height (cm)")+
    theme_classic()
  
  ggplot(mydata, aes(y = neck_height, x = system, fill = system)) +
    geom_boxplot(show.legend = FALSE)+
    scale_y_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))+
    xlab("System") +
    ylab("Vegetative height (cm)")+
    theme_classic()
  
  ggplot(mydata, aes(y = head_length, x = system, fill = system)) +
    geom_boxplot(show.legend = FALSE)+
    xlab("System") +
    ylab("Head length (cm)")+
    theme_classic()
  
 ggplot(mydata, aes(y = head_length, x = neck_height)) + 
    geom_point() +
    geom_smooth(se=F, method = "lm", size= 0.5)+
    xlab("Vegetative height (cm)") +
    ylab("Head length (cm)")+
    theme_classic()        
          

# No fill = system [classic black and white format]

plot_grid(nrow = 2, ncol = 2,
          
          ggplot(mydata, aes(y = height, x = system, fill = system)) +
            geom_boxplot(outlier.shape= NA, show.legend = FALSE)+
            scale_y_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))+
            xlab("System") +
            ylab("Height (cm)")+
            theme_classic()
          
          ggplot(mydata, aes(y = neck_height, x = system, fill= system)) +
            geom_boxplot(outlier.shape= NA, show.legend = FALSE)+
            scale_y_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))+
            xlab("System") +
            ylab("Vegetative height (cm)")+
            theme_classic()
          
          ggplot(mydata, aes(y = head_length, x = system,fill= system )) +
            geom_boxplot(outlier.shape= NA, show.legend = FALSE)+
            xlab("System") +
            ylab("Head length (cm)")+
            theme_classic()
          
          ggplot(mydata, aes(y = head_length, x = neck_height)) + 
            geom_point(outlier.shape= NA, size = 1, Alpha= 0.85) +
            xlab("Vegetative height (cm)") +
            ylab("Head length (cm)")+
            theme_classic()
            

# Regression head_length and vegetative height 

ggplot(mydata, aes(y = head_length, x = neck_height)) + 
  geom_point(size = 1, Alpha= 0.85) +
  xlab("Vegetative height (cm)") +
  ylab("Head length (cm)")+
  theme_classic())


# Two continuous variable ## [Not going to use]

mydata %>%
  ggplot(aes(x=neck_height, y=head_length, col = system))+
  geom_point(alpha= 0.5) +
  geom_smooth(se=F)+
  theme_classic()+
  xlab("Vegetative height (cm)") +
  ylab("Head length (cm)")


# Linear regression #

ggplot(mydata,aes(x = neck_height, y = head_length, shape= system, col=system))+
  geom_point()+
  geom_smooth(se=F, method = "lm", size= 1)+
  theme_classic()+
  scale_x_continuous(limits = c(NA, 80)) +
  scale_y_continuous(limits = c(NA, 30))+
  xlab("Vegetative height (cm)") +
  ylab("Head length (cm)")

## several sub-graphs ##

ggplot(mydata,aes(x = neck_height, y = head_length, col=system))+
  geom_point(size = 1)+
  geom_smooth(se=F, method = "lm", size= 0.5)+
  facet_wrap(~ system, ncol = 3)+
  theme_classic()+
  scale_x_continuous(limits = c(NA, 100)) +
  scale_y_continuous(limits = c(NA, 30))+
  xlab("Vegetative height (cm)") +
  ylab("Head length (cm)")


## in order to have different horizontal scale##


ggplot(mydata,aes(x = neck_height, y = head_length, shape= system, col=system))+
  geom_point(outlier.shape= NA)+
  geom_smooth(se=F, method = "lm", size=0.5)+
  facet_wrap(~ system, ncol = 3)+
  scale_color_manual(values= c("black", "red"))+
  scale_x_continuous(breaks= seq(10, 100, by= 10), labels= seq(10, 100, by= 10))+
  scale_y_continuous(breaks= seq(0, 26, by=2 ), labels= seq(0,26, by= 2))+
  xlab("Vegetative height (cm)") +
  ylab("Head length (cm)")+
  theme_classic()

# without facet_wrap

ggplot(mydata,aes(x = neck_height, y = head_length, shape= system, col=system))+
  geom_point(outlier.shape= NA)+
  geom_smooth(se=F, method = "lm", size=0.5)+
  scale_color_manual(values= c("#F8766D", "#00BFC4"))+
  scale_x_continuous(breaks= seq(10, 100, by= 10), labels= seq(10, 100, by= 10))+
  scale_y_continuous(breaks= seq(0, 26, by=2 ), labels= seq(0,26, by= 2))+
  xlab("Vegetative height (cm)") +
  ylab("Head length (cm)")+
  theme_classic()

ggplot(mydata, aes(x= system, y= tiller, fill= system))+
  geom_boxplot(outlier.shape= NA)+
  scale_y_continuous(breaks= seq(0, 8, by=1 ), labels= seq(0,8, by= 1))+
  xlab("System") +
  ylab("Tiller number")+
  theme_classic()

# Classice black and white display 

ggplot(mydata,aes(x = neck_height, y = head_length, shape= system, col=system))+
  geom_point(outlier.shape= NA)+
  scale_x_continuous(breaks= seq(10, 100, by= 10), labels= seq(10, 100, by= 10))+
  scale_y_continuous(breaks= seq(1, 25, by= 2), labels= seq(1,25, by= 2))+
  geom_smooth(se=F, method = "lm", linetype = 6)+
  scale_color_manual(values= c("black", "black"))+
  facet_wrap(~ system, ncol = 3, scale ="free")+
  theme_classic()+
  xlab("Vegetative height (cm)") +
  ylab("Head length (cm)")


# Perform simple linear regression for head length and vegetative height 

model <- lm(neck_height ~ head_length, data = mydata)

# Print the model summary
summary(model)


# Perform simple linear regression for head length and vegetative height [2]

model1 <- lm(head_length ~ neck_height, data = mydata)

# Print the model summary
summary(model1)

# Perform linear regression with three variables [3]

model2 <- lm(head_length ~ neck_height + system + genotype, data = mydata)

# Print the model summary
summary(model2)


# Estimation and interpretation:Graphical observation of the interaction

ggplot(mydata, aes(x=neck_height, y=head_length, shape=system, colour=system, fill=system)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE, outlier.shape= NA) +
  geom_point() +
  xlab("Vegetative height (cm)") +
  ylab("Head length (cm)") +
  theme_classic()

# Classic black and white 

ggplot(mydata, aes(x=neck_height, y=head_length, shape=system)) +
  geom_smooth(method="lm",se=FALSE,fullrange=TRUE, outlier.shape= NA) +
  geom_point() +
  xlab("Vegetative height (cm)") +
  ylab("Head length (cm)") +
  theme_classic()

# Test of the interaction and of the main effects: Test of the interaction

model6 <- lm(neck_height ~ system + genotype + system*genotype, data=mydata)
model7 <- lm(neck_height ~ system + genotype, data=mydata)
anova(model6, model7)


# Neck HEIGHT [2]

# Perform two-way ANOVA 
model18 <- aov(neck_height ~ system + genotype + system:genotype, data = mydata)
summary(model18)

# System 1   7492    7492  87.448  < 2e-16 *** [p value: 2 to the power of -16 = small i.e., >0.05]
# There is a significant difference between the two systems, significant level (‘***’ 0.001)
# Factor(genotype) 45  15184     337   3.938 3.33e-14 *** 
# [p value: 3.33 to the power of -14 = small i.e., >0.05]
# There is a significant difference between the genotypes (‘***’ 0.001)
# system:factor(genotype)  43  14352     334   3.896 1.49e-13 ***
# There is a significant difference in the interaction between system and genotype (‘***’ 0.001)



# Check normality assumption
residuals <- resid(model18)

# Shapiro-Wilk test
shapiro.test(residuals)

# Perform Tukey test
tukey_results1 <- TukeyHSD(model18)

# Print the Tukey test results
print(tukey_results1)

#Test of the interaction

model18 <- lm(neck_height ~ system + genotype + system*genotype, data=mydata)
model19 <- lm(neck_height ~ system + genotype, data=mydata)
anova(model18)

#Analysis of Variance Table

anova(model18,model19)

# model1 not significant

drop1(model18,.~.,test="F")

model20 <- lm(neck_height ~ system + genotype, data=mydata)
model21 <- lm(neck_height ~ system, data=mydata)
model22 <- lm(neck_height ~ genotype, data=mydata)
anova(model20,model21)

#Analysis of Variance Table

anova(model21,model22)

drop1(model21,.~.,test="F")

anova(model22)

# Modelisation of the problem: Graphical representation

library(cowplot)

plot_grid(nrow = 2, ncol = 2,
          ggplot(mydata, aes(y = neck_height, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE, outlier.shape= NA)+
            xlab("System") +
            ylab("Vegetative height (cm)")+
            theme_classic(),
          ggplot(mydata, aes(y = height, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE, outlier.shape= NA)+
            xlab("System") +
            ylab("Height (cm)")+
            theme_classic(),
          ggplot(mydata, aes(y = head_length, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE, outlier.shape= NA)+
            xlab("System") +
            ylab("Head length (cm)")+
            theme_classic(), 
          ggplot(mydata, aes(y = head_length, x = neck_height)) + 
            geom_point(outlier.shape= NA) +
            xlab("Vegetative height (cm)") +
            ylab("Head length (cm)")+
            theme_classic())

# Classic black and white

plot_grid(nrow = 2, ncol = 2,
          ggplot(mydata, aes(y = neck_height, x = system)) +
            geom_boxplot(show.legend = FALSE, outlier.shape= NA)+
            xlab("System") +
            ylab("Vegetative height (cm)")+
            theme_classic(),
          ggplot(mydata, aes(y = height, x = system)) +
            geom_boxplot(show.legend = FALSE, outlier.shape= NA)+
            xlab("System") +
            ylab("Height (cm)")+
            theme_classic(),
          ggplot(mydata, aes(y = head_length, x = system)) +
            geom_boxplot(show.legend = FALSE, outlier.shape= NA)+
            xlab("System") +
            ylab("Head length (cm)")+
            theme_classic(), 
          ggplot(mydata, aes(y = head_length, x = neck_height, shape= system, fill= system)) + 
            geom_point(outlier.shape= NA) +
            xlab("Vegetative height (cm)") +
            ylab("Head length (cm)")+
            theme_classic())


# Estimation and interpretation:Graphical observation of the interaction

#ggplot(mydata, aes(x=neck_height, y=head_length, shape=system, colour=system, fill=system)) +
  #geom_smooth(method="lm",se=FALSE,fullrange=TRUE) +
  #geom_point() +
  #xlab("Vegetative height (cm)") +
 # ylab("Head length (cm)") +
  #ggtitle("One regression by system")+
  #theme_classic()

# Test of the interaction and of the main effects: Test of the interaction

#model6 <- lm(height ~ system + genotype + system*genotype, data=mydata)
#model7 <- lm(height ~ system + genotype, data=mydata)
#anova(model6,model7)



#Plot for height

ggplot(data=mydata, aes(x=system, y=height))+
  geom_boxplot()+
  theme_classic()+
  ylab("Height (cm)")+
  xlab("System")



library(cowplot)

# Not required

plot_grid(nrow = 1, ncol = 3,
          ggplot(mydata, aes(y = height, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE)+
            xlab("System") +
            ylab("Total height (cm)"),
          ggplot(mydata, aes(y = neck_height, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE)+
            xlab("System") +
            ylab("Vegetative height (cm)"),
          ggplot(mydata, aes(y = head_length, x = system, fill = system)) +
            geom_boxplot(show.legend = FALSE)+
            xlab("System") +
            ylab("Reproductive length(cm)")
          


          
# TILLER 
          
# Perform Kruskal-Wallis test for non-normal data[=tiller]

kruskal.test(tiller ~ system, data = mydata) # non-significant


kruskal.test(tiller ~ genotype, data = mydata) # significant 

pairwise.wilcox.test(mydata$tiller, mydata$genotype,# No significant difference between the genotypes 
                     p.adjust.method = "BH")

pairwise.wilcox.test(mydata$tiller, mydata$genotype) # without adjusted P:  No significant difference between the genotypes 


wilcox.test(tiller ~ mydata$genotype,mydata$system, data=mydata) 

shapiro.test(mydata$tiller) # no
shapiro.test(mydata$head_length)
shapiro.test(mydata$neck_height)
shapiro.test(mydata$height)

# KW test 2

kwTest <- kruskal.test
print(kwTest)

# Mann-Whitney U test or the permutation test for non-parametric distribution

interaction_test <- pairwise.wilcox.test(mydata$tiller, mydata$system, mydata$genotype, p.adj = "none")
                                    

# Print the test results
interaction_test
                     
kruskal_test <- kruskal.test(tiller ~ system:genotype, data = data)

# Print the Kruskal-Wallis test results
print(kruskal.test)

# Perform two-way ANOVA [tiller]
model1 <- aov(tiller ~ system + genotype + system:genotype, data = data)
summary(model1)

# Check normality assumption
residuals1 <- resid(model1)

# Shapiro-Wilk test
shapiro.test(residuals1)
