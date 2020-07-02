# packages
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("moonBook")
library(moonBook)
install.packages("ztable")
library(ztable)
install.packages("quantreg")
library(quantreg)
library(e1071)  # Skewness

# variables
MM <- read_excel(path = "C:/Users/LIFE/OneDrive/project/NSRD/CSV/MM.xlsx",col_names=TRUE)
MM <- MM[,-1]
M20<-subset(MM, MM$Group==1)
F20<-subset(MM, MM$Group==2)
M50<-subset(MM, MM$Group==3)
F50<-subset(MM, MM$Group==4)
male<-subset(MM, MM$Gender==1)
female<-subset(MM, MM$Gender==2)
young<-subset(MM, MM$Age==1)
old<-subset(MM, MM$Age==2)


# correlation analysis
install.packages("corrplot")
library(corrplot)
corrplot(cor(MM[,2:20]))

cor.test(MM$Lat, MM$BMI)
cor.test(MM$AmpPtp, MM$BMI)
cor.test(MM$AreaNeg, MM$BMI)
cor.test(MM$DurNeg, MM$BMI)

## two-way ancova lm(depVar ~ covar1 + covar2  + ... + covar# + IndVar*IndVar, data=data)
output <- lm(MM$Lat ~ Height + Weight + Muscle + Gender*Age, data = MM)
output <- lm(MM$AreaNeg ~ Height + Weight + Muscle + Gender*Age, data = MM)
output <- lm(MM$DurNeg ~ Height + Weight + Muscle + Gender*Age, data = MM)
output <- lm(MM$AmpPtp ~ Height + Weight + Muscle + Gender*Age, data = MM)
output <- lm(MM$AreaNegE ~ Height + Weight + Muscle + Gender*Age, data = MM)
output <- lm(MM$DurNegE ~ Height + Weight + Muscle + Gender*Age, data = MM)
output <- lm(MM$AmpPtpE ~ Height + Weight + Muscle + Gender*Age, data = MM)
output <- lm(MM$CV ~ Height + Weight + Muscle + Gender*Age, data = MM)

summary.aov(output)


#post hocs on the adjusted means
#effect("IV column", output)
install.packages("effects")
library(effects)
effect("Age", output)
effect("Gender", output)

MM$Age <-as.factor(MM$Age)
MM$Gender <-as.factor(MM$Gender)

male <- subset(MM, Gender == 1)
female <- subset(MM, Gender == 2)
young <- subset(MM, Age == 1)
old <- subset(MM, Age == 2)

male.out = lm(male$Lat ~ Height + Age, data = male)
female.out = lm(female$Lat ~ Height + Age, data = female)
young.out = lm(young$Lat ~ Height + Gender, data = young)
old.out = lm(old$Lat ~ Height + Gender, data = old)

install.packages("multcomp")
library(multcomp)
male_post = glht(male.out, linfct=mcp(Age = "Tukey"))
female_post = glht(female.out, linfct=mcp(Age = "Tukey"))
young_post = glht(young.out, linfct=mcp(Gender = "Tukey"))
old_post = glht(old.out, linfct=mcp(Gender = "Tukey"))
summary(male_post)
summary(female_post)
summary(young_post)
summary(old_post)


# Moonbook ANOVA, Subgroup analysis
mytable(Group~., data=MM, method=3)
mytable(Age+Gender~., data=MM, method=3)
mytable(Gender+Age~., data=MM, method=3)

# latex transform
mylatex
xtable




# Latency percentile
latP<-rbind(
  quantile(M20$`Latency_onset`, probs=seq(0.96,0.9,-0.01)),
  quantile(F20$`Latency_onset`, probs=seq(0.96,0.9,-0.01)),
  quantile(M50$`Latency_onset`, probs=seq(0.96,0.9,-0.01)),
  quantile(F50$`Latency_onset`, probs=seq(0.96,0.9,-0.01))
)
latP <- data.frame(latP)
rownames(latP) <-c("M20","F20","M50","F50")

# Duration percentile
DurP<-rbind(
  quantile(M20$Duration_negative, probs=seq(0.96,0.9,-0.01)),
  quantile(F20$Duration_negative, probs=seq(0.96,0.9,-0.01)),
  quantile(M50$Duration_negative, probs=seq(0.96,0.9,-0.01)),
  quantile(F50$Duration_negative, probs=seq(0.96,0.9,-0.01))
)
DurP <- data.frame(DurP)
rownames(DurP) <-c("M20","F20","M50","F50")

# Amplitude btp percentile
AbtpP<-rbind(
  quantile(M20$`Amplitude_btp`, probs=seq(0.04,0.1,0.01)),
  quantile(F20$`Amplitude_btp`, probs=seq(0.04,0.1,0.01)),
  quantile(M50$`Amplitude_btp`, probs=seq(0.04,0.1,0.01)),
  quantile(F50$`Amplitude_btp`, probs=seq(0.04,0.1,0.01))
)
AbtpP <-data.frame(AbtpP)
rownames(AbtpP) <-c("M20","F20","M50","F50")
AbtpP

# Amplitude ptp percentile
AptpP<-rbind(
  quantile(M20$`Amplitude_ptp`, probs=seq(0.04,0.1,0.01)),
  quantile(F20$`Amplitude_ptp`, probs=seq(0.04,0.1,0.01)),
  quantile(M50$`Amplitude_ptp`, probs=seq(0.04,0.1,0.01)),
  quantile(F50$`Amplitude_ptp`, probs=seq(0.04,0.1,0.01))
)
AptpP <-data.fram(AptpP)
rownames(AptpP) <-c("M20","F20","M50","F50")
AptpP

# Area percentile
AreaP<-rbind(
  quantile(M20$Area_negative, probs=seq(0.04,0.1,0.01)),
  quantile(F20$`Area_negative`, probs=seq(0.04,0.1,0.01)),
  quantile(M50$`Area_negative`, probs=seq(0.04,0.1,0.01)),
  quantile(F50$`Area_negative`, probs=seq(0.04,0.1,0.01))
)
AreaP <-data.fram(AreaP)
rownames(AreaP) <-c("M20","F20","M50","F50")
AreaP

# Conduction Velocity percentile
CVP<-rbind(
  quantile(M20$Conduction_Velocity, probs=seq(0.04,0.1,0.01)),
  quantile(F20$Conduction_Velocity, probs=seq(0.04,0.1,0.01)),
  quantile(M50$Conduction_Velocity, probs=seq(0.04,0.1,0.01)),
  quantile(F50$Conduction_Velocity, probs=seq(0.04,0.1,0.01))
)
CVP <-data.fram(CVP)
rownames(CVP) <-c("M20","F20","M50","F50")
CVP


