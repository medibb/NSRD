# packages
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("moonBook")
library(moonBook)
install.packages("quantreg")
library(quantreg)
library(e1071)  # Skewness

# variables
MM <- read_excel(path = "C:/Users/LIFE/OneDrive/project/SRD/MM.xlsx",col_names=TRUE)
MM <- MM[,-1]
M20<-subset(MM, MM$Group==1)
F20<-subset(MM, MM$Group==2)
M50<-subset(MM, MM$Group==3)
F50<-subset(MM, MM$Group==4)
male<-subset(MM, MM$Gender==1)
female<-subset(MM, MM$Gender==2)
young<-subset(MM, MM$Age==1)
old<-subset(MM, MM$Age==2)

# Moonbook ANOVA, Subgroup analysis
mytable(Group~., data=MM, method=3)
mytable(Age+Gender~., data=MM, method=3)
mytable(Gender+Age~., data=MM, method=3)

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
rbind(
  quantile(M20$Duration_negative, probs=seq(0.96,0.9,-0.01)),
  quantile(F20$Duration_negative, probs=seq(0.96,0.9,-0.01)),
  quantile(M50$Duration_negative, probs=seq(0.96,0.9,-0.01)),
  quantile(F50$Duration_negative, probs=seq(0.96,0.9,-0.01))
)

# Amplitude btp percentile
rbind(
  quantile(M20$`Amplitude_btp`, probs=seq(0.04,0.1,0.01)),
  quantile(F20$`Amplitude_btp`, probs=seq(0.04,0.1,0.01)),
  quantile(M50$`Amplitude_btp`, probs=seq(0.04,0.1,0.01)),
  quantile(F50$`Amplitude_btp`, probs=seq(0.04,0.1,0.01))
)

# Amplitude ptp percentile
rbind(
  quantile(M20$`Amplitude_ptp`, probs=seq(0.04,0.1,0.01)),
  quantile(F20$`Amplitude_ptp`, probs=seq(0.04,0.1,0.01)),
  quantile(M50$`Amplitude_ptp`, probs=seq(0.04,0.1,0.01)),
  quantile(F50$`Amplitude_ptp`, probs=seq(0.04,0.1,0.01))
)

# Area percentile
rbind(
  quantile(M20$Area_negative, probs=seq(0.04,0.1,0.01)),
  quantile(F20$`Area_negative`, probs=seq(0.04,0.1,0.01)),
  quantile(M50$`Area_negative`, probs=seq(0.04,0.1,0.01)),
  quantile(F50$`Area_negative`, probs=seq(0.04,0.1,0.01))
)

# Conduction Velocity percentile
rbind(
  quantile(M20$Conduction_Velocity, probs=seq(0.04,0.1,0.01)),
  quantile(F20$Conduction_Velocity, probs=seq(0.04,0.1,0.01)),
  quantile(M50$Conduction_Velocity, probs=seq(0.04,0.1,0.01)),
  quantile(F50$Conduction_Velocity, probs=seq(0.04,0.1,0.01))
)



# shapiro.test, hist, density graph
shapiro.test(M20$Latency_onset)
hist(M20$`Latency_onset`, breaks = 50, col="grey", border="white", prob=T, xlim=c(2,5), ylim=c(0,3))
lines(density(M20$`Latency_onset`))
p<-quantile(M20$Latency_onset, probs=0.96)
abline(v=p)
arrows(p,2.5,p-0.15,2.5,length = 0.05)
text(p-0.3,2.5,"96th %")


shapiro.test(F50$Amplitude_btp)
hist(F50$`Amplitude_btp`, breaks = 30, col="grey", border="white", prob=T, xlim=c(5,30), ylim=c(0,0.2))
lines(density(F50$`Amplitude_btp`))
qqnorm(F50$Amplitude_btp)
qqline(F50$Amplitude_btp)
















