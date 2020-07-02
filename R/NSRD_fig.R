### Figure 3 ###

# shapiro.test, hist, density graph

## Latency
shapiro.test(F20$Latency_onset)
par(mar=c(5,5,5,5))
hist(F50$`Latency_onset`, breaks = 20, col="grey", border="white", prob=T, xlim=c(2,5), ylim=c(0,3),
     xlab = "Distal onser latancy" , ylab = "Density", main = "50's Female", cex.main=3, cex.lab=1.8, cex.axis=1.8)
lines(density(F50$`Latency_onset`))
p<-quantile(F50$Latency_onset, probs=0.96)
abline(v=p)
arrows(p,2.5,p-0.15,2.5,length = 0.05)
text(p-0.3,2.5,"96th %")

## Amplitude_Ptp

shapiro.test(F50$Amplitude_ptp)
par(mar=c(5,5,5,5))
hist(M50$`Amplitude_ptp`, breaks = 20, col="grey", border="white", prob=T, xlim=c(5,30), ylim=c(0,0.3),
     xlab = "Peak to peak amplitude" , ylab = "Density", main = "50's Male", cex.main=3, cex.lab=1.8, cex.axis=1.8)
lines(density(M50$`Amplitude_ptp`))
p<-quantile(M50$Amplitude_ptp, probs=0.04)
abline(v=p)
arrows(p,0.25,p+2,0.25,length = 0.05)
text(p+3,0.25,"4th %")

qqnorm(F50$Amplitude_btp)
qqline(F50$Amplitude_btp)