library(BayesFactor)

source("MDDfunction.R")

size = 10000

experiments = data.frame(
  effect = c(rep("no",size/2), rep("yes",size/2)),
  effectsize = c(rep(0,size/2), rep(0.1,size/2)), 
  variance = runif(size, max = 1, min=0),
  sampleSize = 5,#sample(3:10, size, replace = T),
  mean = 1 #sample(, size, replace=T)
)

out <- data.frame(
  p = rep(NA, size),
  MDD = rep(NA, size),
  MDE = rep(NA, size),
  upperCI = rep(NA, size),
  BF = rep(NA, size),
  OST1 = rep(NA, size),
  OST2 = rep(NA, size),
  OST3 = rep(NA, size),
  OST4 = rep(NA, size),
  OST5 = rep(NA, size),
  OST6 = rep(NA, size),
  OST7 = rep(NA, size),
  OST8 = rep(NA, size),
  OST9 = rep(NA, size),
  OST10 = rep(NA, size)
)


for (i in 1:size)
{
  N <- experiments$sampleSize[i]
  variance <- experiments$variance[i]
  mean <- experiments$mean[i]
  effectsize <- experiments$effectsize[i]
  #simulate experiments:
  control <- rnorm(n = N, mean = mean, sd = sqrt(variance))
  tmean <- mean * (1 - effectsize)
  treatment <- rnorm(n = N, mean = tmean, sd = sqrt(variance))
  #tests and analyses:
  test <- t.test(control, treatment, alternative = "greater", var.equal = T)
  mdd <- MDD(N1 = N, N2 = N, variance1 = var(c(control-mean(control),treatment-mean(treatment))),
                alpha=0.05, two.sided = F, var.equal = T)
  MDE <- power.t.test(n = N, delta = NULL, sd = sd(c(control-mean(control),treatment-mean(treatment))), 
                            sig.level = 0.05, alternative="one.sided", type = "two.sample", 
                            power = 0.8)
  upperCI <- mean(control)-mean(treatment) + 
    qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(control-mean(control),treatment-mean(treatment)))) * sqrt(2/N)
  BF <- ttestBF(control, treatment, nullInterval = c(0, Inf))
  BF <- 1/BF[1]
  OST1 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 0.1)
  OST2 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 0.2)
  OST3 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 0.3)
  OST4 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 0.4)
  OST5 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 0.5)
  OST6 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 0.6)
  OST7 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 0.7)
  OST8 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 0.8)
  OST9 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 0.9)
  OST10 <- t.test(control, treatment, alternative = "less", var.equal = T, mu = 1)
  
  out$p[i] <- test$p.value
  out$MDD[i] <- mdd$mdd
  out$MDE[i] <- MDE$delta
  out$upperCI[i] <- upperCI
  out$BF[i] <- as.data.frame(BF[1])$bf
  out$OST1[i] <- OST1$p.value
  out$OST2[i] <- OST2$p.value
  out$OST3[i] <- OST3$p.value
  out$OST4[i] <- OST4$p.value
  out$OST5[i] <- OST5$p.value
  out$OST6[i] <- OST6$p.value
  out$OST7[i] <- OST7$p.value
  out$OST8[i] <- OST8$p.value
  out$OST9[i] <- OST9$p.value
  out$OST10[i] <- OST10$p.value
} 


all <- cbind(experiments, out)
nonsig <- all[all$p >= 0.05,]

# Error rates ----------------------------------------------------------------------
thresholds = seq(from = 0.1, to = 1, by = 0.05)
thresholds2 = c(seq(1, 3, 0.25), 10^(1/2), 10, 10^(3/2), 10^2)
thresholds3 = seq(from = 0.1, to = 1, by = 0.1)

ErrorRates <- data.frame(
  threshold = thresholds,
  mdd.ftrust = rep(NA,length(thresholds)),
  mdd.fmist = rep(NA,length(thresholds)),
  post.ftrust = rep(NA,length(thresholds)),
  post.fmist = rep(NA,length(thresholds)),
  CI.ftrust = rep(NA,length(thresholds)),
  CI.fmist = rep(NA,length(thresholds))
)

ErrorRates2 <- data.frame(
  threshold2 = thresholds2,
  BF.fzero = rep(NA,length(thresholds2)),
  BF.feff = rep(NA,length(thresholds2))
)

ErrorRates3 <- data.frame(
  threshold = thresholds3,
  OST.fzero = rep(NA, length(thresholds3)),
  OST.feff = rep(NA, length(thresholds3))
)

# calculate fales trust/mistrust rates for MDE, MDD and CI, and error rates for BF
for (i in 1:length(thresholds))
{
  mdd.ftrust <- length(which(nonsig$MDD <= ErrorRates$threshold[i] & 
                               nonsig$effect == "yes"))/length(which(all$effect == "yes"))
  mdd.fmist <- (length(which(nonsig$MDD > ErrorRates$threshold[i] & nonsig$effect == "no")) + 
                  length(which(all$p < 0.05 & all$effect == "no")))/length(which(all$effect == "no"))
  post.ftrust <- length(which(nonsig$MDE <= ErrorRates$threshold[i] & 
                                 nonsig$effect == "yes"))/length(which(all$effect == "yes"))
  post.fmist <- (length(which(nonsig$MDE > ErrorRates$threshold[i] & nonsig$effect == "no"))+
                   length(which(all$p < 0.05 & all$effect == "no")))/length(which(all$effect == "no")) 
  CI.ftrust <- length(which(nonsig$upperCI <= ErrorRates$threshold[i] & 
                              nonsig$effect == "yes"))/length(which(all$effect == "yes"))
  CI.fmist <- (length(which(nonsig$upperCI > ErrorRates$threshold[i] & nonsig$effect == "no")) +
                   length(which(all$p < 0.05 & all$effect == "no")))/length(which(all$effect == "no")) 
  ErrorRates[i,2] <- mdd.ftrust
  ErrorRates[i,3] <- mdd.fmist
  ErrorRates[i,4] <- post.ftrust
  ErrorRates[i,5] <- post.fmist
  ErrorRates[i,6] <- CI.ftrust
  ErrorRates[i,7] <- CI.fmist
}

for (i in 1:length(thresholds2))
{
  BF.fzero <- length(which(all$BF >= ErrorRates2$threshold2[i] & 
                             all$effect == "yes"))/length(which(all$effect == "yes"))
  BF.feff <- length(which(all$BF < ErrorRates2$threshold2[i] & 
                            all$effect == "no"))/length(which(all$effect == "no"))
  ErrorRates2[i, 2] <- BF.fzero
  ErrorRates2[i, 3] <- BF.feff
}

for(i in 1:10){
ErrorRates3$OST.fzero[i] <- length(which(all[,i+10] <= 0.05 & 
                                  all$effect == "yes"))/length(which(all$effect == "yes"))
ErrorRates3$OST.feff[i] <- length(which(all[,i+10] > 0.05 & 
                                  all$effect == "no"))/length(which(all$effect == "no"))
}


## plot error rates:
#png(".png", width = 7.7, height = 5.3, units = "in", res = 1000)
#esp(".esp", width = 7.7, height = 5.3, units = "in", res = 1000)
#pdf("Pareto-plot_BF_OST.pdf", width = 7.7, height = 5.3) #for final plot preparation in ink


png("Plots_230215/effectsize_0.1_all_zoomin.png", width = 7.7, height = 5.3, units = "in", res = 1000)
#
op <- par(mar = c(5,6,3,1), mfrow=c(1,1), xpd = T)

#MDD
plot(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, xlim=c(0,0.1), ylim = c(0.8,1),#xlim = c(-0.1, 1), #ylim = c(-0.05,1), 
     type = "l", xlab = "False negatives", ylab = "", bty = "l", las = 1, 
     cex.lab = 1.5, cex.axis = 1.5, col = "darkred", lwd = 2)
mtext("False positives", side = 2, line = 3.5, cex = 1.5)
text(ErrorRates$mdd.ftrust[ErrorRates$threshold == 0.1 | ErrorRates$threshold == 1], 
     ErrorRates$mdd.fmist[ErrorRates$threshold == 0.1 | ErrorRates$threshold == 1], 
     labels = ErrorRates$threshold[ErrorRates$threshold == 0.1 | ErrorRates$threshold == 1],
    cex= 1.5, col = "darkred", font = 4, pos = c(4, 4))
points(ErrorRates$mdd.ftrust, ErrorRates$mdd.fmist, pch = 17, col = "darkred")

#Posteffect
lines(ErrorRates$post.ftrust, ErrorRates$post.fmist, col = "darkgreen", lty = 3, lwd = 2)
text(ErrorRates$post.ftrust[ErrorRates$threshold == 0.1 | ErrorRates$threshold == 1],
     ErrorRates$post.fmist[ErrorRates$threshold == 0.1 |  ErrorRates$threshold == 1], 
     labels=ErrorRates$threshold[ErrorRates$threshold == 0.1 | ErrorRates$threshold == 1],
     cex= 1.5, col = "darkgreen", pos = c(3,2))
points(ErrorRates$post.ftrust, ErrorRates$post.fmist, pch = 19, col = "darkgreen")

#CI
lines(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, col = "darkblue", lty = 2, lwd = 2)
text(ErrorRates$CI.ftrust[ErrorRates$threshold == 0.1 |ErrorRates$threshold == 1], 
     ErrorRates$CI.fmist[ErrorRates$threshold == 0.1 |  ErrorRates$threshold == 1], 
     labels=ErrorRates$threshold[ErrorRates$threshold == 0.1 | ErrorRates$threshold == 1],
     cex= 1.5, col = "darkblue", pos = c(4,4))
points(ErrorRates$CI.ftrust, ErrorRates$CI.fmist, pch = 20, col = "darkblue")

# OST
lines(ErrorRates3$OST.fzero, ErrorRates3$OST.feff, col = "darkorange3", lty = 1, lwd = 3)
text(ErrorRates3$OST.fzero[ErrorRates3$threshold == 0.1 |ErrorRates3$threshold == 1], 
     ErrorRates3$OST.feff[ErrorRates3$threshold == 0.1 |  ErrorRates3$threshold == 1], 
     labels=ErrorRates3$threshold[ErrorRates3$threshold == 0.1 | ErrorRates3$threshold == 1],
     cex= 1.5, col = "darkorange3", pos = c(2,1))
points(ErrorRates3$OST.fzero, ErrorRates3$OST.feff, pch = 3, col = "darkorange3")

#BF
lines(ErrorRates2$BF.fzero, ErrorRates2$BF.feff, col = "steelblue", lty = 3, lwd = 3)
text(ErrorRates2$BF.fzero[ErrorRates2$threshold2 == 1 |ErrorRates2$threshold2 == 10^(1/2)| ErrorRates2$threshold2 == 10^2], 
     ErrorRates2$BF.feff[ErrorRates2$threshold2 == 1 | ErrorRates2$threshold2 == 10^(1/2) | ErrorRates2$threshold2 == 10^2], 
     labels= c(1, 3.2, 100), pos = c(2,2,2),
     cex= 1.5, col = "steelblue")
points(ErrorRates2$BF.fzero, ErrorRates2$BF.feff, pch = 15, col = "steelblue")


# add legend
# arrows(0.4 , 0.8, 0.5, 0.8, angle = 0, col = "darkblue", lty = 2, lwd = 2, length = 0)
# arrows(0.4 , 0.7, 0.5, 0.7, angle = 0, col = "darkgreen", lty = 3, lwd = 2, length = 0)
# arrows(0.4 , 0.6, 0.5, 0.6, angle = 0, col = "darkred", lty = 1, lwd = 2, length = 0)
# arrows(0.4, 0.5, 0.5, 0.5, angle = 0, col = "red", lty = 1, lwd = 3, length = 0)
# points(0.45, 0.8, pch = 20, col = "darkblue")
# points(0.45, 0.7, pch = 19, col = "darkgreen")
# points(0.45, 0.6, pch = 17, col = "darkred")
# points(0.45, 0.5, pch = 15, col = "red")
text(0.52 , 0.9, "CI upper bound", col = "darkblue", pos=4, offset= 0, cex = 1.5)
text(0.52 , 0.8, "MDE (80% power)", col = "darkgreen", pos=4, offset= 0, cex = 1.5)
text(0.52 , 0.7, "MDD", col = "darkred", pos=4, offset= 0, cex = 1.5, font = 4)
text(0.52 , 0.6, "Bayes Factor", col = "steelblue", pos=4, offset= 0, cex = 1.5) #: nominator: 0, denominator: > 0
text(0.52 , 0.5, "Interval hypothesis test", col = "darkorange3", pos=4, offset= 0, cex = 1.5) #: nominator: 0, denominator: > 0
text("True effect sizes = either 0.1 or 0", x = -0.05, y = 0, pos = 4)
text("1000 simulated experiments", x = -0.05, y = 0.05, pos = 4)

par(op)

dev.off()



