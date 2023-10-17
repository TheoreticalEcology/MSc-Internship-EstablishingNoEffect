set.seed(123)

control <- rnorm(n = 5, mean = 1, sd = 0.75)
treatment <- rnorm(n = 5, mean = 0.4, sd = 0.75)

t.test(control, treatment, alternative = "greater", var.equal = T)

# MDD
variance = var(c(control-mean(control), treatment-mean(treatment)))
mdd <- MDD(N1 = 5, N2 = 5, variance1 = variance, two.sided = F)$mdd

# MDE
mde <- power.t.test(n = 5, delta = NULL, sd = sd(c(control-mean(control), treatment-mean(treatment))), 
                          sig.level = 0.05, alternative="one.sided", type = "two.sample", 
                          power = 0.8)$delta
# CI
upperCI <- mean(control)-mean(treatment) + 
  qt(0.05/1, df = 2*5-2, lower.tail = FALSE) * sqrt(variance) * sqrt(2/5)

c(mde,mdd,upperCI)
# set threshold to 0.9 for all: > MDD = trust, MDE and CI = mistrust 


op <- par(bty = "l")
boxplot(control, treatment, col = "white", bty = "l")
mtext(c("Control", "Treatment"), side = 1, at = c(1, 2), line = 1, cex = 2) 

t.test(control, treatment, alternative = "less", mu = 0.9, var.equal = T)
bayes <- BESTmcmc(control, treatment)
mdiff <- bayes$mu1 - bayes$mu2
mean(mdiff > 0.9)
mean(mdiff <= 0.9)

TOSTtwo.raw(m1=mean(control),m2=mean(treatment),sd1=sd(control),sd2=sd(treatment),n1=5,n2=5,low_eqbound=-0.9, high_eqbound=0.9, alpha = 0.05, var.equal=TRUE)

BF <- ttestBF(control, treatment, nullInterval = c(0, Inf))
1/BF

