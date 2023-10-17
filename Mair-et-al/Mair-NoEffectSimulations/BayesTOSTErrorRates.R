
experiments <- expand.grid(
  effectsize = runif(100, 0, 0.6), #seq(0, 0.5, 0.1),
  variance = 0.5,#seq(0.1, 0.9, 0.1),
  samplesize = 5,#seq(3, 10, 1),
  mean = 1,
  bound = c(0.01, 0.1, 0.2, 0.3)#seq(0.1, 1, 0.1)
)
nrow(experiments)*3/60

size = nrow(experiments)

out <- data.frame(
  p = rep(NA, size),
  TOST = rep(NA, size),
  BEST = rep(NA, size)
)

for(i in 1:size) {
  try({
  N <- experiments$samplesize[i]
  variance <- experiments$variance[i]
  mean <- experiments$mean[i]
  effectsize <- experiments$effectsize[i]
  bound <- experiments$bound[i]
  #simulate experiments:
  control <- rnorm(n = N, mean = mean, sd = sqrt(variance))
  tmean <- mean * (1 - effectsize)
  treatment <- rnorm(n = N, mean = tmean, sd = sqrt(variance))
  #3 analyses
  ttest <- t.test(control, treatment, alternative = "greater", var.equal = T)
  TOST <-  t.test(control, treatment, alternative = "less", var.equal = T, mu = bound)
  bayes <- BESTmcmc(control, treatment)
  mdiff <- bayes$mu1 - bayes$mu2
  # write to output:
  out$p[i] <- ttest$p.value
  out$TOST[i] <- TOST$p.value
  out$BEST[i] <- mean(mdiff > bound)
  }, silent = T)
}

all <- cbind(experiments, out)
#saveRDS(all, "all_200619.rds")
#saveRDS(all, "all_200620.rds") # this is withexperiments <- expand.grid(
# effectsize = runif(100, 0, 0.6), #seq(0, 0.5, 0.1),
# variance = 0.5,#seq(0.1, 0.9, 0.1),
# samplesize = 5,#seq(3, 10, 1),
# mean = 1,
# bound = c(0.01, 0.1, 0.2, 0.3)#seq(0.1, 1, 0.1)
# )

bounds = seq(0.1, 0.5, 0.1)

ErrorRates <- data.frame(
  bound = NA,
  ttest.ftrust = NA,
  ttest.I = NA,
  TOST.I = NA,
  TOST.II = NA,
  BEST.I = NA,
  BEST.II = NA
)

for (i in 1:length(bounds))
{
  pop = all[all$bound == bounds[i],]
  
  ttest.ftrust <- length(which(pop$p > 0.05 & pop$effectsize >pop$bound))/
    length(which(pop$effectsize >pop$bound))
  ttest.I <- length(which(pop$p <= 0.05 & pop$effectsize < pop$bound))/
    length(which(pop$effectsize < pop$bound)) 
  TOST.I <- length(which(pop$TOST <= 0.05 & pop$effectsize > pop$bound))/
                  length(which(pop$effectsize > pop$bound))
  TOST.II <- length(which(pop$TOST > 0.05 & pop$effectsize < pop$bound))/
                  length(which(pop$effectsize < pop$bound))
  BEST.I <- length(which(pop$BEST <= 0.05 & pop$effectsize > pop$bound))/
    length(which(pop$effectsize > pop$bound))
  BEST.II <- length(which(pop$BEST > 0.05 & pop$effectsize < pop$bound))/
    length(which(pop$effectsize < pop$bound))
  
  temp = data.frame(
      bound = bounds[i],
      ttest.ftrust = ttest.ftrust,
      ttest.I = ttest.I,
      TOST.I = TOST.I,
      TOST.II = TOST.II,
      BEST.I = BEST.I,
      BEST.II = BEST.II
  )
  ErrorRates <- rbind(ErrorRates, temp)    
}
ErrorRates <- ErrorRates[-1,]
ErrorRates
# ErrorRates <- data.frame(
# bound = bound,
# ttest.ftrust = rep(NA,nrow(all)),
# ttest.I = rep(NA,nrow(all)),
# TOST.I = rep(NA,nrow(all)),
# TOST.II = rep(NA,nrow(all)),
# BEST.I = rep(NA,nrow(all)),
# BEST.II = rep(NA,nrow(all))
# )

# for (i in 1:length(bounds))
# {
#   with(all[bound == bounds[i],], {
#     ttest.ftrust <- length(which(p > 0.05 & effectsize >0))/
#       length(which(effectsize >0))
#     ttest.I <- length(which(p <= 0.05 & effectsize == 0))/
#       length(which(effectsize == 0)) 
#     TOST.I <- length(which(TOST <= 0.05 & effectsize == bound))/
#       length(which(effectsize == bound))
#     TOST.II <- length(which(TOST > 0.05 & effectsize < bound))/
#       length(which(effectsize < bound))
#     BEST.I <- length(which(BEST <= 0.05 & effectsize > bound))/
#       length(which(effectsize > bound))
#     BEST.II <- length(which(BEST > 0.05 & effectsize < bound))/
#       length(which(effectsize < bound))
#     ErrorRates[i,2] <- ttest.ftrust 
#     ErrorRates[i,3] <- ttest.I
#     ErrorRates[i,4] <- TOST.I
#     ErrorRates[i,5] <- TOST.II
#     ErrorRates[i,6] <- BEST.I
#     ErrorRates[i,7] <- BEST.II
#     
#   })
# }

op <- par(mar = c(5,6,1,1), mfrow=c(1,1))

#ttest
plot(ErrorRates$ttest.ftrust, ErrorRates$ttest.I, xlim = c(-0.1, 1), ylim = c(-0.05,1), 
     type = "l", xlab = "False trust in absence of effect", ylab = "", bty = "l", las = 1, 
     cex.lab = 1.5, cex.axis = 1.5, col = "darkred", lwd = 2)
mtext("False mistrust in absence of effect", side = 2, line = 3.5, cex = 1.5)
text(ErrorRates$ttest.ftrust, ErrorRates$ttest.I, labels = ErrorRates$bound, 
     cex= 1.5, col = "darkred", font = 4, pos = c(4, 4, 4, rep(3, 5)))
points(ErrorRates$ttest.ftrust, ErrorRates$ttest.I, pch = 20, col = "darkred")

#TOST
lines(ErrorRates$TOST.I, ErrorRates$TOST.II, col = "darkgreen", lty = 3, lwd = 2)
text(ErrorRates$TOST.I, ErrorRates$TOST.II, labels=ErrorRates$bound,
     cex= 1.5, col = "darkgreen", pos = c(4, 2, 4, 4, 3, 3, 3, 3))
points(ErrorRates$TOST.I, ErrorRates$TOST.II, pch = 20, col = "darkgreen")

#BEST
lines(ErrorRates$BEST.I, ErrorRates$BEST.II, col = "darkblue", lty = 2, lwd = 2)
text(ErrorRates$BEST.I, ErrorRates$BEST.II, labels=ErrorRates$bound,
     cex= 1.5, col = "darkblue", pos = c(rep(2, 4), rep(1, 4)))
points(ErrorRates$BEST.I, ErrorRates$BEST.II, pch = 20, col = "darkblue")

# add legend
arrows(0.4 , 0.8, 0.5, 0.8, angle = 0, col = "darkblue", lty = 2, lwd = 2, length = 0)
arrows(0.4 , 0.7, 0.5, 0.7, angle = 0, col = "darkgreen", lty = 3, lwd = 2, length = 0)
arrows(0.4 , 0.6, 0.5, 0.6, angle = 0, col = "darkred", lty = 1, lwd = 2, length = 0)
text(0.52 , 0.8, "Bayesian t test", col = "darkblue", pos=4, offset= 0, cex = 1.5)
text(0.52 , 0.7, "H0=bound", col = "darkgreen", pos=4, offset= 0, cex = 1.5)
text(0.52 , 0.6, "Standard t test", col = "darkred", pos=4, offset= 0, cex = 1.5)
par(op)


#---------------------------------
#1. standard NHST----
t.test$p.value # prob. of data if H0: set thresholds on this (usually 0.05) >> error rates

#2. Bayesian t.test: posterior
bayes <- BESTmcmc(control, treatment)
mdiff <- bayes$mu1 - bayes$mu2
mean(mdiff <= bound) #= prob. of real effect < bound >> set thresholds on this

#3. t.test with H0: real effect = bound
tost <- TOSTtwo.raw(m1=mean(y1),m2=mean(y2),sd1=sd(y1),sd2=sd(y2),n1=6,n2=6,low_eqbound=-bound, high_eqbound=bound,alpha = 0.05, var.equal=TRUE) # use this for plotting
t.test(y1,y2, alternative = "less", mu = 2)$p.value # set threshold on this (0.05)




