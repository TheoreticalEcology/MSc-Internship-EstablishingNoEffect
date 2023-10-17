# install package BEST -  prerequisites 
# download latest BEST version from https://cran.r-project.org/src/contrib/Archive/BEST/BEST_0.5.4.tar.gz
# install latest JAGS version from https://sourceforge.net/projects/mcmc-jags/
install.packages("coda")
install.packages("rjags")
install.packages("HDInterval")
install.packages("~/Desktop/BEST_0.5.4.tar", repos = NULL, type = "source") # adapt own path
library(rjags)
library(coda)
library(HDInterval)
library(BEST)
# install package BayesianFirstAid from github 
# alos need JAGS
install.packages("devtools")
devtools::install_github("rasmusab/bayesian_first_aid")
library(BayesianFirstAid)


install.packages("TOSTER")
library(TOSTER)


sessionInfo()

BEST::BESTmcmc
y1 <- c(5.77, 5.33, 4.59, 4.33, 3.66, 4.48)
y2 <- c(3.88, 3.55, 3.29, 2.59, 2.33, 3.59)

BESTout <- BESTmcmc(y1, y2) #can specify priors here!
plot(BESTout)
summary(BESTout)
plotPostPred(BESTout)
muDiff <- BESTout$mu1 - BESTout$mu2
mean(muDiff > 1.47) # this gives the prob. of effect > 1.47
mean(muDiff < 0.5)


bt = bayes.t.test(y1,y2) # prior cannot be specified here? > priors are flat

plot(bt)

###TOSTER
TOSTtwo.raw(m1=mean(y1),m2=mean(y2),sd1=sd(y1),sd2=sd(y2),n1=6,n2=6,low_eqbound=-2, high_eqbound=2, alpha = 0.05, var.equal=TRUE)


t.test(y1,y2, alternative = "less", mu = 2)
t.test(y1,y2, alternative = "greater", mu = -2)

