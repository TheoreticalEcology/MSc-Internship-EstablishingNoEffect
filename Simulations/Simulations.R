# source("functions_Isa.R")
source("MDD_function_Mair.R")

#' Generates the dataset
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function generates a dataframe of two samples of continuous values: "treatment" and "control". 
#' 
#'
CreateSampleDataset <- function(effect) {
  control <- rnorm(n=7, mean = 0, sd = 1)
  treatment <- rnorm(n=7, mean = effect, sd = 1)
  data <- data.frame(treatment, control)
  return(data)
}
 
#plot(data$control ~ data$treatment) 


effect = rep(c(0,1), each = 500)
dataset = 1:length(effect)

dataSets <- lapply(effect, CreateSampleDataset)

# summary(dataSets[[1]])
# boxplot(dataSets[[1]])

simulations = expand.grid(method = c("MDD", "CI", "EQUIV", "BFRatio"), 
                          threshold = seq(0,2, len = 20), dataset = dataset)

simulations$trueEffect = effect[simulations$dataset]

simulations$effectDetected = NA
simulations$noEffectTrusted = NA

str(simulations)

alpha = 0.05

for (i in 1:nrow(simulations)){
  
  dat = dataSets[[simulations$dataset[i]]]
  N = nrow(dat)
  
  # boxplot(dat, main = simulations$trueEffect[i])

  test <- t.test(dat$treatment, dat$control, alternative = "greater", var.equal = T, alpha = alpha)
  simulations$effectDetected[i] <- test$p.value < 0.05
    
  if (simulations$method[i] == "MDD") {
      
      varSample = var(c(dat$control-mean(dat$control),dat$treatment-mean(dat$treatment)))
      
      MDDres <- MDD(N1 = N, 
                 N2 = N, 
                 variance1 = varSample,
                 alpha=alpha, 
                 two.sided = F, 
                 var.equal = T)
      
      simulations$noEffectTrusted[i] =  MDDres$mdd <= simulations$threshold[i]
  }
  else if (simulations$method[i] == "CI") {
    
    test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T)
    
    d = data.frame(res = c(dat$treatment, dat$control), x = rep(c("T", "C"), each = N))
    
    fit <- lm(res~x, data = d)
    confint(fit)
    confint(fit, "wt")
    
    
    
    #CI <- test$conf.int[2] # upper bound
    
    xMean =  mean(dat$treatment)-mean(dat$control)
 
    xSDvarUneq = sqrt(((N-1)*sd(dat$treatment) + (N-1) * sd(dat$control))/ (2 * N - 2))
 
    xSDvarEqual = sd(c(dat$control-mean(dat$control),dat$treatment-mean(dat$treatment)))
    xSDLena = sqrt(var(c(dat$control-mean(dat$control),dat$treatment-mean(dat$treatment)))) 
    
    # Normal approximation CI <- xMean + 1.96 * xSDvarUneq *  sqrt(2/N) 
    
    CI <- xMean + qt(0.05, df = 2*N-2, lower.tail = FALSE) * xSDvarUneq * sqrt(2/N) 
    
    
    
    # how t.test()     #mean_ttest <- (1/2*N)*(sum(dat$trea
tment) + sum(dat$control))
    mean_ttest <- mean(dat$treatment)-mean(dat$control) 
    lower_quantile <- qt(1-(0.05/2), df = 2*N-2, lower.tail = FALSE)
    sd <- sd(c(dat$treatment,dat$control))
    CI_ttest <- mean_ttest + lower_quantile * sd/sqrt(2*N)
    
    
    #upperCI <- mean(dat$treatment)-mean(dat$control) + 
    #qt(0.05/2, df = 2*N-2, lower.tail = FALSE) * sqrt(var(dat$control-mean(dat$control)))/sqrt(N) + sqrt(var(dat$treatment-mean(dat$treatment)))/sqrt(N)
    
  
    simulations$noEffectTrusted[i] =  CI <= simulations$threshold[i]
  }
  else if (simulations$method[i] == "EQUIV") {
    
    one_sided_test <- t.test(dat$treatment, dat$control, alternative = "less") 
    
    EQUIV <- one_sided_test$p.value
    
    simulations$noEffectTrusted[i] =  EQUIV <= simulations$threshold[i]
  }
else if (simulations$method[i] == "BFRatio") {
  library(BayesFactor)
  bt <- ttestBF(dat$treatment, dat$control, nullInterval = c(Inf, 0))
  BF <- extractBF(bt)
  BFRatio <- log(BF$bf[1]/BF$bf[2])
  
  
  simulations$noEffectTrusted[i] =  BFRatio <= simulations$threshold[i]
}
}


detected = aggregate(effectDetected ~ method + threshold +  trueEffect, 
                   data = simulations,
                   FUN = mean)


result = aggregate(noEffectTrusted ~ method + threshold +  trueEffect + effectDetected, 
                       data = simulations,
                       FUN = mean)



plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "False trust rate ", ylab = "False mistrust rate")

plotResult <- function(method = "MDD", col = 1) {
  
  falseTrust <- result[result$method == method & result$effectDetected == F & result$trueEffect > 0, c(2, 5)]
  trueTrust <- result[result$method == method & result$effectDetected == F & result$trueEffect == 0, c(2, 5)]
  lines(falseTrust$noEffectTrusted, 1-trueTrust$noEffectTrusted, type = "b", col = col)
  legend(0.7, 1, legend=c("MDD", "CI", "EQUIV", "BFRatio"), col=c("black","red","green","blue"), lty=1:2, cex=0.6)
}

#abline(1,-1)
plotResult(method = "MDD")  
plotResult(method = "CI", col = 2) 
plotResult(method = "EQUIV", col = 3) 
plotResult(method = "BFRatio", col = 4) 

# falseTrust_CI <- result[result$method == 'CI' & result$effectDetected == F & result$trueEffect > 0, c(2,5)]
# thresholds = maximum acceptable difference, try: ==alpha 

