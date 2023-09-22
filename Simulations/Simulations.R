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


effect = rep(c(0,1), each = 5000)
dataset = 1:length(effect)

dataSets <- lapply(effect, CreateSampleDataset)

# summary(dataSets[[1]])
# boxplot(dataSets[[1]])

simulations = expand.grid(method = c("MDD", "CI"), 
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
    CI <- test$conf.int[2] # upper bound
    #CI <- mean(dat$treatment)-mean(dat$control) + 
    #qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(dat$control-mean(dat$control),dat$treatment-mean(dat$treatment)))) * sqrt(2/N)
    # diff_means <- mean(dat$treatment) - mean(dat$control)
    # sd_errorTR <- var(dat$treatment)/sqrt(length(dat$treatment))
    # sd_errorCR <- var(dat$control)/sqrt(length(dat$control))
    # CI<- diff_means + qt((1 - 0.05) / 2, df = 22 - 1) * sd_errorTR
    # CI<- diff_means + qt((1 - 0.05) / 1, df = 2*length(dat$treatment)-2) * sqrt(sd_errorTR)
    # CI<- diff_means + qt(0.05/ 1, df = 2*length(dat$treatment)-2) * sqrt(var(c(dat$control-mean(dat$control),dat$treatment-mean(dat$treatment)))) * sqrt(2/N)
    #if(CI<=threshold){
    #plot(CI )
    #}
     
    simulations$noEffectTrusted[i] =  CI <= simulations$threshold[i]
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
}

abline(1,-1)
plotResult(method = "MDD")  
plotResult(method = "CI", col = 2)   

#legend("topright", legend = legend_label, col = col, pch = 1, ncol=1)

# falseTrust_CI <- result[result$method == 'CI' & result$effectDetected == F & result$trueEffect > 0, c(2,5)]


