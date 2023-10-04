source("MDD_function_Mair.R")

#' Generates the dataset
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function generates a dataframe of two samples of continuous values: "treatment" and "control". 
#' 
#'
CreateSampleDataset <- function(effect) {
  control <- rnorm(n=25, mean = 0, sd = 1)
  treatment <- rnorm(n=25, mean = effect, sd = 1)
  data <- data.frame(treatment, control)
  return(data)
}


effect = rep(c(0,0.75), each = 500)
dataset = 1:length(effect)

dataSets <- lapply(effect, CreateSampleDataset)

simulations = expand.grid(method = c("MDD", "CI", "EQUIV", "BFRatio", "Alpha"), 
                          threshold = seq(0,1, len = 15), dataset = dataset)

simulations$trueEffect = effect[simulations$dataset]

simulations$effectDetected = NA
simulations$noEffectTrusted = NA

str(simulations)


library(parallel)
library(BayesFactor)

cl <- makeCluster(5)
clusterExport(cl, varlist = list("simulations", "dataSets"))
clusterEvalQ(cl, {source("MDD_function_Mair.R")})

results_sim =
  parLapply(cl, 1:nrow(simulations), function(i) {
    
    alpha = 0.05
    
    dat = dataSets[[simulations$dataset[i]]]
    N = nrow(dat)
    
    test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T, alpha = alpha)
    simulations$effectDetected[i] <- test$p.value < 0.05
    
    if (simulations$method[i] == "MDD") {
      
      #varSample = var(c(dat$treatment-mean(dat$treatment),dat$control-mean(dat$control)))
      #MDDres <- MDD(N1 = N, 
                   # N2 = N, 
                   # variance1 = var(c(dat$treatment-mean(dat$treatment),dat$control-mean(dat$control))),
                   # alpha = alpha, 
                   # two.sided = F, 
                   # var.equal = T)
     # MDD <- MDDres$mdd
      
      
      t.critical <- qt(0.05, df = 2*N-2, lower.tail = FALSE)
      s <- sqrt(var(c(dat$treatment,dat$control))) # no residual standard deviation 
      MDD <- (t.critical * s * sqrt(2/N))
      
      
      
      simulations$noEffectTrusted[i] =  MDD <= simulations$threshold[i]
    }
    else if (simulations$method[i] == "CI") {
      
      test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T)
      CI <- test$conf.int[2] # upper bound
      simulations$noEffectTrusted[i] =  CI <= simulations$threshold[i]
    }
    else if (simulations$method[i] == "EQUIV") {
      
      one_sided_test <- t.test(dat$treatment - simulations$threshold[i], 
                               dat$control, alternative = "less", var.equal = T) 
      
      simulations$noEffectTrusted[i] =   one_sided_test$p.value <= 0.05
    }
    else if (simulations$method[i] == "BFRatio") {
      bt <- BayesFactor::ttestBF(dat$treatment, dat$control, 
                                 nullInterval = c(Inf, 0), var.equal = T,
                                 rscale = "ultrawide" )
      BF <- BayesFactor::extractBF(bt)
      BFRatio <- log10(BF$bf[1]/BF$bf[2])
      #thresholdBF = seq(1,3, len = 20)
      
      simulations$noEffectTrusted[i] =  BFRatio <= simulations$threshold[i]
    }
    
    else if (simulations$method[i] == "Alpha") {
      
      one_sided_test <- t.test(dat$treatment, dat$control, alternative = "less") 
      
      alpha_EQUIV <- one_sided_test$p.value
      
      simulations$noEffectTrusted[i] =  Alpha <= simulations$threshold[i]
      
    }
    else if (simulations$method[i] == "LLR") {
      
      model1 <- lm(dat$treatment ~ dat$control, data = dat)
      model2 <- lm(dat$treatment ~ dat$control + mean(dat$treatment), data = dat)
      
      simulations$noEffectTrusted[i] =  LLR <= simulations$threshold[i]
    }
    return(simulations[i,])
  })

simulations_results = do.call(rbind, results_sim)
stopCluster(cl)

detected = aggregate(effectDetected ~ method + threshold +  trueEffect, 
                     data = simulations_results,
                     FUN = mean)


result = aggregate(noEffectTrusted ~ method + threshold +  trueEffect + effectDetected, 
                   data = simulations_results,
                   FUN = mean)


plotResult <- function(method = "MDD", col = "lightblue") {
  falseTrust <- result[result$method == method & result$effectDetected == F & result$trueEffect > 0, c(2, 5)]
  trueTrust <- result[result$method == method & result$effectDetected == F & result$trueEffect == 0, c(2, 5)]
  lines(falseTrust$noEffectTrusted, 1-trueTrust$noEffectTrusted, type = "b", col = col)
  text(falseTrust$noEffectTrusted - 0.1, 1-trueTrust$noEffectTrusted, labels = round(falseTrust$threshold, digits = 2), col = col)
}

plot(NULL, xlim = c(-0.1, 1), ylim = c(-0.05, 1), type = "l", xlab = "False trust rate ", 
     ylab = "False mistrust rate", bty = "l", las = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2, pch=19)

abline(1,-1)
plotResult(method = "MDD")  
plotResult(method = "CI", col = "red") 
plotResult(method = "EQUIV", col = "green") 
plotResult(method = "BFRatio", col = "darkblue") 
plotResult(method = "Alpha", col = "purple")
plotResult(method = "LLR", col = "orange")
legend("topright", legend=c("MDD", "CI", "EQUIV", "BFRatio", "Alpha", "LLR"), col=c("lightblue","red","green","darkblue","purple", "orange"), lty=c(1,1,1,1), cex=0.4)

# falseTrust_CI <- result[result$method == 'CI' & result$effectDetected == F & result$trueEffect > 0, c(2,5)]
# thresholds = maximum acceptable difference, try: ==alpha
