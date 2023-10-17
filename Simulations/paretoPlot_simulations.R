#' CreateSampleDataset Generates a data frame 
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function generates a data frame called "data" of two samples of continuous values: "treatment" and "control". The control group displays a mean of the value 0 and the treatment group displays a mean of the value of the effect. Both groups are normal distributed and and have the same standard deviation. Each group has 25 observations. 
#' 
#'
CreateSampleDataset <- function(effect, n = 25) {
  control <- rnorm(n, mean = 0, sd = 1)
  treatment <- rnorm(n, mean = effect, sd = 1)
  data <- data.frame(treatment, control)
  return(data)
}


effect = rep(c(0,0.2), each = 50)
dataset = 1:length(effect)

dataSets <- lapply(effect, CreateSampleDataset)

simulations = expand.grid(method = c("MDD", "CI", "EQUIV", "BFRatio", "random_values"), 
                          threshold = seq(0,1, len = 10), dataset = dataset)# alternative: define threshold for each method separately -> less time complexity 


simulations$trueEffect = effect[simulations$dataset]

simulations$effectDetected = NA
simulations$noEffectTrusted = NA

str(simulations)


library(parallel)
library(BayesFactor)

cl <- makeCluster(5)
clusterExport(cl, varlist = list("simulations", "dataSets"))
#clusterEvalQ(cl, {source("MDD_function_Mair.R")})


results_sim =
  parLapply(cl, 1:nrow(simulations), function(i) {
    
    alpha = 0.05
    
    dat = dataSets[[simulations$dataset[i]]]
    N = nrow(dat)
    
    test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T, alpha = alpha)
    simulations$effectDetected[i] <- test$p.value < 0.05
    simulations$pvalue <- test$p.value
    
    if (simulations$method[i] == "MDD") {
      
      varSample = var(c(dat$treatment-mean(dat$treatment),dat$control-mean(dat$control)))
      
      #MDDres <- MDD(N1 = N, 
                   #N2 = N,
                   #variance1 = varSample,
                   #alpha = alpha,
                   #two.sided = F,
                   #var.equal = T)
     #MDD <- MDDres$mdd
      
      
      t.critical <- qt(0.05, df = 2*N-2, lower.tail = FALSE)
      MDD <- (t.critical * sqrt(varSample) * sqrt(2/N))

      simulations$noEffectTrusted[i] =  MDD <= 2*simulations$threshold[i]
    }
    else if (simulations$method[i] == "CI") {
      
      test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T)
      CI <- test$conf.int[2] # upper bound
      simulations$noEffectTrusted[i] =  CI <= 2*simulations$threshold[i]
    }
    else if (simulations$method[i] == "EQUIV") {
      
      one_sided_test <- t.test(dat$treatment - 2*simulations$threshold[i], 
                               dat$control, alternative = "less", var.equal = T) 
      
      simulations$noEffectTrusted[i] =   one_sided_test$p.value <= 0.05
    }
    else if (simulations$method[i] == "BFRatio") {
      bt <- BayesFactor::ttestBF(dat$treatment, dat$control, 
                                 nullInterval = c(Inf, 0), var.equal = T,
                                 rscale = "ultrawide" )
      BF <- BayesFactor::extractBF(bt)
      BFRatio <- log10(BF$bf[1]/BF$bf[2])
  
      simulations$noEffectTrusted[i] =  BFRatio <= 10 * simulations$threshold[i]
    }
    
    else if (simulations$method[i] == "Alpha") {
      
      #one_sided_test <- t.test(dat$treatment, dat$control, alternative = "less") 
      
      #Alpha <- one_sided_test$p.value
      
      #simulations$noEffectTrusted[i] =  Alpha <= simulations$threshold[i]
      
    }
    else if (simulations$method[i] == "random_values") {
      
      random_values = runif(20)
      
      
      simulations$noEffectTrusted[i] =  random_values <= simulations$threshold[i]
    }
    return(simulations[i,])
  })

simulations_results = do.call(rbind, results_sim)
stopCluster(cl)


#detected = aggregate(effectDetected ~ method + threshold +  trueEffect, 
                     #data = simulations_results,
                     #FUN = mean)


result = aggregate(noEffectTrusted ~ method + threshold +  trueEffect + effectDetected,
                   data = simulations_results,
                   FUN = mean)


plotResult <- function(method = "MDD", col = "lightblue") {
  falseTrust <- result[result$method == method & result$effectDetected == F & result$trueEffect > 0, c(2, 5)]
  trueTrust <- result[result$method == method & result$effectDetected == F & result$trueEffect == 0, c(2, 5)]
  lines(falseTrust$noEffectTrusted, 1-trueTrust$noEffectTrusted, type = "b", col = col)
  text(falseTrust$noEffectTrusted - 0.1, 1-trueTrust$noEffectTrusted, labels = round(falseTrust$threshold, digits = 1), col = col, pos = 1)
}

plot(NULL, xlim = c(-0.1, 1), ylim = c(-0.05, 1), type = "l", xlab = "False trust rate ", 
     ylab = "False mistrust rate", bty = "l", las = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2, pch=19)

abline(1,-1)
plotResult(method = "MDD")  
plotResult(method = "CI", col = "red") 
plotResult(method = "EQUIV", col = "green") 
plotResult(method = "BFRatio", col = "darkblue") 
plotResult(method = "random_values", col = "orange")
#plotResult(method = "Alpha", col = "purple")

legend("topright", legend=c("MDD", "CI", "EQUIV", "BFRatio", "Random"
                            ), col=c("lightblue","red","green","darkblue","orange", "purple"), lty=c(1,1,1,1), cex=0.4)


