#' CreateSampleDataset - Generates a data frame of two sample groups
#'
#' @author Isabelle Halbhuber
#' 
#' @param effect The mean difference between the treatment and control groups.
#' @param n The number of observations in each group (default: 25).
#'
#' @details This function generates a data frame called "data" consisting of two sample groups: "treatment" and "control." The control group has a mean of 0, while the treatment group has a mean equal to the 'effect' parameter. Both groups follow a normal distribution with the same standard deviation of 1. You can specify the number of observations in each group using the 'n' parameter (default is 25).
#'
#' @return A data frame containing two columns: "treatment" and "control".
#'
CreateSampleDataset <- function(effect, n = 25) {
  control <- rnorm(n, mean = 0, sd = 1)
  treatment <- rnorm(n, mean = effect, sd = 1)
  data <- data.frame(treatment, control)
  return(data)
}


effect = rep(c(0,0.75), length.out = 50) # Create an effect vector with alternating values 0 and 0.75
dataset = 1:length(effect) # Create a dataset vector from 1 to the length of the 'effect' vector


dataSets <- lapply(effect, CreateSampleDataset) # Generate a list of datasets using the 'CreateSampleDataset' function

simulations = expand.grid(method = c("MDD", "CI", "EQUIV", "BFRatio", "random_values", "Alpha"), 
                          threshold = seq(0,1, len = 10), dataset = dataset) # Create a grid of simulation settings 
# Alternative: define threshold for each method separately -> less time complexity 


simulations$trueEffect = effect[simulations$dataset] # Assign the true effect values to the 'simulations' data frame

simulations$effectDetected = NA # Initialize a column for recording whether the effect is detected
simulations$noEffectTrusted = NA # Initialize a column for recording whether no effect is trusted


library(parallel)
library(BayesFactor)

cl <- makeCluster(5) # Create a cluster for parallel processing with 5 nodes
clusterExport(cl, varlist = list("simulations", "dataSets"))
#clusterEvalQ(cl, {source("MDD_function_Mair.R")})


results_sim =
  parLapply(cl, 1:nrow(simulations), function(i) {
    
    alpha = 0.05
    
    dat = dataSets[[simulations$dataset[i]]]
    N = nrow(dat)
    
    # Perform a t-test to determine if the effect is detected
    test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T, alpha = alpha) 
    simulations$effectDetected[i] <- test$p.value < 0.05
    simulations$pvalue <- test$p.value
    
    # Check for various methods such as MDD, CI, EQUIV, BFRatio, and random_values
    if (simulations$method[i] == "MDD") {
      # Calculate MDD (Minimum Detectable Difference)
      
      varSample = var(c(dat$treatment-mean(dat$treatment),dat$control-mean(dat$control)))  
      t.critical <- qt(0.05, df = 2*N-2, lower.tail = FALSE)
      MDD <- (t.critical * sqrt(varSample) * sqrt(2/N))
      
      # Calculate MDD using the MDD Funktion from Mair et al. (2020):
      #MDDres <- MDD(N1 = N, 
                   #N2 = N,
                   #variance1 = varSample,
                   #alpha = alpha,
                   #two.sided = F,
                   #var.equal = T)
     #MDD <- MDDres$mdd
     
      simulations$noEffectTrusted[i] =  MDD <= simulations$threshold[i]
    }
    else if (simulations$method[i] == "CI") {
      # Calculate CI (Confidence Interval)
      
      test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T)
      CI <- test$conf.int[2] # upper bound
      simulations$noEffectTrusted[i] =  CI <= 2*simulations$threshold[i]
    }
    else if (simulations$method[i] == "EQUIV") {
      # Check equivalence using a one-sided test
      
      one_sided_test <- t.test(dat$treatment - 2*simulations$threshold[i], 
                               dat$control, alternative = "less", var.equal = T) 
      
      simulations$noEffectTrusted[i] =   one_sided_test$p.value <= 0.05
    }
    else if (simulations$method[i] == "BFRatio") {
      # Calculate Bayes Factor Ratio and compare it to a threshold
      
      bt <- BayesFactor::ttestBF(dat$treatment, dat$control, 
                                 nullInterval = c(Inf, 0), var.equal = T,
                                 rscale = "ultrawide" )
      BF <- BayesFactor::extractBF(bt)
      BFRatio <- log10(BF$bf[1]/BF$bf[2])
  
      simulations$noEffectTrusted[i] =  BFRatio <= 10 * simulations$threshold[i]
    }
    
    else if (simulations$method[i] == "random_values") {
      # Generate random values and compare to a threshold
      
      random_values = runif(20)
      
      
      simulations$noEffectTrusted[i] =  random_values <= simulations$threshold[i]
    }
    return(simulations[i,])
  })

simulations_results = do.call(rbind, results_sim)
stopCluster(cl)

# Aggregate the results to calculate the False Mistrust Rates (FMR) and False Trust Rates (FTR)

result = aggregate(noEffectTrusted ~ method + threshold +  trueEffect + effectDetected,
                   data = simulations_results,
                   FUN = mean)

#' plotResult - Visualize trust in the "no effect"
#'
#' @author Isabelle Halbhuber
#' 
#' @param method The statistical method to visualize trust for (default: "MDD").
#' @param col The color to use for the plotted line (default: "lightblue").
#'
#' @details This function plots the trust in the "no effect" hypothesis for a given statistical method and color. It takes the method and color as input, retrieves relevant data from the 'result' data frame, and plots a line graph to visualize the FMR against the FTR. The function also labels the graph with threshold values.
#'
#' @return No explicit return value; it plots a graph.
#'
plotResult <- function(method = "MDD", col = "lightblue") {
  falseTrust <- result[result$method == method & result$effectDetected == F & result$trueEffect > 0, c(2, 5)]
  trueTrust <- result[result$method == method & result$effectDetected == F & result$trueEffect == 0, c(2, 5)]
  lines(falseTrust$noEffectTrusted, 1-trueTrust$noEffectTrusted, type = "b", col = col)
  text(falseTrust$noEffectTrusted - 0.1, 1-trueTrust$noEffectTrusted, labels = round(falseTrust$threshold, digits = 1), col = col, pos = 1)
}
# Create a plot to visualize FMR/FTR
plot(NULL, xlim = c(-0.1, 1), ylim = c(-0.05, 1), type = "l", xlab = "False trust rate ", 
     ylab = "False mistrust rate", bty = "l", las = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2, pch=19)

abline(1,-1)
plotResult(method = "MDD")  
plotResult(method = "CI", col = "red") 
plotResult(method = "EQUIV", col = "green") 
plotResult(method = "BFRatio", col = "darkblue") 
plotResult(method = "random_values", col = "orange")

legend("topright", legend=c("MDD", "CI", "EQUIV", "BFRatio", "Random"
                            ), col=c("lightblue","red","green","darkblue","orange"), lty=c(1,1,1,1), cex=0.4)


