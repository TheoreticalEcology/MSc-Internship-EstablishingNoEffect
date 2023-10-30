#' CreateSampleDataset - Generates a Data Frame of Two Sample Groups
#'
#' @author Isabelle Halbhuber
#' 
#' @param effect The mean difference between the treatment and control groups.
#' @param n The number of observations in each group (default: 25).
#'
#' @details This function generates a data frame called "data" consisting of two sample groups: "treatment" and "control." The control group has a mean of 0, while the treatment group has a mean equal to the 'effect' parameter. Both groups follow a normal distribution with the same standard deviation of 1. You can specify the number of observations in each group using the 'n' parameter (default is 25).
#'
#' @return A data frame containing two columns: "treatment" and "control."
#'

CreateSampleDataset <- function(effect, n = 25) {
  control <- rnorm(n, mean = 0, sd = 1)
  treatment <- rnorm(n, mean = effect, sd = 1)
  data <- data.frame(treatment, control)
  return(data)
}

effect = rep(c(0,2), length.out = 125) # Create an effect vector with alternating values 0 and 2
dataset = 1:length(effect) # Create a dataset index to keep track of the datasets.
dataSets <- lapply(effect, CreateSampleDataset) # Generate a list of datasets using 'CreateSampleDataset' function with the 'effect' values.

ns = rep(seq(4, 100, by = 4), each = 10) # Define a sequence of values for dataset creation.
createDataN = function(n) CreateSampleDataset(effect = effect, n = n) # Create a function 'createDataN' to generate datasets for different 'n' values.
dataset = ns
dataSets <- lapply(ns, createDataN) # Generate datasets with varying values using the 'createDataN' function.


simulations = expand.grid(method = c("MDD", "EQUIV"), 
                          dataset = dataset)  # Create a grid of simulation settings 
simulations$trueEffect = effect[simulations$dataset] # Assign the true effect values to the 'simulations' data frame

simulations$effectDetected = NA # Initialize a column for recording whether the effect is detected
simulations$noEffectTrusted = NA # Initialize a column for recording whether no effect is trusted

# Initialize a numeric vector 'mean_differences' to store the differences between treatment and control group means.
mean_differences <- numeric(length(simulations$dataset)) 
for (i in 1:length(simulations$dataset)) { # Calculate the mean differences for each dataset.
  dat <- dataSets[[simulations$dataset[i]]] # Retrieve the current dataset from 'dataSets' based on the dataset index.
  mean_diff <- mean(dat$treatment) - mean(dat$control) # Calculate the mean difference between 'treatment' and 'control' groups.
  mean_differences[i] <- mean_diff # Store the mean difference in the 'mean_differences' vector.

}

average_mean_difference <- mean(mean_differences) # Calculate the average mean difference across all datasets.

# Calculate the Least Significant Difference (LSD) for the specified confidence level (0.05).
# The LSD is used to determine a threshold for hypothesis testing.
LSD = qt(0.05, df = 2*(nrow(dat))-2, lower.tail = FALSE) * 1 * sqrt((1/nrow(dat)) + (1/nrow(dat))) 
threshold = exp(average_mean_difference - LSD) # Calculate the threshold value for hypothesis testing by subtracting the LSD from the average mean difference.
# Alternatively, you can set the threshold manually by uncommenting the following line.
# threshold = effect * 0.1

library(parallel)
library(BayesFactor)

cl <- makeCluster(5) # Create a cluster for parallel processing with 5 nodes
clusterExport(cl, varlist = list("simulations", "dataSets", "threshold"))

results_sim =
  parLapply(cl, 1:nrow(simulations), function(i) {
    alpha = 0.05
    dat = dataSets[[simulations$dataset[i]]]
    N = nrow(dat)
    
    for (rep in 1:100) { 
    # Perform a t-test to determine if the effect is detected
    test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T, alpha = alpha)
    simulations$effectDetected[i] <- test$p.value < 0.05
    
    if (simulations$method[i] == "MDD") {
      # Calculate MDD (Minimum Detectable Difference)
      varSample = var(c(dat$treatment-mean(dat$treatment),dat$control-mean(dat$control)))
      
      t.critical <- qt(0.05, df = 2*N-2, lower.tail = FALSE)
      MDD <- (t.critical * sqrt(varSample) * sqrt(2/N))
      
      simulations$noEffectTrusted[i] =  MDD <= threshold
    }
    
    else if (simulations$method[i] == "EQUIV") {
      # Check equivalence using a one-sided test
      
      one_sided_test <- t.test(dat$treatment - threshold, 
                               dat$control, alternative = "less", var.equal = T, alpha= 0.2) 
      
      simulations$noEffectTrusted[i] =   one_sided_test$p.value <= 0.05
      
    }
}
    return(simulations[i,])
  })

simulations_results = do.call(rbind, results_sim)
stopCluster(cl)
tail(simulations_results)

# Calculate the mean values of 'noEffectTrusted' for each combination of 'dataset' and 'method'.
result = aggregate(noEffectTrusted ~ dataset+method,
                   data = simulations_results,
                   FUN = mean)

# Extract data for the "EQUIV" method from the result.
equiv_data <- subset(result, method == "EQUIV")
# Plot a graph for the "EQUIV" method's 'noEffectTrusted' values against 'dataset'.
plot(equiv_data$dataset, equiv_data$noEffectTrusted, xlim = c(0, 100), ylim = c(0,1), type = "b", col = "darkblue", xlab = "Sample size",main = "Equivalence test", ylab = "Accepted", bty = "l", las = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2, pch=19)
# Extract data for the "MDD" method from the result.
mdd_data <- subset(result, method == "MDD")
# Plot a graph for the "MDD" method's 'noEffectTrusted' values against 'dataset'.
plot(mdd_data$dataset, mdd_data$noEffectTrusted, xlim = c(0, 100), ylim = c(0, 1), type = "b", col = "darkblue", xlab = "Sample size", ylab = "Accepted",main = "MDD", bty = "l", las = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2, pch=19)




