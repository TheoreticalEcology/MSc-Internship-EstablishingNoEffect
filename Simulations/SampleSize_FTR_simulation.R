# Number of iterations
num_simulations <- 100

# Generates a list to save the results of the simulation 
simulation_results <- vector("list", num_simulations)

# Loop to conduct the simulation 
for (i in 1:num_simulations) {
  CreateSampleDataset <- function(effect, n = 70) {
    control <- rnorm(n, mean = 0, sd = 0.5)
    treatment <- rnorm(n, mean = effect, sd = 0.5)
    data <- data.frame(treatment, control)
    return(data)
  }
  
  effect <- rep(c(rep(0, each = 2), rep(0.75, each = 2)), length.out = 350)
  ns <- rep(seq(2, 70, by = 2), each = 10)
  
  # Generate datasets with alternating values 0 and 2
  dataSets <- lapply(1:length(effect), function(i) {
    n <- ns[i]
    effect_i <- effect[i]
    CreateSampleDataset(effect_i, n)
  })
  
  # Create a grid of simulation settings 
  simulations <- expand.grid(method = c("MDD", "EQUIV"), dataset = 1:length(ns))
  simulations$trueEffect <- effect
  simulations$dataset_name <- ns
  
  simulations$effectDetected = NA # Initialize a column for recording whether the effect is detected
  simulations$noEffectTrusted = NA # Initialize a column for recording whether the effect is trusted


#########################  Threshold calculation (see statistical considerations of EFSA 2015) ########################    
### Initialize a numeric vector 'mean_differences' to store the differences between treatment and control group means.
###  mean_differences <- numeric(length(simulations$dataset)) 
  
###  for (j in 1:length(simulations$dataset)) { # Calculate the mean differences for each dataset.
###    dat <- dataSets[[simulations$dataset[j]]] # Retrieve the current dataset from 'dataSets' based on the dataset index.
###    mean_diff <- mean(dat$treatment) - mean(dat$control) # Calculate the mean difference between 'treatment' and 'control' groups.
###    mean_differences[j] <- mean_diff # Store the mean difference in the 'mean_differences' vector.
###}
  
### average_mean_difference <- mean(mean_differences) # Calculate the average mean difference across all datasets.
### LSD = qt(0.05, df = 2*(nrow(dat))-2, lower.tail = FALSE) * 1 * sqrt((1/nrow(dat)) + (1/nrow(dat))) # Calculate the Least Significant Difference (LSD) for the specified confidence level (0.05). The LSD is used to determine a threshold for hypothesis testing.
### threshold = exp(average_mean_difference - LSD) # Calculate the threshold value for hypothesis testing by subtracting the LSD from the average mean difference.

######################### Alternatively, you can set the threshold manually (EFSA, 2023)#########################
  threshold = 0.75 - 0.1 * 0.75
  
  library(parallel)
  library(BayesFactor)
  
  cl <- makeCluster(5) 
  clusterExport(cl, varlist = list("simulations", "dataSets", "threshold"))
  
  results_sim =
    parLapply(cl, 1:nrow(simulations), function(j) {
      dat = dataSets[[simulations$dataset[j]]]
      N = nrow(dat)
        
        test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T, alpha = 0.2)
        simulations$effectDetected[j] <- test$p.value < 0.2
        
        if (simulations$method[j] == "MDD") {
          
          varSample = var(c(dat$treatment - mean(dat$treatment), dat$control - mean(dat$control)))
          
          t.critical <- qt(0.05, df = 2*N-2, lower.tail = FALSE)
          MDD <- (t.critical * sqrt(varSample) * sqrt(2/N))
          
          simulations$noEffectTrusted[j] = as.numeric(MDD <= threshold)
        }
        
        else if (simulations$method[j] == "EQUIV") {
          
          one_sided_test <- t.test(dat$treatment, 
                                   dat$control, alternative = "less", var.equal = T, alpha = 0.2, mu = threshold) 
          
          simulations$noEffectTrusted[j] = as.numeric(one_sided_test$p.value <= 0.2)
        }
      return(simulations[j,])
    })
  
  simulations_results = do.call(rbind, results_sim)
  stopCluster(cl)
  
  result = aggregate(noEffectTrusted ~ method +  trueEffect + effectDetected  + dataset_name,
                     data = simulations_results,
                     FUN = mean)
  
  simulation_results[[i]] <- result
}

all_simulation_results <- do.call(rbind, simulation_results)
grouped_results <- aggregate(noEffectTrusted ~ method +  trueEffect + effectDetected + dataset_name,
                             data = all_simulation_results, 
                             FUN = mean)

falseTrust_mdd <- grouped_results[grouped_results$method == "MDD" & grouped_results$effectDetected == F & grouped_results$trueEffect > 0, ]
falseTrust_equiv <- grouped_results[grouped_results$method == "EQUIV" & grouped_results$effectDetected == F & grouped_results$trueEffect > 0, ]


# Plot a graph for the "EQUIV" method's 'noEffectTrusted' values against 'dataset'.
plot(falseTrust_equiv$dataset_name, falseTrust_equiv$noEffectTrusted, xlim = c(2, 70), ylim = c(0,1), type = "l", col = "darkblue", xlab = "Sample size",main = "Equivalence test", ylab = "False Trust Rate", bty = "l", las = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2, pch=19)

# Plot a graph for the "MDD" method's 'noEffectTrusted' values against 'dataset'.
plot(falseTrust_mdd$dataset_name, falseTrust_mdd$noEffectTrusted, xlim = c(2, 70), ylim = c(0, 1), type = "l", col = "darkblue", xlab = "Sample size", ylab = "False Trust Rate",main = "MDD", bty = "l", las = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2, pch=19)


