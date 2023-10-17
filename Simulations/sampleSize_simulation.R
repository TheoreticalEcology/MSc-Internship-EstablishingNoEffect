#' CreateSampleDataset Generates a data frame 
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function generates a data frame called "data" of two samples of continuous values: "treatment" and "control". The control group displays a mean of the value 0 and the treatment group displays a mean of the value of the effect. Both groups are normal distributed and and have the same standard deviation. Each group has 25 observations. 
#' 

CreateSampleDataset <- function(effect, n = 25) {
  control <- rnorm(n, mean = 0, sd = 1)
  treatment <- rnorm(n, mean = effect, sd = 1)
  data <- data.frame(treatment, control)
  return(data)
}

effect = rep(c(0,0.75), length.out = 125)
dataset = 1:length(effect)
dataSets <- lapply(effect, CreateSampleDataset)

#for (rep in 1:2000) { 
ns = rep(seq(4, 100, by = 4), each = 10)
createDataN = function(n) CreateSampleDataset(effect = effect, n = n)
dataset = ns
dataSets <- lapply(ns, createDataN)
#}

simulations = expand.grid(method = c("MDD", "EQUIV"), 
                          dataset = dataset)
simulations$trueEffect = effect[simulations$dataset]

simulations$effectDetected = NA
simulations$noEffectTrusted = NA

mean_differences <- numeric(length(simulations$dataset))
for (i in 1:length(simulations$dataset)) {
  dat <- dataSets[[simulations$dataset[i]]]
  mean_diff <- mean(dat$treatment) - mean(dat$control)
  mean_differences[i] <- mean_diff
}

average_mean_difference <- mean(mean_differences)

LSD = qt(0.05, df = 2*(nrow(dat))-2, lower.tail = FALSE) * 1 * sqrt((1/nrow(dat)) + (1/nrow(dat))) 
threshold = exp(average_mean_difference - LSD)


library(parallel)
library(BayesFactor)

cl <- makeCluster(5)
clusterExport(cl, varlist = list("simulations", "dataSets", "threshold"))
#clusterEvalQ(cl, source())

results_sim =
  parLapply(cl, 1:nrow(simulations), function(i) {
    alpha = 0.05
    dat = dataSets[[simulations$dataset[i]]]
    N = nrow(dat)
    
    for (rep in 1:100) { 
    
    test <- t.test(dat$treatment, dat$control, alternative = "less", var.equal = T, alpha = alpha)
    simulations$effectDetected[i] <- test$p.value < 0.05
    
    if (simulations$method[i] == "MDD") {
      
      varSample = var(c(dat$treatment-mean(dat$treatment),dat$control-mean(dat$control)))
      
      t.critical <- qt(0.05, df = 2*N-2, lower.tail = FALSE)
      MDD <- (t.critical * sqrt(varSample) * sqrt(2/N))
      
      simulations$noEffectTrusted[i] =  MDD <= threshold
    }
    
    else if (simulations$method[i] == "EQUIV") {
      
      one_sided_test <- t.test(dat$treatment - threshold, 
                               dat$control, alternative = "less", var.equal = T) 
      
      simulations$noEffectTrusted[i] =   one_sided_test$p.value <= 0.05
    }
}
    return(simulations[i,])
  })

simulations_results = do.call(rbind, results_sim)
stopCluster(cl)
tail(simulations_results)


result = aggregate(noEffectTrusted ~ dataset+method,
                   data = simulations_results,
                   FUN = mean)


equiv_data <- subset(result, method == "EQUIV")
plot(equiv_data$dataset, equiv_data$noEffectTrusted, xlim = c(0, 100), ylim = c(0,1), type = "b", col = "darkblue", xlab = "Sample size",main = "Equivalence test", ylab = "Accepted", bty = "l", las = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2, pch=19)

mdd_data <- subset(result, method == "MDD")
plot(mdd_data$dataset, mdd_data$noEffectTrusted, xlim = c(0, 100), ylim = c(0, 1), type = "b", col = "darkblue", xlab = "Sample size", ylab = "Accepted",main = "MDD", bty = "l", las = 1, cex.lab = 1.5, cex.axis = 1.5, lwd = 2, pch=19)

#legend("topright", legend = c("EQUIV", "MDD"), col = c("blue", "red"), pch = c(1, 1))




