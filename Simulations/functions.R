library(BayesFactor)
library(TOSTER)
library (EnvStats)



#' Calculates the MDD
#' 
#' @param data a list with two samples of continuous values: "treatment" and "control"
#'
#' @author Isabelle Halbhuber
#' 
#' @returns description
#' 
#' @details This function calculates the MDD using the "EnvStats" package  
#' 
#'
#'#erst testen auf signifikanz, wenn nicht dann mdd, dann wenn kleiner als threshold 
getMDD <- function(data, threshold = 0.1, alpha = 0.5) {
  n1 <- length(data$treatment)
  n2 <- length(data$control)
  p2 <- 0.5
  
  mdd <- data.frame()
  
  significant = T # TODSO
  
  MDD LENA
  
  out = MDD < threshold
  out = out & !(significant)
    
  return(out)
}



alpha_list <- seq(0.01, 0.94, len = 20)
mdd_results <- getMDD(data, alpha_list)
print(mdd_results)




#' Calculates the MDD
#' 
#' @param data a list with two samples of continuous values: "treatment" and "control"
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function calculates the Confidence Interval (CI) based on MLE and Bayes. 
#' 
#'
getCI <- function(data, threshold = seq(0, 1, len = 20), method = c("MLE", "Bayes")) {
  if (method[1] == "MLE") {
    diff_means <- mean(data$treatment) - mean(data$control)
    sd_errorTR <- var(data$treatment)/sqrt(length(data$treatment))
    sd_errorCR <- var(data$control)/sqrt(length(data$control))
    ci_lowerTR <- diff_means - qt((1 - threshold) / 2, df = 2 - 1) * sd_errorTR
    ci_upperTR <- diff_means + qt((1 - threshold) / 2, df = 2 - 1) * sd_errorTR
    ci_lowerCR <- diff_means - qt((1 - threshold) / 2, df = 2 - 1) * sd_errorCR
    ci_upperCR <- diff_means + qt((1 - threshold) / 2, df = 2 - 1) * sd_errorCR
    
    results <- data.frame(
      Threshold = threshold,
      CI_lowerTR = ci_lowerTR,
      CI_upperTR = ci_upperTR,
      CI_lowerCR = ci_lowerCR,
      CI_upperCR = ci_upperCR
    )
    
    
  } else if (method[1] == "Bayes") {
    bt <- ttestBF(data$control, data$treatment, nullInterval = threshold)
  }
  
  return(list(results = results, ttestBF = bt))
}

CI <- getCI(data)
print(CI)


#' Calculates the MDD
#' 
#' @param data a list with two samples of continuous values: "treatment" and "control"
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function calculates the equivalence basted on student's t-test and Smallest Effect Size of Interest (SESOI).  
#' 
#'
getEQIV <- function(data, threshold = seq(0,1,len = 20)){
  alpha <- threshold 
  t_test_result <- t.test(data$treatment, data$control, conf.level = 1-threshold)
  #sample_mean <- t_test_result$estimate
  confidence_interval <- t_test_result$conf.int
  Delta = mean(data$treatment) - mean(data$control)
  DeltaL = -SESOI
  DeltaU = SESOI
  equivalece == FALSE
  
  
if (Delta + DeltaL) < confidence_interval < (Delta + DeltaU) {
  equivalence == TRUE
}
    
    
  results <- data.frame(
    Threshold = threshold,
    equivalence = equivalence)
  
  
  return(results)
}

results <- getEQIV(data)
results

# könnte man als SESOI den 


#' Calculates the MDD
#' 
#' @param data a list with two samples of continuous values: "treatment" and "control"
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function calculates Bayes Factor using the "BayesFactor" package. 
#' 
#'
getBF <- function(data){
  bt <- ttestBF(data$control, data$treatment, nullInterval = c(0, Inf))
  return(bt)
}
BF <- getBF(data)
BF
# threshold macht nur, wenn man a priori mit posteriori modell vergleichen möchte 
# macht wenig sinn das in einen Plot reinzubekommen -> weil kein zusammenhang zwischen false positives und false negatives im Fall von bayes factoren 


######## Generate a matriX ###########

matrix(c(mdd_results,CI), nrow= 20, ncol=7, byrow=TRUE)
print(matrix)
