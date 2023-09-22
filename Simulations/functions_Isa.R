library(BayesFactor)
library(TOSTER)
library (EnvStats)
source("MDD_function_Mair.R")


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
#'
#'
getMDD <- function(data, threshold= 0.1, alpha = 0.05) {
  
  N = nrow(data)
  
  
  MDD <- MDD(N1 = N, N2 = N, 
             variance1 = var(c(data$control-mean(data$control),data$treatment-mean(data$treatment))),
             alpha=alpha, two.sided = F, var.equal = T)
  
  #if(MDD$mdd<=threshold){
    
   # plot(MDD$mdd)
  #}
  
  return(MDD$mdd <= threshold)


}
# getCI(data)

#' Calculates the Condifence Interval 
#' 
#' @param data a list with two samples of continuous values: "treatment" and "control"
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function calculates the Confidence Interval (CI) based on MLE and Bayes. 
#' 
#'
getCI <- function(data, threshold = 0.1) {
  test <- t.test(data$control, data$treatment, alternative = "less", var.equal = T)
  CI <- test$conf.int[2] # upper bound
  #CI <- mean(data$treatment)-mean(data$control) + 
    #qt(0.05/1, df = 2*N-2, lower.tail = FALSE) * sqrt(var(c(data$control-mean(data$control),data$treatment-mean(data$treatment)))) * sqrt(2/N)
  diff_means <- mean(data$treatment) - mean(data$control)
  sd_errorTR <- var(data$treatment)/sqrt(length(data$treatment))
  sd_errorCR <- var(data$control)/sqrt(length(data$control))
  #CI<- diff_means + qt((1 - 0.05) / 2, df = 22 - 1) * sd_errorTR
  # CI<- diff_means + qt((1 - 0.05) / 1, df = 2*length(data$treatment)-2) * sqrt(sd_errorTR)
  # CI<- diff_means + qt(0.05/ 1, df = 2*length(data$treatment)-2) * sqrt(var(c(data$control-mean(data$control),data$treatment-mean(data$treatment)))) * sqrt(2/N)
  #if(CI<=threshold){
    #plot(CI )
  #}
  
  
  return(CI <= threshold)
}
# getCI(data)

#' Calculates the Equivalence Test 
#' 
#' @param data a list with two samples of continuous values: "treatment" and "control"
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function calculates the equivalence basted on student's t-test and Smallest Effect Size of Interest (SESOI).  
#' 
#'
#'

# getEQIV <- function(data, threshold = 0.1){
#   effectsize = mean(data$treatment) - mean(data$control)
#   equivalece == FALSE
#   test <- t.test(data$control, data$treatment, alternative = "greater", var.equal = T)
# 
#   if (!significant){
#     CI <- test$conf.int
#     
#     out = CI < threshold
#   } 
# 
# if (Delta < threshold < Delta) {
#   equivalence == TRUE
# }
#   return(results)
# }


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

# threshold macht nur, wenn man a priori mit posteriori modell vergleichen möchte 
# macht wenig sinn das in einen Plot reinzubekommen -> weil kein zusammenhang zwischen false positives und false negatives im Fall von bayes factoren 


