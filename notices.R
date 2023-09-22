plot(NULL, xlim = c(0,1), ylim = c(0,1))

plotResult<-function(method="MDD", col = 1){
  falseTrust_mdd <- result[result$method == 'MDD' & result$effectDetected == F & result$trueEffect > 0, c(2,5)]
  trueTrust_mdd <- result[result$method == 'MDD' & result$effectDetected == F & result$trueEffect == 0, c(2,5)]
  
  lines(falseTrust_mdd$noEffectTrusted, 1-trueTrust_mdd$noEffectTrusted, type = "b", col = col)
  
  
  
  
  plot(NULL, xlim = c(0,1), ylim = c(0,1))
  
  plotResult<-function(method = "method", col = 1){
    
    method_filter <- result$method == method
    falseTrust <- result[method_filter & result$effectDetected == F & result$trueEffect > 0, c(2, 5)]
    trueTrust <- result[method_filter & result$effectDetected == F & result$trueEffect == 0, c(2, 5)]
    
    lines(falseTrust$noEffectTrusted, 1 - trueTrust$noEffectTrusted, type = "b", col = col)
    
  }
  
  plotResult(method = "CI")
  
  
  ################
  plotResult <- function(method = "MDD") {
    method_filter <- result$method == method
    falseTrust <- result[method_filter & result$effectDetected == F & result$trueEffect > 0, c(2, 5)]
    trueTrust <- result[method_filter & result$effectDetected == F & result$trueEffect == 0, c(2, 5)]
    
    if (method == "MDD") {
      col <- 1
      #legend_label <- "MDD"
    } else if (method == "CI") {
      col <- 2
      #legend_label <- "CI"
    }
    
    lines(falseTrust$noEffectTrusted, 1 - trueTrust$noEffectTrusted, type = "b", col = col)
    #legend("topright", legend = legend_label, col = col, pch = 1, ncol=1)
    
  }
  
  plotResult(method = "MDD")  
  plotResult(method = "CI")   
  
  falseTrust_CI <- result[result$method == 'CI' & result$effectDetected == F & result$trueEffect > 0, c(2,5)]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  