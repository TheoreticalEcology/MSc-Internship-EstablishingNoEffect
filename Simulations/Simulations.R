


#' Generates the dataset
#'
#' @author Isabelle Halbhuber
#' 
#' @details This function generates a dataframe of two samples of continuous values: "treatment" and "control". 
#' 
#'
CreateSampleDataset <- function(effect) {
  treatment <- abs(rnorm(n=100, mean = effect, sd = 1))
  control <- abs(rnorm(n=100, mean = 0, sd = 1))
  data <- data.frame(treatment, control)
  return(data)
}


effect = rep(c(0,0.5), each = 50)
dataset = 1:length(effect)

dat <- lapply(effect, CreateSampleDataset)


simulations = expand.grid(method = c("MDD", "CI"), 
                          threshold = c(0.1,0.2,0.3), dataset = dataset)

simulations$effect = effect[simulations$dataset]


simulations$result = NA

for i in length(simulations){

  if simulations$method[i] == "MDD" {
    simulations$result 
  }  
  
  
}

