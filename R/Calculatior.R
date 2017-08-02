
calculate_score <- function(fileName){
library(xlsx)
# param_values <- read.xlsx2("param values for R program.xlsx",sheetName = "Sheet1")
  fileName <- "variables for R program.xlsx"
param_weights <- read.csv(fileName, sheetName = "Sheet1")

str(param_weights)
names(param_weights)
cols = c(3:14)    
param_weights[,cols] =apply(param_weights[,cols], 2, function(x) as.numeric(as.character(x)))


compute_Scores <- function(){sapply(param_weights$PARAM.DATA, function(x){
  
   ifelse(x <= param_weights$Static.Range..penalty.[1],
          x*param_weights$Static.weight..penalty.*param_weights$Dynamic..weight..penalty.,
            ifelse(x <= param_weights$Static.Range..Base.[1],
               x*param_weights$Static.Weight...Base. *param_weights$Dynamic.Weight...Base.,
                  ifelse(x<=param_weights$Static.Range..Incentive.,x,
                        x*param_weights$Static.Weight..Incentive.*param_weights$Dynamic..Weight..Incentive.)))
})

}

param_weights$SCORE <- compute_Scores()

fit_score <- sum(param_weights[param_weights$Category=="FIT",]$SCORE*
  param_weights[param_weights$Category=="FIT",]$Weight)

health_score <- sum(param_weights[param_weights$Category=="HEALTH",]$SCORE*
                   param_weights[param_weights$Category=="HEALTH",]$Weight)

value_score <- sum(param_weights[param_weights$Category=="VALUE",]$SCORE*
                   param_weights[param_weights$Category=="VALUE",]$Weight)

success_score <- 0.1*fit_score+0.5*health_score + 0.4*value_score
final_scores <- cbind(fit_score,health_score,value_score,success_score)
return(fit_score)
}
