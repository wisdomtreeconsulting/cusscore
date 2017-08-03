
calculate_score <- function(fileName){

param_weights <- read.csv("G:\\Wisdom Tree\\Scoring\\Web Service\\weights.csv")

str(param_weights)
names(param_weights)
cols = c(4:15)
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

# Restrict max value to 100
param_weights$SCORE <- pmin(compute_Scores(),100)

final_scores <- aggregate((SCORE*Weight)~ Customer.Id+Category, data = param_weights, sum)
colnames(final_scores) <- c("CustomerID","Category","Score")

#Get to desired Format
library(reshape2)
final_scores <- dcast(final_scores, CustomerID~Category,value.var = "Score")

final_scores$SUCCESS_SCORES <- 0.1*final_scores$FIT+0.5*final_scores$HEALTH + 0.4*final_scores$VALUE
library(jsonlite)
                         
 list(
    message = toJSON(final_scores)
  )

}
