calculate_score <- function(date){

# Set the working Directory to source
# Need to figure out


param_weights <- read.csv("G:\\Wisdom Tree\\Scoring\\Web Service\\Data_Tables\\ParamWeightMappings.csv")
cust_metadata <- read.csv("G:\\Wisdom Tree\\Scoring\\Web Service\\Data_Tables\\CustomerData.csv")
raw_input <- read.csv("G:\\Wisdom Tree\\Scoring\\Web Service\\Data_Tables\\CustParamMapping.csv")


str(param_weights)
str(cust_metadata)
str(raw_input)

combined_inputs <- merge(x = param_weights, y = raw_input, by=c("cust_id","param_id"))



compute_Scores <- function(){sapply(combined_inputs$value, function(x){

   ifelse(x <= combined_inputs$penalty_range[1],
          x*combined_inputs$static_penalty_weight*combined_inputs$dynamic_penalty_weight,
            ifelse(x <= combined_inputs$base_range[1],
               x*combined_inputs$static_base_weight *combined_inputs$dynamic_base_weight,
                  ifelse(x<=combined_inputs$incentive_range,x,
                        x*combined_inputs$static_incentive_weight*combined_inputs$dynamic_incentive_weight)))
})

}

# Restrict max value to 100
combined_inputs$SCORE <- pmin(compute_Scores(),100)

final_scores <- aggregate(round(SCORE*weight)~ cust_id+Category+date, data = combined_inputs, sum)


colnames(final_scores) <- c("CustomerID","Category","Date","Score")

#Get to desired Format
library(reshape2)
final_scores <- dcast(final_scores, CustomerID+Date~Category,value.var = "Score")
final_scores <- merge(final_scores, cust_metadata, by.x =c("CustomerID"),by.y = c("cust_id"))



final_scores$SUCCESS_SCORES <- 0.1*final_scores$FIT+0.5*final_scores$HEALTH + 0.4*final_scores$VALUE

library(jsonlite)
library(mongolite)

customerCollection <- mongo(collection = "customer_score", db="local")

result <- tryCatch({
  customerCollection$insert(final_scores)

}, warning = function(w) {

}, error = function(e) {

}, finally = {

})

final_output <- customerCollection$find('{"Date":"1-Jul-17"}',sort = '{"SUCCESS_SCORES": -1}',limit = 10,
                                        fields = '{"_id":false,"Cust_Name":true,"FIT": true,"HEALTH" : true,
                                        "VALUE": true,"SUCCESS_SCORES":true}')



return("abc")
}
