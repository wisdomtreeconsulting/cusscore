calculate_score <- function(date){

# Set the working Directory to source
# Need to figure out


param_weights <- read.csv("G:\\Wisdom Tree\\Scoring\\Web Service\\Data_Tables\\ParamWeightMappings.csv")
cust_metadata <- read.csv("G:\\Wisdom Tree\\Scoring\\Web Service\\Data_Tables\\CustomerData.csv")
raw_input <- read.csv("G:\\Wisdom Tree\\Scoring\\Web Service\\Data_Tables\\CustParamMapping.csv")


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



final_scores$SUCCESS_SCORES <- round(0.1*final_scores$FIT+0.5*final_scores$HEALTH + 0.4*final_scores$VALUE)

library(jsonlite)
library(mongolite)

customerCollection <- mongo(collection = "customer_score", db="local")




tryCatch({
  customerCollection$insert(final_scores)

}, warning = function(w) {

}, error = function(e) {

}, finally = {

})

final_output <- customerCollection$find('{"Date":"1-Jul-17"}',sort = '{"SUCCESS_SCORES": -1}',limit = 10,
                                        fields = '{"_id":false,"Cust_Name":true,"FIT": true,"HEALTH" : true,
                                        "VALUE": true,"SUCCESS_SCORES":true,"CustomerID":true}')

#need to find last 7 days success scores for the customer ids
trend_customers <- final_output$CustomerID

trend_scores <- customerCollection$find('{"CustomerID":{"$in":[1001,1002]}}',
                        fields = '{"_id":false,"Cust_Name":false,"FIT": false,"VALUE": false,
                        "Date":false,"HEALTH":false}')

trend_scores <- data.frame(aggregate( SUCCESS_SCORES ~ CustomerID,
                                      data = trend_scores, paste, collapse = ","))

colnames(trend_scores) <- c("CustomerID", "Trend_Scores")


final_output <- merge(final_output, trend_scores, by = "CustomerID",all.x = TRUE)
final_output$Trend_Scores<- ifelse(!is.na(final_output$Trend_Scores)>0, final_output$Trend_Scores,"No Data")

return(toJSON(final_output))
}
