library(h2o)
library(dplyr)
h2o.init(max_mem_size = "10G", nthreads = -1)

train.h2o <- h2o.importFile("D:/prueba/Train_1.csv")
y <- "CHURN"

train.h2o[,y] <- as.factor(train.h2o[,y])


splits <- h2o.splitFrame(train.h2o, c(0.7), seed=1234)
train <- h2o.assign(splits[[1]], "train")   
test <- h2o.assign(splits[[2]], "test") 


y <- "CHURN"
x <- c( "accountlength"    ,         
        "internationalplan"    ,      "voicemailplan"        ,     
        "numbervmailmessages"  ,      "totaldayminutes"     ,      
        "daycalls"              ,     "totaldaycharge"     ,       
        "totaleveminutes"        ,    "totalevecalls"     ,        
        "totalevecharge"          ,   "totalnightminutes",         
        "totalnightcalls"          ,  "totalnightcharge",          
        "totalintlminutes"          , "totalintlcalls",            
        "totalintlcharge"            ,"numbercustomerservicecalls")


Modelo = h2o.gbm(x,y,train)


x_test <- as.data.frame(test)[, x]

# make response variable numeric binary vector
y_test <- as.vector(as.numeric(as.character(test$CHURN)))
head(y_test)

pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}

pred(Modelo, x_test) %>% head()

explainer_gbm <- explain(
  model = Modelo,
  data = x_test,
  y = y_test,
  predict_function = pred,
  label = "h2o gbm"
)

varimp_automl <- variable_importance(explainer_gbm)
h2o.varimp_plot(Modelo)

pdp_gbm_1  <- variable_response(explainer_gbm, variable =  "totaldayminutes", type = "pdp")
plot(pdp_gbm_1)

ale_gbm_1  <- variable_response(explainer_gbm, variable =  "totaldayminutes", type = "ale")
plot(ale_gbm_1)



pdp_gbm_2 <- variable_response(explainer_gbm, variable =  "numbercustomerservicecalls", type = "pdp")
plot(pdp_gbm_2)

ale_gbm_2  <- variable_response(explainer_gbm, variable =  "numbercustomerservicecalls", type = "ale")
plot(ale_gbm_2)









#__________________________________________________________________________________________________________


library("ingredients")
selected_passangers <- select_sample(as.data.frame(test[,x]), n = 1000)

cp_rf <- ceteris_paribus(explainer_gbm, selected_passangers)
pdp_Sex_rf <- aggregate_profiles(cp_rf, variables = "totaldayminutes")
plot(pdp_Sex_rf)



pdp_Sex_rf_2 <- aggregate_profiles(cp_rf, variables = "totaldayminutes",
                                 groups = "voicemailplan")

plot(pdp_Sex_rf_2)
