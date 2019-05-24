library(taRifx)
library(tidyverse)

data <- iris %>% remove.factors()
data <- data[data$Species!="setosa",]
data[data$Species=="versicolor",]$Species <- 1
data[data$Species=="virginica",]$Species <- 0
data$Species <- data$Species %>% as.integer()
data <- cbind(data$Species,data[,1:4])

#logistic_變數篩選
#Y要放在最前面
logistic_feacture <- function(data=data){
  var_table <- as.character()
  begin_name <- colnames(data) #原名
  col_name <- paste0("X",1:(ncol(data)-1))
  col_name <- c("Y",col_name)
  colnames(data) <- col_name
  pb_i <- txtProgressBar(min = 2, max = ncol(data), style = 3)
  for (i in 2:ncol(data))
  {
    formula_describe <- paste0("Y~",colnames(data)[i])
    model <- glm(formula = formula_describe,data = data,family = binomial(link = "logit"))
    beta_var <- round(model$coefficients[2],2)
    real <- data[,1]
    pred <- predict.glm(model,type = "response",newdata = NULL) %>% as.numeric()
    pred <- round(pred,2)
    pred_mse <- (sum((real-pred)^2,2))/length(pred)
    pred_mse <- round(pred_mse,2)
    beta_mse <- round(beta_var/pred_mse,2)
    
    var_temp <- cbind(begin_name[i],beta_var,pred_mse,beta_mse) %>% as.data.frame()
    colnames(var_temp) <- c("term","Beta","Residual_MSE","Beta/MSE") 
    var_table <- rbind(var_table,var_temp)
    setTxtProgressBar(pb_i, i)
  }
  return(var_table)
}

test <- logistic_feacture(data)
