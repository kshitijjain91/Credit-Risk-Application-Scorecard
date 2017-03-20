library(caret)
library(ggplot2)
library(Information)
library(caTools)
library(randomForest)
library(stringr)
library(car)
library(Hmisc)
library(ROCR)

# Load credit bureau and demographic dataset  
credit <- read.csv("Credit Bureau_v1.csv")
demo <- read.csv("Demogs_v1.csv")


# Basic data quality checks
# Check for duplicate values
# Credit bureau
str(credit)
dup <- duplicated(credit$Application.ID)
sum(dup)
# There are 3 duplicate IDs in the credit bureau data, removing them
credit <- credit[!duplicated(credit$Application.ID), ]
str(credit)

# Check for duplicate values in demographic data
str(demo)
dup <- duplicated(demo$Application.ID)
sum(dup)
demo <- demo[!duplicated(demo$Application.ID), ]
str(demo)


# Merging both datasets by a common ID i.e "Application.ID"
master <- merge(credit, demo, by="Application.ID")
str(master)

# Check if performance tags of both data sets are same i.e "Performance.Tag.x" and "Performance.Tag.y" should be same
na_match <- sum(which(is.na(master$Performance.Tag.x)) == which(is.na(master$Performance.Tag.y)))

performance_match <- sum(master$Performance.Tag.x == master$Performance.Tag.y, na.rm=T)
na_match + performance_match == nrow(master)
# conclusion: all na values and data are matching in performance tags
# removing performance tag y
master <- master[, -30]

# Also, the ones with performance tag NA are the rejected population
# Storing it as a separate dataframe
reject_rows <- which(is.na(master$Performance.Tag.x)) 
rejected <- master[reject_rows, ]
str(rejected)
str(master)
nrow(rejected)
rejection_rate <- nrow(rejected)/nrow(master)
rejection_rate*100
approval_rate <- (1-rejection_rate)
approval_rate*100  

# Missing value analysis and imputation
# Let's check the summary 
master <- master[-reject_rows, ]
str(master)
sapply(master, function(x) sum(is.na(x)))


# Information value and WOE calculation
IV <- create_infotables(master[,-1],y="Performance.Tag.x",ncore = 2)
IV$Tables
IV$Summary

# Let's create a dataframe containing IV values of all the variables 
IV_dataframe <- IV$Summary
str(IV_dataframe)


# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter
for(i in 1:nrow(IV_dataframe)){
  
  if (IV_dataframe$IV[i]<0.02){
    IV_dataframe$feedback[i] = "Useless"
    
  } else if(IV_dataframe$IV[i]>=0.02& IV_dataframe$IV[i]<0.1){
    IV_dataframe$feedback[i] = "Weak"
    
  } else if(IV_dataframe$IV[i]>=0.1 & IV_dataframe$IV[i]<0.3){
    IV_dataframe$feedback[i] = "Medium"
    
  }else if(IV_dataframe$IV[i]>=0.3 & IV_dataframe$IV[i]<0.5){
    IV_dataframe$feedback[i] = "Strong"
    
  }else if(IV_dataframe$IV[i]>0.1 & IV_dataframe$IV[i]<0.3){
    IV_dataframe$feedback[i] = "Suspicious"
  }
}

str(IV_dataframe)
IV_dataframe$Variable <- as.factor(IV_dataframe$Variable)
IV_dataframe$feedback <- as.factor(IV_dataframe$feedback)


## Extract "Strong" and "Medium" variables  
imp_vars <- which(IV_dataframe$feedback=="Strong"|IV_dataframe$feedback=="Medium")
k1 <- IV_dataframe[imp_vars, 1]
imp <- which(colnames(master) %in% k1)
str(master)
master <- master[, c(1, imp, 19)]
str(master)
# master now only contains the medium and strong variables

#  Creating a master_woe where I'll replace the values with woe values
master_woe <- master
info_val <- create_infotables(master_woe[, -1], y="Performance.Tag.x", ncore = 2)
info_val$Tables
info_val$Summary

# The most crucial variables seem to be:
#Avgas.CC.Utilization.in.last.12.months
# No.of.trades.opened.in.last.12.months
# No.of.PL.trades.opened.in.last.12.months
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
# Outstanding.Balance
# No.of.times.30.DPD.or.worse.in.last.6.months
# Total.No.of.Trades

# These should appear in the model unless they are collinear
#############################################################

# Let's replace the variables with woe value for model building 
woe_list <- list()

typeof(info_val$Tables)
length(info_val$Tables)

# Extracting only the bins and the woe values of each bin
for(i in 1:length(info_val$Tables)) {
  woe_list[[i]] = cbind(info_val$Tables[[i]][1],info_val$Tables[[i]][4])
}
woe_list
# woe_list is a list of dataframes with each df having the following structure:
woe_list[[6]]
str(woe_list[[6]])

# We now need to replace the values in the master df with the corresponding woe values 
# There are two methods to convert dataset with woe values

# Method 1: By creating a function which takes in the woe_list and a column of master and replaces
# the actual values by woe values

## Method 2. By using a for loop (higher run time) 

## Let's see the function method first. 
woe_function <- function(df, variable) {
  for(i in 1:nrow(df)){
    s <- df[i,1]
    if(s=="NA"){
      replace_by = df[i,2]
      variable[which(is.na(variable))] = replace_by
    } else {
      s <- str_replace_all(s, fixed(" "), "")
      s_list <- strsplit(gsub("\\[|\\]", "", s), split=",")
      n = as.integer(s_list[[1]][[1]])
      m = as.integer(s_list[[1]][[2]])
      
      range <- n:m
      replace_by = df[i,2]
      
      variable[which(variable %in% range)] = replace_by
    }
  }
  return(variable)
}

empty_matrix <- matrix(0, nrow(master_woe), ncol(master_woe))
str(master_woe)
col_replace <- which(!colnames(master_woe) %in% c("Application.ID", "Performance.Tag.x") )
col_replace
for(i in col_replace)
{
  master_woe[, i] = woe_function(df=woe_list[[i-1]], variable=master_woe[,i])  
}

str(master_woe)

# let's create plots of woe for all the variables 
names <- names(info_val$Tables)
names
plots <- list()

for (i in 1:length(names)){
  plots[[i]] <- plot_infotables(info_val, names[i])
}

# Lets see the plots
typeof(plots)
plots



#### Model building steps ####

# 1. Logistic regression model 

set.seed(1)
master_woe <- master_woe[, -1]
split_indices <- sample.split(master_woe$Performance.Tag.x, SplitRatio = 0.70)
train <- master_woe[split_indices, ]
test <- master_woe[!split_indices, ]

# Tag= 1 implies default, 0 implies good
initial_model = glm(Performance.Tag.x ~ ., data = train, family = "binomial")
summary(initial_model)

# Run stepwise feature selection to remove the insignificant independent variables 
best_model_1 = step(initial_model, direction = "both")
summary(best_model_1)


# Checking the variance inflation factor to detect the highly correlated independent variable.
vif(best_model_1)

#  Remove "No.of.times.30.DPD.or.worse.in.last.6.months"

best_model_2 <- glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                      No.of.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                      Total.No.of.Trades, family = "binomial", data = train)

# Checking summary of "best_model_2"
summary(best_model_2)

# Checking VIF of "best_model_2"
vif(best_model_2)


#  Remove "Total.No.of.Trades"

best_model_3 <- glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.12.months + 
                      Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., family = "binomial", data = train)



# Summary "best_model_3"

summary(best_model_3)
vif(best_model_3)



# Final Model for Prediction

final_model <- best_model_3


###############################################################################
############################# Model Evaluation ################################


str(test)
test$Performance.Tag.x <- as.factor(test$Performance.Tag.x) 
predictions_logit <- predict(final_model, newdata = test[, -16], type = "response")
summary(predictions_logit)# returns p(bad)
################################################################################


# Let's find out the optimal probablility cutoff 

perform_fn <- function(cutoff) 

  {
  predicted_response <- as.factor(ifelse(predictions_logit >= cutoff, "1", "0"))
  
  conf <- confusionMatrix(test$Performance.Tag.x,predicted_response, positive = "1")

  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

### Confusion Matrix
predicted_response <- as.factor(ifelse(predictions_logit >= 0.0912, "1", "0"))

summary(predicted_response)

test$Performance.Tag.x <- as.factor(test$Performance.Tag.x)

summary(test$Performance.Tag.x)

conf <- confusionMatrix(test$Performance.Tag.x,predicted_response, positive = "1")

conf

#######################################################################################

#-------------------------------------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0.013,0.10 ,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
}


#######################################################################################

# plotting cutoffs 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    





#######################################################################################

# Random Forest. 


RF_data <- woe_data


# Spliting the bank data in 70:30 ratio

set.seed(101)

RF_data$Performance.Tag.x <- as.factor(ifelse(RF_data$Performance.Tag.x==1,"yes","no"))
split_indices <- sample.split(RF_data$Performance.Tag.x, SplitRatio = 0.70)

train_rf <- RF_data[split_indices, ]

test_rf <- RF_data[!split_indices, ]

nrow(train_rf)/nrow(RF_data)

nrow(test_rf)/nrow(RF_data)



## Model 

library(randomForest)

rf_model <- randomForest( Performance.Tag.x~., data = train_rf, proximity = F, do.trace = T, mtry = 5)
rf_pred <- predict(rf_model, test_rf[, -16], type = "prob")

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag.x, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(0.01,0.2 ,length=100)

OUT_rf = matrix(0,100,3)


for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
  
}

#######################################################################################


# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

predicted_response <- as.factor(ifelse(rf_pred[, 2] >= 0.016, "yes", "no"))
conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag.x, positive = "yes")
conf

#Create 5 equally size fol
set.seed(101)
folds <- cut(seq(1,nrow(woe_data)),breaks=5,labels=FALSE)
woe_data$Performance.Tag.x <- as.factor(woe_data$Performance.Tag.x )


#Perform 5 fold cross validation

auc_combined <- vector()
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- woe_data[testIndexes, ]
  trainData <- woe_data[-testIndexes, ]
  rf <- ranger(Performance.Tag.x ~ ., data = trainData, num.trees = 100,min.node.size = 10, write.forest = TRUE,probability = TRUE,seed=1214133,num.threads = 8)
  fit_test <- predict(rf,data = testData, type = 'response')$predictions[,2]
  pred <- prediction(predictions = fit_test,  testData$Performance.Tag.x) 
  auc.tmp <- performance(pred,"auc");
  auc_combined <- append(auc_combined,as.numeric(auc.tmp@y.values))
}
sprintf("Auc 5-fold %s", round(mean(auc_combined),2))
