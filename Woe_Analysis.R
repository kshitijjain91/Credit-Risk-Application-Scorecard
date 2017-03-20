library(Information)

##-----------------------DATA PREPARATION ------------------------##

# Load Credit Bureau dataset in working directory 
Credit_bureau <- read.csv("Credit Bureau_v1.csv")
# Demographic dataset
Demogs <- read.csv("Demogs_v1.csv")


# Removing duplicates rows from credit bureau data
Credit_bureau_1 <- Credit_bureau[!duplicated(Credit_bureau$Application.ID), ]
# Removing duplicates rows from Demographics data
Demogs_1 <- Demogs[!duplicated(Demogs$Application.ID), ]


# Merging both the dataset by a common ID i.e "Application.ID"
Credit_Demogs_merge_1 <- merge(Credit_bureau_1,Demogs_1,by="Application.ID")
str(Credit_Demogs_merge_1)

# Creating new column to check performance tag of both column would be same i.e "Performance.Tag.x" and "Performance.Tag.y" should be same
Credit_Demogs_merge_1$New<- ifelse(Credit_Demogs_merge_1$Performance.Tag.x==Credit_Demogs_merge_1$Performance.Tag.y,yes = "Y",no = "N")

# Checking table of New column.
table(Credit_Demogs_merge_1$New) # (It is cleared now that all the obsevations in both the columns are same, thus we can go ahead and remove any one column)
#(Also, you can find that the total rows contain 71292 but the observation )
# Removing "New" and "Performance.Tag.y" column from merged dataset
Credit_Demogs_merge_2<-Credit_Demogs_merge_1[,-c(30:31)]


# Let's check the summary 
summary(Credit_Demogs_merge_2)
str(Credit_Demogs_merge_2)


Credit_Demogs_merge_3 <-subset(Credit_Demogs_merge_2,!is.na(Credit_Demogs_merge_2$Performance.Tag.x))

  



# Information value and WOE calculation
IV <- create_infotables(Credit_Demogs_merge_3[,-1],y="Performance.Tag.x",ncore = 2)


# Let's understand the woe information of each variable

IV$Tables

# Let's create a dataframe contains IV values of all the variables 
IV_dataframe <- IV$Summary



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

# Change to Factor 

IV_dataframe$Variable <- as.factor(IV_dataframe$Variable)


## Extract "Strong" and "Medium" levels 
k1 <- IV_dataframe[which(IV_dataframe$feedback=="Strong"|IV_dataframe$feedback=="Medium"),1]
Credit_Demogs_merge_4 <- Credit_Demogs_merge_3[,colnames(Credit_Demogs_merge_3) %in% k1]


#  For woe analysis
str(Credit_Demogs_merge_4)
Credit_Demogs_merge_4 <- cbind(Credit_Demogs_merge_4,Credit_Demogs_merge_3[,19])
colnames(Credit_Demogs_merge_4)[16] <- "Performance.Tag.x"
IV_1 <- create_infotables(Credit_Demogs_merge_4,y="Performance.Tag.x",ncore = 2)


# Let's understand the woe information of each variable
IV_1$Tables

# Let's create a dataframe contains IV values of All the variables 
IV_1$Summary
#############################################################

# Package "Information takes numeric variables for calculation"
# Let's replace numeric variable to factor and change the level and again convert in
# to numeric back 

woe_table <- Credit_Demogs_merge_4

View(woe_table)

IV_2 <- IV_1


# Now, let's replace the variables with woe value for model building 
matrix_1 <- list()

for(i in 1 :15) {
  
  matrix_1[[i]] = cbind(IV_2$Tables[[i]][1],IV_2$Tables[[i]][4])
  
}
matrix_1

### There are two methods to convert dataset with woe values

## 1. By using Function for doing the same task 
## 2. By using for loop 

## Let's see the Function method first. 

#####################################################################
#####################################################################

library(stringr)
woe_function <- function(matrix , variable) {
  
  for(i in 1 : nrow(matrix)){
    
    k <- matrix
    
    s <- k[i,1]
    
    if(s=="NA"){
      
      replace_by = k[i,2]
      
      variable[which(is.na(variable))] = replace_by
      
    } else {
      
      s <- str_replace_all(s, fixed(" "), "")
      
      s_list <- strsplit(gsub("\\[|\\]", "", s), split=",")
      
      n = as.integer(s_list[[1]][[1]])
      m = as.integer(s_list[[1]][[2]])
      
      range <- n:m
      
      replace_by = k[i,2]
      
      
      variable[which(variable %in% range)] = replace_by
      Credit_Demogs_merge_4[,14]
      
    }
    
  }
  
  return(variable)
}

empty_matrix <- matrix(0,nrow(Credit_Demogs_merge_4),ncol(Credit_Demogs_merge_4))

for(i in 1:ncol(Credit_Demogs_merge_4[,-16]))
{
  
  empty_matrix[,i] = woe_function(matrix_1[[i]],Credit_Demogs_merge_4[,i])  
  
}


woe_data <- data.frame(empty_matrix)



for(i in 1:ncol(woe_data)){
  
  colnames(woe_data)[i] <- colnames(Credit_Demogs_merge_4)[i]
  
}


woe_data$Performance.Tag.x <- Credit_Demogs_merge_4$Performance.Tag.x

# let's create a plot of woe for all the variables 

names <- names(IV$Tables)

plots <- list()

for (i in 1:length(names)){
  
  plots[[i]] <- plot_infotables(IV_2, names[i])
  
}

# Lets see the plots

plots[1:ncol(woe_data)]

##################################################################################

k121 <- woe_data

###################################################################################

#### Model building steps ####

#--------------------------------------------------------------------------------
library(car)
library(Hmisc)
library(ROCR)
library(caret)
library(caTools)

# Develop Logistic Regression Model 

# set the set.seed() function 
set.seed(1)

split_indices <- sample.split(k121$Performance.Tag.x, SplitRatio = 0.70)

train <- k121[split_indices, ]

test <- k121[!split_indices, ]



# Using logistic regression model
# 1 implies default, 0 implies good
initial_model = glm(Performance.Tag.x ~ ., data = train, family = "binomial")

summary(initial_model)

# Run Stepwise model to remove insignificant indipendent variables from model.
best_model_1 = step(initial_model, direction = "both")

# Checking summary of "best_model_1" 
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
