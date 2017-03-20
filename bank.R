##--------------------------------------------------------BFS Capstone Project----------------------------------------------##
library(caret)
library(mass)
library(car)
library(caTools)
library(sqldf)

##-----------------------DATA PREPARATION ------------------------##

# Load Credit Bureau dataset 
Credit_bureau <- read.csv("Credit Bureau_v1.csv")
str(Credit_bureau)
length(unique(Credit_bureau$Application.ID))
which(duplicated(Credit_bureau$Application.ID))

# Removing duplicates rows from credit bureau data
Credit_bureau_1 <- Credit_bureau[!duplicated(Credit_bureau$Application.ID), ]

# Load Demographic data
Demogs <- read.csv("Demogs_v1.csv")
str(Demogs)

# Removing duplicates rows from Demographics data
length(unique(Demogs$Application.ID))
Demogs_1 <- Demogs[!duplicated(Demogs$Application.ID), ]

# Merging both the dataset by a common ID i.e "Application.ID"
Credit_Demogs_merge_1 <- merge(Credit_bureau_1,Demogs_1,by="Application.ID")
str(Credit_Demogs_merge_1)
table(Credit_Demogs_merge_1$Performance.Tag.x, Credit_Demogs_merge_1$Performance.Tag.y)
# the performance tags match perfectly


# Removing "New" and "Performance.Tag.y" column from merged dataset
Credit_Demogs_merge_2 <- Credit_Demogs_merge_1[, -30]
str(Credit_Demogs_merge_2)

# Now, let's check the summary 
summary(Credit_Demogs_merge_2)
sapply(Credit_Demogs_merge_2, function(x) sum(is.na(x)))
# you can see that the "Presence.of.open.home.loan" and "Outstanding.Balance" are having same number of NA's = 272


# Let's first check whether both columns contain same number of NA's are not 

Check_NA <- subset(Credit_Demogs_merge_2, is.na(Credit_Demogs_merge_2$Avgas.CC.Utilization.in.last.12.months)& is.na(Credit_Demogs_merge_2$Presence.of.open.home.loan))
Check_NA
#( So, yes, both the columns contain same number of NA's)

# So,now let's remove these observation from merged data file and store it to "Credit_Demogs_merge_3"
Credit_Demogs_merge_3 <- subset(Credit_Demogs_merge_2,!is.na(Credit_Demogs_merge_2$Avgas.CC.Utilization.in.last.12.months)& !is.na(Credit_Demogs_merge_2$Presence.of.open.home.loan |Credit_Demogs_merge_2$Performance.Tag.x))

# Data cleaning
summary(Credit_Demogs_merge_3)

## Detecting Outliers, NAs etc 
# Age
quantile(Credit_Demogs_merge_3$Age, probs = seq(0, 1, 0.01))
Credit_Demogs_merge_3$Age[which(Credit_Demogs_merge_3$Age < 27)] <- 27

# Gender
summary(Credit_Demogs_merge_3$Gender)
levels(Credit_Demogs_merge_3$Gender)[1] <-"M"

# Marital Status
summary(Credit_Demogs_merge_3$Marital.Status..at.the.time.of.application.)
levels(Credit_Demogs_merge_3$Marital.Status..at.the.time.of.application.)[1]<- "Married"

# Profession
summary(Credit_Demogs_merge_3$Profession)
levels(Credit_Demogs_merge_3$Profession)[1] <-"SAL"

# No of dependents
summary(Credit_Demogs_merge_3$No.of.dependents)
Credit_Demogs_merge_3$No.of.dependents[is.na(Credit_Demogs_merge_3$No.of.dependents)] <- 3

# Income
summary(Credit_Demogs_merge_3$Income)
quantile(Credit_Demogs_merge_3$Income, probs=seq(0, 1, 0.01))
Credit_Demogs_merge_3$Income[which(Credit_Demogs_merge_3$Income < 4.5)] <- 4.5


# Education
summary(Credit_Demogs_merge_3$Education)
levels(Credit_Demogs_merge_3$Education)[1] <- "Professional"

#Type of residence
summary(Credit_Demogs_merge_3$Type.of.residence)
levels(Credit_Demogs_merge_3$Type.of.residence)[1] <- "Rented"

#Performance Tag
summary(Credit_Demogs_merge_3$Performance.Tag.x)
sum(is.na(Credit_Demogs_merge_3$Performance.Tag.x))

colnames(Credit_Demogs_merge_6)[19] <- "Performance.Tag"


##--Exploratory Data Analysis------------------------------------------------------#

# Structure of Credit_Demogs_merge_6 dataset
str(Credit_Demogs_merge_6)

# Checking 6 month DPD data
table(Credit_Demogs_merge_6$No.of.times.90.DPD.or.worse.in.last.6.months,Credit_Demogs_merge_6$No.of.times.60.DPD.or.worse.in.last.6.months,Credit_Demogs_merge_6$No.of.times.30.DPD.or.worse.in.last.6.months)



#--------------------------------------------------------------------------------
library(car)
library(Hmisc)
library(ROCR)
library(caret)
library(caTools)

##---------Logistic Regression Model-------------------------------------------

# Converting into dummy variables

levels(Credit_Demogs_merge_6$Gender)[1] <- "1"
levels(Credit_Demogs_merge_6$Gender)[2] <-"0"
Credit_Demogs_merge_6$Gender <- as.numeric(levels(Credit_Demogs_merge_6$Gender))[Credit_Demogs_merge_6$Gender]

levels(Credit_Demogs_merge_6$Marital.Status..at.the.time.of.application.)[1]<-"1"
levels(Credit_Demogs_merge_6$Marital.Status..at.the.time.of.application.)[2]<-"0"
Credit_Demogs_merge_6$Marital.Status..at.the.time.of.application. <- as.numeric(levels(Credit_Demogs_merge_6$Marital.Status..at.the.time.of.application.))[Credit_Demogs_merge_6$Marital.Status..at.the.time.of.application.]

dummy_1 <- data.frame(model.matrix( ~Education, data = Credit_Demogs_merge_6))
dummy_1<-dummy_1[,-1]

dummy_2 <- data.frame(model.matrix( ~Profession, data = Credit_Demogs_merge_6))
dummy_2<-dummy_2[,-1]

dummy_3 <- data.frame(model.matrix( ~Type.of.residence, data = Credit_Demogs_merge_6))
dummy_3<-dummy_3[,-1]


# Combining all the numeric columns 
Credit_Demogs_merge_7<- cbind(Credit_Demogs_merge_6[ , -c(25:27)], dummy_1,dummy_2,dummy_3)

# set the set.seed() function 
set.seed(200)
s=sample(1:nrow(Credit_Demogs_merge_7),0.7*nrow(Credit_Demogs_merge_7))
train = Credit_Demogs_merge_7[s,]
test = Credit_Demogs_merge_7[-s,]

# Using logistic regression model
initial_model = glm(Performance.Tag ~ ., data = train, family = "binomial")
summary(initial_model)

# Run Stepwise model to remove insignificant independent variables from model.
best_model = step(initial_model, direction = "both", k = 2)

# Checking summary of "best_model" 
summary(best_model)

# Checking the variance inflation factor to detect the highly correlated independent variable.
vif(best_model)

# Remove "No.of.times.30.DPD.or.worse.in.last.6.months" attribute from the model.
best_model_1 <-glm(formula = Performance.Tag ~ No.of.times.60.DPD.or.worse.in.last.6.months +
               No.of.times.90.DPD.or.worse.in.last.12.months +
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
               No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
               Total.No.of.Trades + Gender + Income + No.of.months.in.current.residence +
               No.of.months.in.current.company + EducationOthers, family = "binomial",
              data = train)

# Checking summary of "best_model_1"
summary(best_model_1)

# Checking VIF of "best_model_1"
vif(best_model_1)

# Remove "No.of.times.60.DPD.or.worse.in.last.6.months" attribute is not much significant as well as having high VIF so, 
# let's remove this:
best_model_2 <-glm(formula = Performance.Tag ~
               No.of.times.90.DPD.or.worse.in.last.12.months +
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
               No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
               Total.No.of.Trades + Gender + Income + No.of.months.in.current.residence +
               No.of.months.in.current.company + EducationOthers, family = "binomial",
               data = train)

# Summary of best_model_2
summary(best_model_2)

# Checking VIF of "best_model_2" model
vif(best_model_2)

# Same here, remove "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." variable from model.

best_model_3 <-glm(formula = Performance.Tag ~
               No.of.times.90.DPD.or.worse.in.last.12.months +
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
               No.of.PL.trades.opened.in.last.12.months +
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
               Total.No.of.Trades + Gender + Income + No.of.months.in.current.residence +
               No.of.months.in.current.company + EducationOthers, family = "binomial",
               data = train)

# Check summary of "best_model_3"
summary(best_model_3)

# Check VIF of "best_model_3"
vif(best_model_3)

# Remove "No.of.times.90.DPD.or.worse.in.last.12.months " variable.

best_model_4 <-glm(formula = Performance.Tag ~
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
               No.of.PL.trades.opened.in.last.12.months +
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
               Total.No.of.Trades + Gender + Income + No.of.months.in.current.residence +
               No.of.months.in.current.company + EducationOthers, family = "binomial",
               data = train)

# Let's see the summary of "best_model_4" 
summary(best_model_4)

# VIF of "best_model_4"
vif(best_model_4)

# Remove "No.of.times.90.DPD.or.worse.in.last.12.months" variable.

best_model_5 <-glm(formula = Performance.Tag ~
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
               No.of.PL.trades.opened.in.last.12.months +
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
               Total.No.of.Trades +  Income + No.of.months.in.current.residence +
               No.of.months.in.current.company + EducationOthers, family = "binomial",
               data = train)

# Checking summary
summary(best_model_5)

# Checking VIF of best_model_5
vif(best_model_5)

# Remove "No.of.months.in.current.residence "
best_model_6 <-glm(formula = Performance.Tag ~
              No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
              No.of.PL.trades.opened.in.last.12.months +
              No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
              Total.No.of.Trades +  Income  +
              No.of.months.in.current.company + EducationOthers, family = "binomial",
              data = train)

summary(best_model_6)
vif(best_model_6)

# Remove "EducationOthers"
best_model_7 <-glm(formula = Performance.Tag ~
              No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
              No.of.PL.trades.opened.in.last.12.months +
              No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
              Total.No.of.Trades +  Income  +
              No.of.months.in.current.company , family = "binomial",
              data = train)

# summary of best_model_7 
summary(best_model_7)

# Remove "Income"
best_model_8 <-glm(formula = Performance.Tag ~
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
               No.of.PL.trades.opened.in.last.12.months +
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
               Total.No.of.Trades +
               No.of.months.in.current.company , family = "binomial",
               data = train)

summary(best_model_8)

# Removing "No.of.months.in.current.company " variable.

best_model_9 <-glm(formula = Performance.Tag ~
               No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months +
               No.of.PL.trades.opened.in.last.12.months +
               No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. +
               Total.No.of.Trades, family = "binomial",
               data = train)

# Summary "best_model_9"
summary(best_model_9)

# Final model 
final_model <- best_model_9

##---------- Model Evaluation--------------------------------------------
# predicting the responses on training data
# type = "response" returns the log odds of default and allows concordance measurement

train$predicted_prob <- predict(final_model, train, type = "response")

rcorr.cens(train$predicted_prob,train$Performance.Tag)

test$predicted_prob <- predict(final_model, test, type = "response")

rcorr.cens(test$predicted_prob, test$Performance.Tag)

model_score <- prediction(train$predicted_prob,train$Performance.Tag)

model_perf <- performance(model_score, "tpr", "fpr")

plot(model_perf)

# KS Statistic
ks_table <- attr(model_perf, "y.values")[[1]] - 
  (attr(model_perf, "x.values")[[1]])

ks = max(ks_table)
ks
which(ks_table== ks)


## Model Test dataset-----------------------------------------------

model_score_test <- prediction(test$predicted_prob,test$Performance.Tag)

model_perf_test <- performance(model_score_test, "tpr", "fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - 
  (attr(model_perf_test, "x.values")[[1]])

ks_test = max(ks_table_test)
ks_test
which(ks_table_test== ks_test)

threshold = 0.35

# Threshold Value and confusionMatrix
pred_cf = predict(final_model, newdata = test[,-19], type = "response")
table(pred_cf > threshold, test$Performance.Tag)

# trying different thresholds
# Accuracy, specificity and sensitivity can be calculated from the table
table(pred_cf > 0.2, test$Performance.Tag)
table(pred_cf > 0.5, test$Performance.Tag)


#Confusion matrix
confusionMatrix(as.numeric(train$predicted_prob>0.35),train$Performance.Tag)

confusionMatrix(as.numeric(test$predicted_prob>0.5),test$Performance.Tag)
