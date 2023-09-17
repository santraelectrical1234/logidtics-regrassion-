
##Logistic Regression Case Study 

list.of.packages <- c("caret", "ggplot2","MASS","car","mlogit","caTools","sqldf","Hmisc","aod","BaylorEdPsych","ResourceSelection","pROC","ROCR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)# LOGISTIC MODEL
library(ggplot2)# VISUALIZATION
library(MASS)# VIF CALCULATION
library(car)# VIF CALCULATION
library(mlogit)# LOGISTIC MODEL
library(sqldf)#WOE & IV
library(Hmisc)#WOE & IV
library(aod)#WALD TEST
#library(BaylorEdPsych)#R-SQUARE
library(ResourceSelection)#HOSMER LEMESHOW TEST
library(pROC)#ROC CURVE
library(ROCR)#ROC CURVE
library(caTools)#TRAIN AND TEST SPLIT


Path<-"C:/Users/dipan/OneDrive/Desktop/ivy all/MY_ALL_IVY_ONLYCLASS/R+STATICS/Case study/Case Study-2(logistics)"
setwd(Path)
getwd()


data<-read.csv("Data_for_Logistic_Regression.csv",header = TRUE)
data1=data#To create a backup of original data
head(data1)

str(data1)
summary(data1)
dim(data1)

#Changing into factor
cols_cat<-c("Status_Checking_Acc","Credit_History","Purposre_Credit_Taken","Savings_Acc",
            "Years_At_Present_Employment","Marital_Status_Gender","Other_Debtors_Guarantors",
            "Property","Other_Inst_Plans","Housing","Job","Telephone","Foreign_Worker","Default_On_Payment")
data1[cols_cat]<-lapply(data1[cols_cat],factor)

#-----------------------------------Missing Value Treatment (if any)-------------------------------------------#
data.frame(colSums(is.na(data1)))

#--------------------------Splitting the data into training and test data set------------------------#

set.seed(144)#This is used to produce reproducible results, every time we run the model

spl = sample.split(data1$Default_On_Payment , 0.7)
data.train = subset(data1, spl == TRUE)
str(data.train)
dim(data.train)

data.test = subset(data1, spl == FALSE)
str(data.test)
dim(data.test)

#-------------------------------------Logistic Regression Model Building------------------------------------------#


model <- glm(Default_On_Payment~., data=data.train, family=binomial())# ~.,.-> dot means all valus & binomial means two values
summary(model)



##problems
## Remove the insignificant variable
model <- glm(Default_On_Payment~ Status_Checking_Acc +	Duration_in_Months + Credit_History + Purposre_Credit_Taken + Credit_Amount +	Savings_Acc + Years_At_Present_Employment
             + Inst_Rt_Income + Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs + Property + Age 
             + Other_Inst_Plans + Housing + Num_CC + Job +	Dependents 
             + Telephone+Foreign_Worker, data=data.train, family=binomial())
summary(model)


summary(data1)
##
model <- glm(Default_On_Payment~ Status_Checking_Acc +	Duration_in_Months + I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
             +   + Credit_Amount +	Savings_Acc + I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")
             + Inst_Rt_Income + I(Marital_Status_Gender=="A93")+ Other_Debtors_Guarantors + Current_Address_Yrs + Property + Age 
             + Other_Inst_Plans + Housing + Num_CC + Job +	Dependents 
             + Telephone+Foreign_Worker, data=data.train, family=binomial())
summary(model)

# removing ->Current_Address_Yrs, Job 
model <- glm(Default_On_Payment~ Status_Checking_Acc +	Duration_in_Months + I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
             +   + Credit_Amount +	Savings_Acc + I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")
             + Inst_Rt_Income + I(Marital_Status_Gender=="A93")+ Other_Debtors_Guarantors + Property + Age 
             + Other_Inst_Plans + Housing + Num_CC  +	Dependents 
             + Telephone+Foreign_Worker, data=data.train, family=binomial())
summary(model)



## Remove PropertyA122,PropertyA123,Other_Inst_PlansA142 
model <- glm(Default_On_Payment~ Status_Checking_Acc +	Duration_in_Months + I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
             +   + Credit_Amount +	Savings_Acc + I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")
             + Inst_Rt_Income + I(Marital_Status_Gender=="A93")+ Other_Debtors_Guarantors + I(Property=="A124") + Age 
             + I(Other_Inst_Plans=="A142") + Housing + Num_CC  +	Dependents 
             + Telephone+Foreign_Worker, data=data.train, family=binomial())
summary(model)

## Remove->Other_Debtors_Guarantors,Savings_AccA62 ,Savings_AccA63  
model <- glm(Default_On_Payment~ Status_Checking_Acc +	Duration_in_Months + I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
             +   + Credit_Amount +	I(Savings_Acc=="A64") +	I(Savings_Acc=="A65")+ I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")
             + Inst_Rt_Income + I(Marital_Status_Gender=="A93") + I(Property=="A124") + Age 
             + I(Other_Inst_Plans=="A142") + Housing + Num_CC  +	Dependents 
             + Telephone+Foreign_Worker, data=data.train, family=binomial())
summary(model)

#-----------------------------------------Final Model---------------------------------------#
## Remove  I(PaymentMethod == "Credit card (automatic)")


vif(model) #  vif> 1.75 that means multicolinearity presents,if vif not greater than1.75 no need to change.

# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)



## Remove   MonthlyCharges, tenure
# model <- glm(Default_On_Payment~ 	SeniorCitizen  
#              + I(MultipleLines=="Yes")+ InternetService  +tenure  + I(StreamingTV=="Yes") 
#              + I(StreamingMovies=="Yes") + Contract + PaperlessBilling
#              + I(PaymentMethod=="Electronic check")+MonthlyCharge, data=data.train, family=binomial())
# summary(model)

vif(model)



#------------------------------Checking the overall fitness of the model----------------------------#


#--------------->using Wald Test
wald.test(b=coef(model), Sigma= vcov(model), Terms=1:12)#Here Terms, no. of independent variables in your final train model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0


#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
#significantly improves the model fit or not)


# Difference betweene null deviance and deviance
modelChi <- model$null.deviance - model$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chidf


# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)



#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(model) # deviance residuals
residuals(model, "pearson") # pearson residuals

sum(residuals(model, type = "pearson")^2)
deviance(model)

#########Larger p value indicate good model fit
1-pchisq(deviance(model), df.residual(model))
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies


#####################################################################################################################
# Coefficients (Odds)
model$coefficients
# Coefficients (Odds Ratio)
exp(model$coefficients)#Interpret 
#if the customer has Internet Service = Fiber Optic, then he is 2.53 times more likely to Default_On_Payment

# Variable Importance of the model
varImp(model)#caret packeges

# Predicted Probabilities
prediction <- predict(model,newdata = data.train,type="response")#Classification Model
#prediction <- predict(model,newdata = data.train,probability=TRUE)

prediction

write.csv(prediction,"pred.csv")


rocCurve<- roc(response = data.train$Default_On_Payment, predictor = factor(prediction, ordered =TRUE), 
               levels = rev(levels(data.train$Default_On_Payment)))
data.train$Default_On_Payment <- as.factor(data.train$Default_On_Payment)

#Metrics - Fit Statistics

threshold<-as.numeric(coords(rocCurve,"best")[1])
#predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
predclass <-ifelse(prediction>threshold,1,0) # converted the output 

Confusion <- table(Predicted = predclass,Actual = data.train$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
#Gini Coefficient is the area under the Lorenz Curve (Similiar ROC Curve where final model compared to baseline model)
#Range 0.5 - 0.8

Confusion
sencivity=626/(626+420)
sepcificity=2110/(2110+344)
auc(rocCurve)
plot(rocCurve)





#########################################################################################################################
### KS (Kolmogorov-Smirnov ) statistics calculation
data.train$m1.yhat <- predict(model, data.train, type = "response")
m1.scores <- prediction(data.train$m1.yhat, data.train$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7

############################################################################################################


###################### Residual Analysis ################################################################################

#this is Train data calculation
logistic_data <- data.train

logistic_data$predicted.probabilities<-fitted(model)

logistic_data$standardized.residuals<-rstandard(model)
logistic_data$studentized.residuals<-rstudent(model)
logistic_data$dfbeta<-dfbeta(model)
logistic_data$dffit<-dffits(model)
logistic_data$leverage<-hatvalues(model)

logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
write.csv(logistic_data, "Res.csv")




###########################################   Model has been build  ##############################################
###########################################   Testing on the test dataset  #######################################




# Logistic Regression on full data

modelt <- glm(Default_On_Payment~ Status_Checking_Acc +	Duration_in_Months + I(Credit_History=="A32")+I(Credit_History=="A33")+I(Credit_History=="A34")
             +   + Credit_Amount +	I(Savings_Acc=="A64") +	I(Savings_Acc=="A65")+ I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")
             + Inst_Rt_Income + I(Marital_Status_Gender=="A93") + I(Property=="A124") + Age 
             + I(Other_Inst_Plans=="A142") + Housing + Num_CC  +	Dependents 
             + Telephone+Foreign_Worker,data=data.test, family=binomial())
summary(modelt)
# Removed -> I(Credit_History=="A33"),I(Credit_History == "A32")TRUE,I(Years_At_Present_Employment == "A75")TRUE
#Age,I(Other_Inst_Plans == "A142",Dependents,TelephoneA192  that column 
modelt <- glm(Default_On_Payment~ Status_Checking_Acc +	Duration_in_Months +I(Credit_History=="A34")
              +   + Credit_Amount +	I(Savings_Acc=="A64") +	I(Savings_Acc=="A65")+ I(Years_At_Present_Employment=="A74")
              + Inst_Rt_Income + I(Marital_Status_Gender=="A93") + I(Property=="A124") 
              + Housing + Num_CC  
              + Foreign_Worker,data=data.test, family=binomial())
summary(modelt)


# final model
modelt <- glm(Default_On_Payment~ Status_Checking_Acc +	Duration_in_Months +I(Credit_History=="A34")
              +   + Credit_Amount +	I(Savings_Acc=="A64") +	I(Savings_Acc=="A65")+ I(Years_At_Present_Employment=="A74")
              + Inst_Rt_Income + I(Marital_Status_Gender=="A93") + I(Property=="A124") 
              + Housing + Num_CC  
              + Foreign_Worker,data=data.test, family=binomial())
summary(modelt)



vif(modelt)

# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)



library(car)
library(mlogit)

# Difference between -2LL of Null model and model with variables
modelChi <- modelt$null.deviance - modelt$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modelt$df.null - modelt$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


# Hosmer and Lemeshow R square
R2.hl<-modelChi/modelt$null.deviance
R2.hl


# Cox and Snell R Square (the last number; here is 2000 should be total no. of ovservation)

R.cs <- 1 - exp ((modelt$deviance - modelt$null.deviance) /2000)
R.cs

# Max rescaled R square (Nagelkarke) (the last number; here is 2000 should be total no. of ovservation)

R.n <- R.cs /(1-(exp(-(modelt$null.deviance/2000))))
R.n



######### Lackfit Deviance ######################################################
residuals(modelt) # deviance residuals
residuals(modelt, "pearson") # pearson residuals

sum(residuals(modelt, type = "pearson")^2)
deviance(modelt)

#########Large p value indicate good model fit
1-pchisq(deviance(modelt), df.residual(modelt))

#######################################################################################
#Function - HS Test

hosmerlem <- function (y, yhat, g = 10) {
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)),
                 include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## High p value incidates the model fits well

hosmerlem(y = data.test$Default_On_Payment, yhat = fitted(modelt))
################################################################################################################
# Hosmer and Lemeshow test in a different way
## High p value incidates the model fits well

library(ResourceSelection)
hl <- hoslem.test(data.test$Default_On_Payment, fitted(modelt), g=10)
hl
#####################################################################################################################
# Coefficients (Odds)
modelt$coefficients
# Coefficients (Odds Ratio)
exp(modelt$coefficients)

# Predicted Probabilities
prediction <- predict(modelt,newdata = data.test,type="response")
prediction



rocCurve   <- roc(response = data.test$Default_On_Payment, predictor = prediction, 
                  levels = rev(levels(data.test$Default_On_Payment)))
data.test$Default_On_Payment <- as.factor(data.test$Default_On_Payment)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = prediction,Actual = data.test$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



#########################################################################################################################
### KS statistics calculation
data.test$m1.yhat <- predict(modelt, data.test, type = "response")

library(ROCR)
m1.scores <- prediction(data.test$m1.yhat, data.test$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

############################################################################################################

#########################################################################################################################
###################### Residual Analysis ################################################################################


logistic_data <- data.test

logistic_data$predicted.probabilities<-fitted(modelt)
logistic_data$standardized.residuals<-rstandard(modelt)
logistic_data$studentized.residuals<-rstudent(modelt)
logistic_data$dfbeta<-dfbeta(modelt)
logistic_data$dffit<-dffits(modelt)
logistic_data$leverage<-hatvalues(modelt)




#logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
#write.csv(logistic_data, file = "C://Users//Subhojit//Desktop//Logistic Regression//Prepared by me//pred.csv")




#######################################################################################################




