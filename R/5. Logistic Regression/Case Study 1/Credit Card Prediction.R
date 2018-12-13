# Logistic Regression
#Problem Statement:
#==================

----------------------------------------------
#Packages Needed
#dplyr
#car

#Install if not exists
#dplyr 
  if("dplyr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("dplyr")
  }

#car
if("car" %in% rownames(installed.packages()) == FALSE) {
  install.packages("car")
}

#use below package
library(dplyr)
library(car)



----------------------------------------------
  
  
#Since the problem statement has not been stated

#better way to set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


ds1 <- read.csv("ds1.csv", stringsAsFactors = F)
ds2 <- read.csv("ds2.csv", stringsAsFactors = F)
ds3 <- read.csv("ds3.csv", stringsAsFactors = F)
ds4 <- read.csv("ds4.csv", stringsAsFactors = F)

glimpse(ds1)
glimpse(ds2)
glimpse(ds3)
glimpse(ds4)

#Now looking at the data we can guess we need to find out if card can be offered to customer.
#Use first 3 sets to as train data set and 4th to validate.

data.temp.set <- as.data.frame(rbind(ds1, ds2, ds3))


output.set  <- ds4

#Create Backup for train.set
data.temp.set.bk <- data.temp.set

#-----------------------------------------------------------

# STEP 3 
# Try to get most of the columns in numeric

#Creating Dummy Variables for demographic_slice
data.temp.set <- data.temp.set %>%
  mutate(DS_AX03efs = as.numeric(demographic_slice=="AX03efs"),
         DS_BWEsk45 = as.numeric(demographic_slice=="BWEsk45"),
         DS_CARDIF2 = as.numeric(demographic_slice=="CARDIF2")) %>%
  select(-demographic_slice)

#Creating Dummy Variables for country_reg
data.temp.set <- data.temp.set %>%
  mutate(CR_East = as.numeric(country_reg=="E")) %>%
  select(-country_reg)

#Creating Dummy Variablesfor ad_exp
data.temp.set <- data.temp.set %>%
  mutate(AE_Yes = as.numeric(ad_exp=="Y")) %>%
  select(-ad_exp)

#Resetting the output variable card_offer as TRUE to be 1 
#and FALSE to be 0
data.temp.set$card_offer[which(data.temp.set$card_offer==TRUE)]=1

#Setting the seed to review
set.seed(2)
#Splitting the data in train and test
s=sample(1:nrow(data.temp.set),0.7*nrow(data.temp.set))
train.temp.set=data.temp.set[s,]
test.set=data.temp.set[-s,]

#Splitting the data in train and validation
s1=sample(1:nrow(train.temp.set),0.7*nrow(train.temp.set))
train.set=train.temp.set[s1,]
val.set=train.temp.set[-s1,]

library(car)
for_vif=lm(card_offer~.-customer_id,data=train.set)
sort(vif(for_vif), decreasing = T)

#Getting the error "there are aliased coefficients in the model"
#alias(lm(card_offer ~. -customer_id, data=train.set))

#Found that the linear variables were DS_CARDIF2 and DS_BWEsk45
#Hence removing DS_CARDIF2
#for_vif=lm(card_offer~. -customer_id -DS_CARDIF2, data=train.set)
#sort(vif(for_vif),decreasing = T)

#Considering VIF to be 4 for the removal of auto corelation variables 
for_vif=lm(card_offer~. -customer_id -imp_cscore, data=train.set)
sort(vif(for_vif),decreasing = T)

fit.set <- train.set %>%
  select(-customer_id, -imp_cscore)

#Applying the glm function
fit <- glm(card_offer~., family="binomial", data=fit.set)

#Stepwise Variable Reduction by checking the quality of model using AIC (Akaikie Information Criteria)
fit <- step(fit)

#Getting the formula of the fit
formula(fit)

#Using the formula to check the significance of the variables
fit1 <- glm(card_offer ~ est_income + hold_bal + pref_cust_prob + RiskScore + 
              imp_crediteval + DS_AX03efs + DS_CARDIF2 + CR_East, data=fit.set, family="binomial")
summary(fit1)


#Repeating all the steps done with the train data for the validation data

library(car)
for_vif=lm(card_offer~.-customer_id,data=val.set)
sort(vif(for_vif), decreasing = T)

#Getting the error "there are aliased coefficients in the model"
#alias(lm(card_offer ~. -customer_id, data=val.set))

#Found that the linear variables were DS_CARDIF2 and DS_BWEsk45
#Hence removing DS_CARDIF2
#for_vif=lm(card_offer~. -customer_id -DS_CARDIF2, data=val.set)
#sort(vif(for_vif),decreasing = T)

#Considering VIF to be 4 for the removal of auto corelation variables 
for_vif=lm(card_offer~. -customer_id -imp_cscore, data=val.set)
sort(vif(for_vif),decreasing = T)

fit.set <- val.set %>%
  select(-customer_id, -imp_cscore)

#Applying the glm function
fit <- glm(card_offer~., family="binomial", data=fit.set)

#Stepwise Variable Reduction by checking the quality of model using AIC (Akaikie Information Criteria)
fit <- step(fit)

#Getting the formula of the fit
formula(fit)

#Using the formula to check the significance of the variables
fit2 <- glm(card_offer ~ est_income + hold_bal + pref_cust_prob + imp_crediteval + 
              DS_AX03efs + DS_CARDIF2 + CR_East, data=fit.set, family="binomial")
summary(fit2)


#Displaying the formulae for fit1 and fit2 to compute the fit_final having common variables from both the formulae
formula(fit1)
formula(fit2)

fit_final <- glm(card_offer ~ est_income + hold_bal + pref_cust_prob + imp_crediteval + 
                   DS_AX03efs + DS_CARDIF2 + CR_East, family="binomial", data=train.set)
summary(fit_final)

#Getting the score
val.set$score <- predict(fit_final, newdata = val.set, type = "response")

#Determining the KS Cutoff

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0,P=0,N=0)
cutoffs=seq(0,1,length=100)
cutoff = cutoffs[3]
for (cutoff in cutoffs){
  predicted=as.numeric(val.set$score>cutoff)
  
  TP=sum(predicted==1 & val.set$card_offer==1)
  FP=sum(predicted==1 & val.set$card_offer==0)
  FN=sum(predicted==0 & val.set$card_offer==1)
  TN=sum(predicted==0 & val.set$card_offer==0)
  P=FN+TP
  N=TN+FP
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN,P,N))
}

# removing the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2)) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  mutate(M=(8*FN+2*FP)/(P+N)) %>%
  select(-P,-N)

test.set$score=predict(fit_final,newdata = test.set,type = "response")

KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))]
KS_cutoff


table(y = test.set$card_offer,cutoff = as.numeric(test.set$score>KS_cutoff))

#Accuracy of the model = (7184+1302)/(7184+490+24+1302) = 94.28%Ac


#Munching data for the output file
#Creating Dummy Variables for demographic_slice
output.set <- output.set %>%
  mutate(DS_AX03efs = as.numeric(demographic_slice=="AX03efs"),
         DS_BWEsk45 = as.numeric(demographic_slice=="BWEsk45"),
         DS_CARDIF2 = as.numeric(demographic_slice=="CARDIF2")) %>%
  select(-demographic_slice)

#Creating Dummy Variables for country_reg
output.set <- output.set %>%
  mutate(CR_East = as.numeric(country_reg=="E")) %>%
  select(-country_reg)

#Creating Dummy Variablesfor ad_exp
output.set <- output.set %>%
  mutate(AE_Yes = as.numeric(ad_exp=="Y")) %>%
  select(-ad_exp)

#Creating the card_offer field to numeric
output.set$card_offer <- as.numeric(output.set$card_offer)

#Resetting the output variable card_offer as TRUE to be 1 and FALSE to be 0
output.set$score=predict(fit_final,newdata = output.set,type = "response")
output.set$card_offer = as.logical(output.set$score>KS_cutoff)
write.csv(output.set, file = "DS4_output.csv")

