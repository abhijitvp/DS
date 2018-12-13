#Problem Statement:
#==================

#Since the problem statement has not been stated

setwd("C:/Govinda/PU2376/project/Desktop/Desktop/Data Science/Edu Pristine/logisticregressioncodesandmaterial/DATA")
getwd()

library(dplyr)

data.temp.set <- read.csv("train.csv", stringsAsFactors = F)

glimpse(data.temp.set)

shapiro.test(data.temp.set$LoanAmount)



#Creating Dummy Variables for gender
data.temp.set <- data.temp.set %>%
  mutate( gender = as.numeric(Gender=="Male"))%>%
  select(-Gender)

#Creating Dummy Variables for married
data.temp.set <- data.temp.set %>%
  mutate( married = as.numeric(Married=="Yes"))%>%
  select(-Married)

#Creating Dummy Variables for married
data.temp.set$Dependents <- as.numeric(data.temp.set$Dependents)

#Creating Dummy Variables for Education
data.temp.set <- data.temp.set %>%
  mutate( graduate = as.numeric(Education=="Graduate"))%>%
  select(-Education)

#Creating Dummy Variables for Self Employed
data.temp.set <- data.temp.set %>%
  mutate( selfEmployed = as.numeric(Self_Employed=="Yes"))%>%
  select(-Self_Employed)

#Creating Dummy Variables for Self Employed
data.temp.set <- data.temp.set %>%
  mutate( property_area = as.numeric(Property_Area=="Urban"))%>%
  select(-Property_Area)

#Creating Dummy Variables for Loan Status
data.temp.set <- data.temp.set %>%
  mutate( loan_status = as.numeric(Loan_Status=="Y"))%>%
  select(-Loan_Status)

#Creating Dummy Variables for dependant
#data.temp.set <- data.temp.set %>%
  #mutate( dependant_1 = as.numeric(Dependents=="0"),
   #       dependant_1 = as.numeric(dependant_1=="1"),
     #     dependant_1 = as.numeric(dependant_1=="2"),
    #      dependant_1 = as.numeric(dependant_1=="3"))%>%
 # select(-Dependents)

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
for_vif=lm(loan_status~. -Loan_ID,data=train.set)
sort(vif(for_vif),decreasing = T)

#Getting the formula of the fit
formula(fit)

# Get summary of Fit
summary(fit)

fit1 <- glm(loan_status~. -Loan_ID -Dependents -Loan_Amount_Term -gender - CoapplicantIncome -property_area, family = "binomial", data = train.set )

summary(fit2)

# Repeate the steps for the val.set

for_vif=lm(loan_status~. -Loan_ID,data=val.set)
sort(vif(for_vif),decreasing = T)

#Getting the formula of the fit
formula(fit)

# Get summary of Fit
summary(fit)

fit2 <- glm(loan_status~. -Loan_ID -Dependents -ApplicantIncome -graduate - selfEmployed  , family = "binomial", data = val.set )

summary(fit2)


#Displaying the formulae for fit1 and fit2 to compute the fit_final having common variables from both the formulae

formula(fit1)
formula(fit2)

fit_final <- glm(loan_status~ +LoanAmount +Credit_History +married + graduate +selfEmployed, family = "binomial" , data = train.set)


#Getting the score
val.set$score <- predict(fit_final, newdata = val.set, type = "response")

#Determining the KS Cutoff

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0,P=0,N=0)
cutoffs=seq(0,1,length=100)

for (cutoff in cutoffs){
  predicted=as.numeric(val.set$score>cutoff)
  
  TP=sum(predicted==1 & val.set$loan_status==1)
  FP=sum(predicted==1 & val.set$loan_status==0)
  FN=sum(predicted==0 & val.set$loan_status==1)
  TN=sum(predicted==0 & val.set$loan_status==0)
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

library(tidyr)
cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M) %>%
  gather(Criterion,Value,Sn:M) 

ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()

test.set$score=predict(fit_final,newdata = test.set,type = "response")

KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]

table(y = test.set$loan,cutoff = as.numeric(test.set$score>KS_cutoff))

#output of the prediction

output.set <- read.csv("train.csv")

#Creating Dummy Variables for gender
output.set <- output.set %>%
  mutate( gender = as.numeric(Gender=="Male"))%>%
  select(-Gender)

#Creating Dummy Variables for married
output.set <- output.set %>%
  mutate( married = as.numeric(Married=="Yes"))%>%
  select(-Married)

#Creating Dummy Variables for married
output.set$Dependents <- as.numeric(output.set$Dependents)

#Creating Dummy Variables for Education
output.set <- output.set %>%
  mutate( graduate = as.numeric(Education=="Graduate"))%>%
  select(-Education)

#Creating Dummy Variables for Self Employed
output.set <- output.set %>%
  mutate( selfEmployed = as.numeric(Self_Employed=="Yes"))%>%
  select(-Self_Employed)

#Creating Dummy Variables for Self Employed
output.set <- output.set %>%
  mutate( property_area = as.numeric(Property_Area=="Urban"))%>%
  select(-Property_Area)

#Creating Dummy Variables for Loan Status
output.set <- output.set %>%
  mutate( loan_status = as.numeric(Loan_Status=="Y"))%>%
  select(-Loan_Status)

#Creating the card_offer field to numeric
#output.set$loan_offer <- as.numeric(output.set$loan_offer)

#Resetting the output variable card_offer as TRUE to be 1 and FALSE to be 0
output.set$score=predict(fit_final,newdata = output.set,type = "response")
output.set$loan_offer = as.logical(output.set$score>KS_cutoff)
write.csv(output.set, file = "output.csv")