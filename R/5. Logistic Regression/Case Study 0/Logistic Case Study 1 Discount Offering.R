# Logisitc Regression

#A financial institution is planning to roll out a stock market trading faciliation service for their existing
#account holders. This service costs significant amount of money for the bank in terms of infra, licensing
#and people cost. To make the serive offering profitable, they charge a percentage base comission on every
#trade transaction. However this is not a unique service offered by them, many of their other competitors are
#offering the same service and at lesser commission some times. To retain or attract people who trade heavily
#on stock market and in turn generate a good commission for institution, they are planning to offer discounts
#as they roll out the service to entire customer base.

#Problem is , that this discount, hampers profits coming from the customers who do not trade in large
#quantities . To tackle this issue , company wants to offer discounts selectively. To be able to do so, they need
#to know which of their customers are going to be heavy traders faor money makers for them.

#To be able to do this, they decided to do a beta run of their service to a small chunk of their customer base
#[approx 10000 people]. For these customers they have manually divided them into two revenue categories 1
#and 2. Revenue one category is the one which are moeny makers for the bank, revenue category 2 are the
#ones which need to be kept out of discount offers.
#We need to use this study's data to build a prediction model which should be able to identify if a customer is
#potentially eligible for discounts [falls In revnue grid category 1]. Lets get the data and begin.

# ----------------------------------------------
#Packages Needed
# tidyr
# ggplot2

#Install required packages
if("tidyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("tidyr")
  }
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ggplot2")
}
if("car" %in% rownames(installed.packages()) == FALSE) {
  install.packages("car")
}
----------------------------------------------
  
  
# Step 1: set working dir
# setwd("D:/Abhijit/WK/Study/Data Science/Logistic Regression")
# getwd()

#better way to set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()  
  
  

rg=read.csv("Existing Base.csv",stringsAsFactors = FALSE)
library(dplyr)
glimpse(rg)

----------------------------------------------

#step 2 : Data Cleaning
table(rg$children)

rg = rg %>%
  mutate(children=ifelse(children=="Zero",0,substr(children,1,1)),
         children=as.numeric(children))

glimpse(rg)


#now age_band columns
table(rg$age_band)

#

# 18-21   22-25   26-30   31-35   36-40   41-45   45-50   51-55   55-60   61-65   65-70     71+ 
#   63     456     927    1061    1134    1112    1359    1052    1047     881     598     410 
# Unknown 
# 55

#there are multiple ways to handle unknow 55.
#One you can omit these 55 rows by saying na.omit
#Replace na with mode i.e 45-50
#best way to see the ditributon of ageband to revenugrid if its matching with anyof the other band then replace it with that

round(prop.table(table(rg$age_band,rg$Revenue.Grid),1),4)

rg=rg %>%
  mutate(a1=as.numeric(substr(age_band,1,2)),
         a2=as.numeric(substr(age_band,4,5)),
         age=ifelse(substr(age_band,1,2)=="71",71,ifelse(age_band=="Unknown",NA,0.5*(a1+a2)))
  ) 
glimpse(rg)
table(rg$age)

rg = rg %>%
  select(-a1,-a2,-age_band) %>%
  na.omit()
glimpse(rg)


#now status
rg = rg %>%
  mutate(status_div=as.numeric(status=="Divorced/Separated"),
         status_partner=as.numeric(status=="Partner"),
         status_single=as.numeric(status=="Single/Never Married")) %>%
  select(-status)

glimpse(rg)

#occupation
table(rg$occupation)

round(prop.table(table(rg$occupation,rg$Revenue.Grid),1),2)
# we will make categories based on ditribution
# combine those categories which has same ditribution

rg=rg %>%
  mutate(occ_BM_prof=as.numeric(occupation %in% c("Business Manager","Professional")),
         occ_Retired=as.numeric(occupation=="Retired"),
         occ_HW=as.numeric(occupation=="Housewife")) %>%
  select(-occupation)

glimpse(rg)

#partner
table(rg$occupation_partner)
round(prop.table(table(rg$occupation_partner,rg$Revenue.Grid),1),2)

#combine categories based on distribution
#so there 0.10,0.11 and 0.12
# we will create 2 categories for 0.10 and 0.12

rg=rg %>%
  mutate(op_1=as.numeric(occupation_partner %in% c("Other","Retired","Unknown")),
         op_2=as.numeric(occupation_partner %in% c("Student","Secretarial/Admin"))) %>%
  select(-occupation_partner)

glimpse(rg)

#home status
table(rg$home_status)

#make 4 categories
#to copy paste the values use unique
unique(rg$home_status)

rg=rg %>%
  mutate(hs_livein=as.numeric(home_status=="Live in Parental Hom"),
         hs_own=as.numeric(home_status=="Own Home"),
         hs_rent_private=as.numeric(home_status=="Rent Privately"),
         hs_rent_council=as.numeric(home_status=="Rent from Council/HA")) %>%
  select(-home_status)
glimpse(rg)


#Family Income
table(rg$family_income)
round(prop.table(table(rg$family_income,rg$Revenue.Grid),1),2)
#categorize it based on family income

rg=rg %>%
  mutate(fi_1=as.numeric(family_income %in%
                           c("< 4,000","< 8,000, >= 4,000")),
         fi_2=as.numeric(family_income %in%
                           c("<12,500, >=10,000","<25,000, >=22,500","<27,500, >=25,000")),
         fi_3=as.numeric(family_income %in%
                           c("<10,000, >= 8,000","<15,000, >=12,500","<20,000, >=17,500",">=35,000")),
         fi_4=as.numeric(family_income %in%
                           c("<17,500, >=15,000","<22,500, >=20,000","<30,000, >=27,500"))
  ) %>%
  select(-family_income)
glimpse(rg)


#self_emplpoyed
table(rg$self_employed)

table(rg$self_employed_partner)

table(rg$gender)

rg=rg %>%
  mutate(self_emp_yes=as.numeric(self_employed=="Yes"),
         self_emp_part_yes=as.numeric(self_employed_partner=="Yes"),
         gender_f=as.numeric(gender=="Female"),
         gender_m=as.numeric(gender=="Male")) %>%
  select(-self_employed,-self_employed_partner,-gender)

glimpse(rg)


table(rg$TVarea)
table(rg$post_code)
table(rg$post_area)
table(rg$region)
# too many values .. you can safely ignore them

rg=rg %>%
  select(-TVarea,-post_code,-post_area,-region)


glimpse(rg)


#now there is business rule
# filter year_last_moved !=0


rg=rg %>% filter(!(year_last_moved==0))
glimpse(rg)


# --------------------------------------
# Now Data Cleansing is Done

#--------------------
# Step 3: Sampling
set.seed(2)
s=sample(1:nrow(rg),0.7*nrow(rg))
rg_trainval=rg[s,]
rg_test=rg[-s,]

s1=sample(1:nrow(rg_trainval),0.7*nrow(rg_trainval))
rg_train=rg_trainval[s1,]
rg_val=rg_trainval[-s1,]


# -----------------------

# Step 4 : Create Model 
# Remove columns one by one using vif
library(car)
for_vif=lm(Revenue.Grid~.-REF_NO,data=rg_train)
sort(vif(for_vif))

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity,data=rg_train)
sort(vif(for_vif))

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative ,data=rg_train)
sort(vif(for_vif))

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity,data=rg_train)
sort(vif(for_vif))

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m,data=rg_train)
sort(vif(for_vif))

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m-hs_own,data=rg_train)
sort(vif(for_vif))

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m-hs_own-fi_3,data=rg_train)
sort(vif(for_vif))

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m-hs_own-fi_3-Portfolio.Balance,data=rg_train)
sort(vif(for_vif))

# all columns are below 10 now

summary(for_vif)




rg_fit=rg_train %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-gender_m,-hs_own,-fi_3,-Portfolio.Balance)
fit=glm(Revenue.Grid~.,family = "binomial",data=rg_fit)

#You get an error that y values or your response should be 0 and 1. In our data they are 1 and 2, lets do that
#conversion and move ahead. [We'll have to redo sampling for this effect to appear across all data]

rg$Revenue.Grid=as.numeric(rg$Revenue.Grid==1)


#again sample
set.seed(2)
s=sample(1:nrow(rg),0.7*nrow(rg))
rg_trainval=rg[s,]
rg_test=rg[-s,]

s1=sample(1:nrow(rg_trainval),0.7*nrow(rg_trainval))
rg_train=rg_trainval[s1,]
rg_val=rg_trainval[-s1,]



# ------------------------
# Create new dataset after removing columns found in vif analysis
rg_fit=rg_train %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-gender_m,-hs_own,-fi_3,-Portfolio.Balance)
fit=glm(Revenue.Grid~.,family = "binomial",data=rg_fit)

#instead of doing summary(fit) and removing one column at a time by looking at *
#you can also use STEP function. It will do all these recursive work and let us know which
#columns to remove

fit=step(fit)

formula(fit)
summary(fit1)

# use it on rg_train and then remove columns one by one looking p values and *s
fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance +
           Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond +
           Home.Loan + Online.Purchase.Amount + status_single + fi_2 +
           fi_4 + self_emp_yes + gender_f , data=rg_train, family = "binomial")
summary(fit1)

#remove status_single
fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance +
           Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond +
           Home.Loan + Online.Purchase.Amount + fi_2 +
           fi_4 + self_emp_yes + gender_f , data=rg_train, family = "binomial")
summary(fit1)

#remove self_emp_yes
fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance +
           Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond +
           Home.Loan + Online.Purchase.Amount + fi_2 +
           fi_4 +gender_f , data=rg_train, family = "binomial")
summary(fit1)

#remove fi_2 and fi_4 having almost same p values 0.134
fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance +
           Personal.Loan + Investment.Tax.Saving.Bond +
           Home.Loan + Online.Purchase.Amount +
           gender_f , data=rg_train, family = "binomial")
summary(fit1)
#all columns are * now 

#Now lets build a model on rg_val dataset using the same method as we did it for rg_train



rg_fit_val=rg_val %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-gender_m,-hs_own,-fi_3,-Portfolio.Balance)

fit=glm(Revenue.Grid~.,family = "binomial",data=rg_fit_val)
fit=step(fit)

summary(fit)

formula(fit)

fit2=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Personal.Loan +
           Investment.Tax.Saving.Bond + Home.Loan + Online.Purchase.Amount +
           occ_Retired + op_2 + self_emp_yes + gender_f,family="binomial",data=rg_fit_val)
summary(fit2)

#remove self_emp_yes
fit2=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Personal.Loan +
           Investment.Tax.Saving.Bond + Home.Loan + Online.Purchase.Amount +
           occ_Retired + op_2 + gender_f,family="binomial",data=rg_fit_val)
summary(fit2)

#remove gender_f
fit2=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Personal.Loan +
           Investment.Tax.Saving.Bond + Home.Loan + Online.Purchase.Amount +
           occ_Retired + op_2,family="binomial",data=rg_fit_val)
summary(fit2)

formula(fit1)

formula(fit2)

#take only common columns to generate fit_final
fit_final=glm(Revenue.Grid ~ Average.Credit.Card.Transaction +
                Balance.Transfer + Term.Deposit + Life.Insurance +
                Medical.Insurance + Personal.Loan + Investment.Tax.Saving.Bond
              + Home.Loan + Online.Purchase.Amount,
              family = "binomial",data=rg_train)
summary(fit_final)

#predict score using final model
rg_val$score=predict(fit_final,newdata=rg_val,type = "response")

glimpse(rg_val)

library(ggplot2)
ggplot(rg_val,aes(y=Revenue.Grid,x=score,color=factor(Revenue.Grid)))+
  geom_point()+geom_jitter()

# ------------------------
# Step 5: Find cutoff
# Now our model is ready and we predicted probability values as well
# Now we want to find out cutoff for probablity values such that below that all
# values would be 0 and above 1
# Don't assume cut off as 0.5 
# there are many methods to determine cut off values like KS, business driven, based on
# performance metrics such as Sensitivity, Specificity, KS, Accuracy, Lift etc
# To calculate these performance metrics first you need to create matrix of basic
# metrics such as TP,FP, FN, TN etc.
# P=TP+FN
# N=TN+FP
# total=P+N

# So we will create a matrix for 0.01 to 1.00 with increment of 0.01 and calculate different 
# metrics on it.. Finally we will decide which metrics to choose to decide cutoff probability
# cutoff  TP   FP  FN   TN
# 2     0.00 218 1893   0    0
# 3     0.01 214 1314   4  579

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0, P=0,N=0)
cutoffs=seq(0,1,by = 0.01)

for (cutoff in cutoffs){
  predicted=as.numeric(rg_val$score>cutoff)
  TP=sum(predicted==1 & rg_val$Revenue.Grid==1)
  FP=sum(predicted==1 & rg_val$Revenue.Grid==0)
  FN=sum(predicted==0 & rg_val$Revenue.Grid==1)
  TN=sum(predicted==0 & rg_val$Revenue.Grid==0)
  P=TP+FN
  N=TN+FP
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN, P, N))
}
# lets remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

# Once we calculate TP,FP, FN, TN then its easier to calculate rest of the metrics
# sensitivity: TP/P
# specificity: TN/N
# KS : |TP/P - FN/N|
# Accuracy: TP+TN/Total
# Lift: (TP/P)/((TP+FP)/Total)

# There could be business rule which is derived with experice 
# e.g. 8FN+2FP/Total

# Now calculate above metrics
cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2),P=FN+TP,N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  mutate(M=(8*FN+2*FP)/(P+N)) %>%
  select(-P,-N)

cutoff_data

#lets visualiz data
library(tidyr)

cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M) %>%
  gather(Criterion,Value,Sn:M)
ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()

#visualise lift separately because of its scale 
cutoff_viz %>%
  filter(Criterion=="Lift") %>%
  ggplot(aes(x=cutoff,y=Value,color=Criterion))+geom_line()



# ---------------
# Now predit score on test data
rg_test$score=predict(fit_final,newdata = rg_test,type = "response")

#Cutoff with minimum KS:
KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff

table(rg_test$Revenue.Grid,as.numeric(rg_test$score>KS_cutoff))


#Cutoff with minimum distance
dist_cutoff=cutoff_data$cutoff[which(cutoff_data$dist==min(cutoff_data$dist))][1]
dist_cutoff

table(rg_test$Revenue.Grid,as.numeric(rg_test$score>dist_cutoff))

#Cutoff with max Accuracy
Acc_cutoff=cutoff_data$cutoff[which(cutoff_data$Accuracy==max(cutoff_data$Accuracy))][1]
Acc_cutoff

table(rg_test$Revenue.Grid,as.numeric(rg_test$score>Acc_cutoff))


# Cutoff with minimum M ( The hypothetical business criterion)
M_cutoff=cutoff_data$cutoff[which(cutoff_data$M==min(cutoff_data$M))][1]
M_cutoff

table(rg_test$Revenue.Grid,as.numeric(rg_test$score>M_cutoff))

glimpse(rg_test)
# You can get cutoff using any method. But rememeber
# If business has given cutoff criteria exclusively then use it
# If not then use KS method by default

#-------------------------------------------------------------

#Step 6 : Predict using Cutoff
# Assume here we are using KS cutoff
# So predict on test data using KS cutoff
rg_test = rg_test %>% 
  mutate (Predicted_Revenue_Grid = as.numeric(score >= KS_cutoff))

table(rg_test$Revenue.Grid)
table(rg_test$Predicted_Revenue_Grid)

# Done!!!!
#-------------------------------------------------------------

