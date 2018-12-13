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


# Step 1: set working dir
setwd("D:/Abhijit/WK/Study/Data Science/Logistic Regression")
getwd()

rg=read.csv("Existing Base.csv",stringsAsFactors = FALSE)
library(dplyr)
glimpse(rg)



#step 2 : Data Cleaning
table(rg$children)

rg = rg %>%
  mutate(children=ifelse(children=="Zero",0,substr(children,1,1)),
         children=as.numeric(children))

table(rg$age_band)

prop.table(table(rg$age_band,rg$Revenue.Grid),1)

rg=rg %>%
  mutate(a1=as.numeric(substr(age_band,1,2)),
         a2=as.numeric(substr(age_band,4,5)),
         age=ifelse(substr(age_band,1,2)=="71",71,ifelse(age_band=="Unknown",NA,0.5*(a1+a2)))
  ) %>%
  select(-a1,-a2,-age_band) %>%
  na.omit()

table(rg$status)

rg = rg %>%
  mutate(status_div=as.numeric(status=="Divorced/Separated"),
         status_partner=as.numeric(status=="Partner"),
         status_single=as.numeric(status=="Single/Never Married")) %>%
  select(-status)

table(rg$occupation)

round(prop.table(table(rg$occupation,rg$Revenue.Grid),1),2)

rg=rg %>%
  mutate(occ_BM_prof=as.numeric(occupation %in% c("Business Manager","Professional")),
         occ_Retired=as.numeric(occupation=="Retired"),
         occ_HW=as.numeric(occupation=="Housewife")) %>%
  select(-occupation)
round(prop.table(table(rg$occupation_partner,rg$Revenue.Grid),1),2)

rg=rg %>%
  mutate(op_1=as.numeric(occupation_partner %in% c("Other","Retired","Unknown")),
         op_2=as.numeric(occupation_partner %in% c("Student","Secretarial/Admin"))) %>%
  select(-occupation_partner)
table(rg$home_status)

unique(rg$home_status)

rg=rg %>%
  mutate(hs_livein=as.numeric(home_status=="Live in Parental Hom"),
         hs_own=as.numeric(home_status=="Own Home"),
         hs_rent_private=as.numeric(home_status=="Rent Privately"),
         hs_rent_council=as.numeric(home_status=="Rent from Council/HA")) %>%
  select(-home_status)


round(prop.table(table(rg$family_income,rg$Revenue.Grid),1),2)

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
table(rg$self_employed)

table(rg$self_employed_partner)

table(rg$gender)

rg=rg %>%
  mutate(self_emp_yes=as.numeric(self_employed=="Yes"),
         self_emp_part_yes=as.numeric(self_employed_partner=="Yes"),
         gender_f=as.numeric(gender=="Female"),
         gender_m=as.numeric(gender=="Male")) %>%
  select(-self_employed,-self_employed_partner,-gender)

rg=rg %>%
  select(-TVarea,-post_code,-post_area,-region)

rg=rg %>%
  filter(!(year_last_moved==0))
glimpse(rg)

set.seed(2)
s=sample(1:nrow(rg),0.7*nrow(rg))
rg_trainval=rg[s,]
rg_test=rg[-s,]

s1=sample(1:nrow(rg_trainval),0.7*nrow(rg_trainval))
rg_train=rg_trainval[s1,]
rg_val=rg_trainval[-s1,]

library(car)
for_vif=lm(Revenue.Grid~.-REF_NO,data=rg_train)
sort(vif(for_vif), decreasing = T)

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity,data=rg_train)
sort(vif(for_vif), decreasing = T)

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative ,data=rg_train)
vif(for_vif)

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity,data=rg_train)
vif(for_vif)

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m,data=rg_train)
vif(for_vif)

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m-hs_own,data=rg_train)
vif(for_vif)

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m-hs_own-fi_3,data=rg_train)
vif(for_vif)

for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative
           -Investment.in.Equity-gender_m-hs_own-fi_3-Portfolio.Balance,data=rg_train)
vif(for_vif)

rg_fit=rg_train %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-gender_m,-hs_own,-fi_3,-Portfolio.Balance)
fit=glm(Revenue.Grid~.,family = "binomial",data=rg_fit)

#You get an error that y values or your response should be 0 and 1. In our data they are 1 and 2, lets do that
#conversion and move ahead. [We'll have to redo sampling for this effect to appear across all data]

rg$Revenue.Grid=as.numeric(rg$Revenue.Grid==1)

set.seed(2)
s=sample(1:nrow(rg),0.7*nrow(rg))
rg_trainval=rg[s,]
rg_test=rg[-s,]

s1=sample(1:nrow(rg_trainval),0.7*nrow(rg_trainval))
rg_train=rg_trainval[s1,]
rg_val=rg_trainval[-s1,]

rg_fit=rg_train %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-gender_m,-hs_own,-fi_3,-Portfolio.Balance)
fit=glm(Revenue.Grid~.,family = "binomial",data=rg_fit)

fit=step(fit)

formula(fit)

fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance +
           Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond +
           Home.Loan + Online.Purchase.Amount + status_single + fi_2 +
           fi_4 + self_emp_yes + gender_f , data=rg_train, family = "binomial")
summary(fit1)

fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance +
           Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond +
           Home.Loan + Online.Purchase.Amount + fi_2 +
           fi_4 + self_emp_yes +gender_f , data=rg_train, family = "binomial")
summary(fit1)

fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance +
           Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond +
           Home.Loan + Online.Purchase.Amount + fi_2 +
           fi_4 +gender_f , data=rg_train, family = "binomial")
summary(fit1)

fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance +
           Personal.Loan + Investment.Tax.Saving.Bond +
           Home.Loan + Online.Purchase.Amount +
           gender_f , data=rg_train, family = "binomial")
summary(fit1)

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

fit2=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Personal.Loan +
           Investment.Tax.Saving.Bond + Home.Loan + Online.Purchase.Amount +
           occ_Retired + op_2 + gender_f,family="binomial",data=rg_fit_val)
summary(fit2)

fit2=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer +
           Term.Deposit + Life.Insurance + Medical.Insurance + Personal.Loan +
           Investment.Tax.Saving.Bond + Home.Loan + Online.Purchase.Amount +
           occ_Retired + op_2,family="binomial",data=rg_fit_val)
summary(fit2)

formula(fit1)

formula(fit2)

fit_final=glm(Revenue.Grid ~ Average.Credit.Card.Transaction +
                Balance.Transfer + Term.Deposit + Life.Insurance +
                Medical.Insurance + Personal.Loan + Investment.Tax.Saving.Bond
              + Home.Loan + Online.Purchase.Amount,
              family = "binomial",data=rg_train)
summary(fit_final)

rg_val$score=predict(fit_final,newdata=rg_val,type = "response")


library(ggplot2)
ggplot(rg_val,aes(y=Revenue.Grid,x=score,color=factor(Revenue.Grid)))+
  geom_point()+geom_jitter()

#cutoff=0.2
#predicted=as.numeric(rg_val$score>cutoff)
#TP=sum(predicted==1 & rg_val$Revenue.Grid==1)
#FP=sum(predicted==1 & rg_val$Revenue.Grid==0)
#FN=sum(predicted==0 & rg_val$Revenue.Grid==1)
#TN=sum(predicted==0 & rg_val$Revenue.Grid==0)
# lets also calculate total number of real positives and negatives in the data
#P=TP+FN
#N=TN+FP
# total number of observations
#total=P+N

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0, P=0,N=0)
cutoffs=seq(0,1,length=100)
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

#lets remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2),P=FN+TP,N=TN+FP) %>%
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


#We'll visualise lift separately because of its scale
cutoff_viz %>%
  filter(Criterion=="Lift") %>%
  ggplot(aes(x=cutoff,y=Value,color=Criterion))+geom_line()

rg_test$score=predict(fit_final,newdata = rg_test,type = "response")


#Cutoff with minimum KS:
KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff

table(rg_test$Revenue.Grid,as.numeric(rg_test$score>KS_cutoff))

