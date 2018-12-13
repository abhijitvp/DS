#Linear Regrassion Case Study

#Packages Needed
#dplyr
#car
if("dplyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("dplyr")
}

if("car" %in% rownames(installed.packages()) == FALSE) {
  install.packages("car")
}

#use below package
library(dplyr)
library(car)



# STEP 1
# set working directory
# its folder path
# not file path

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#-----------------------------------------------------------
# STEP 2
# Read the file
# You must read file as stringsAsFactors = FALSE
ld = read.csv("loans data.csv", stringsAsFactors = FALSE)

#check loaded data
str(ld)

#dplyr has glimps function similar to str for checking structure
glimpse(ld)


#-----------------------------------------------------------

# STEP 3 
# Try to get most of the columns in numeric

#View the columns what values it consists
table(ld$Amount.Requested) #here you can see 4 number of dots. 

#make it numeric
ld$Amount.Requested = as.numeric(ld$Amount.Requested)
#check data and datatype now
table(ld$Amount.Requested)
glimpse(ld)


table(ld$Amount.Funded.By.Investors)
table(ld$Interest.Rate)
table(ld$Debt.To.Income.Ratio)
table(ld$Monthly.Income)
table(ld$Open.CREDIT.Lines)
table(ld$Revolving.CREDIT.Balance)
table(ld$Loan.Length)

#other way all columns at once
ld = ld %>% mutate(Amount.Funded.By.Investors = as.numeric(Amount.Funded.By.Investors),
                   Interest.Rate = as.numeric(gsub("%","",Interest.Rate)),
                   Debt.To.Income.Ratio = as.numeric(gsub("%","",Debt.To.Income.Ratio)),
                   Monthly.Income = as.numeric(Monthly.Income),
                   Open.CREDIT.Lines = as.numeric(Open.CREDIT.Lines),
                   Revolving.CREDIT.Balance = as.numeric(Revolving.CREDIT.Balance)
                   )

#--------------------------------------------------------
  
#STEP 4 : character cleaning
# if the data availble now will not be avaible in future then there is no point keeping  data
# here Amount funded by investor data may not be avaible on real data as being sensitive. 
# Hence we are removing it 
ld %>% select(-Amount.Funded.By.Investors)

glimpse(ld)

#--------------------------------------------------------



#STEP 5 : Feature Engineering
# symbols
# range
# numbers+chars

#FICO.Range has value like 735-739
# We will take avg instead of range
table(ld$FICO.Range)

ld = ld %>% mutate(f1 = as.numeric(substr(FICO.Range,1,3)),
                   f2 = as.numeric(substr(FICO.Range,5,7)),
                   FICO = 0.5*(f1+f2)
                   ) %>%
            select(-FICO.Range, -f1, -f2)

glimpse(ld)            

#Employee Length column
# see values
table(ld$Employment.Length)

ld <- ld %>% mutate(
                    el = gsub("<", "", Employment.Length),
                    el = gsub("\\+", "", el),
                    el = gsub(" ", "", el),
                    el = gsub("years", "", el),
                    el = gsub("year", "", el),
                    el = as.numeric(el)
                   ) %>%
                select(- Employment.Length
                   )

glimpse(ld)  
table (ld$Employment.Length)
table (ld$el)


table(ld$Home.Ownership)
#Home Ownership Create Dummy Variables

# MORTGAGE     NONE    OTHER      OWN     RENT 
# 1147        1        5      200     1146 

# Here "None" and "Other" are insignificant. 
# So we will create 3 Columns for 4 Data Values

ld = ld %>% 
      mutate(HW_RENT = as.numeric(Home.Ownership == "RENT"),
             HW_MORT = as.numeric(Home.Ownership == "MORTGAGE"),
             HW_OWN = as.numeric(Home.Ownership == "OWN")
        
      ) %>%
  select(-Home.Ownership)

glimpse(ld)


# Loan Purpose
table(ld$Loan.Purpose)
# here even though there are 10 differnrent values, we are not going to create n-1 columns
# we will consider only those values which has more than 10% fills
# debt_consolidation, credit_card

ld = ld %>% 
      mutate(
        LP_DC = as.numeric(Loan.Purpose == "debt_consolidation"),
        LP_CC = as.numeric(Loan.Purpose == "credit_card"),
        LP_OTHER = as.numeric(Loan.Purpose == "other")
      )%>%
        select(-Loan.Purpose)
      
      glimpse(ld)
      
      

table(ld$Loan.Length)
# . 36 months 60 months 
# 1      1950       548 
# Since loans are of two tenures 3 yrs and 5 yrs we will categorize it in 2 


ld = ld %>% 
  mutate(
    LL_36 = as.numeric(Loan.Length == "36 months")
    
  )%>%
  select(-Loan.Length)

glimpse(ld)



#state
table(ld$State)
# State has too many values without anyone dominating. 
# Unless business states that State is important we can safely ignore.

ld = ld %>% select (-State)

      
glimpse(ld)
      
# Please omit all the rows where na
# Missing value imputation : Ideally before you clean data, you need to fix missing values
# i.e. ., n/a needs to be replaced with different methods e.g with mode value for categorical data
# But for simplicity we are going to omit all n/a for this example

ld = na.omit(ld)
glimpse(ld)


#----------------------------------------------------

#Step 5 : Sampling
# Now data is cleaned. Its time to split data set into 70:30 (trainval:test) and then from 70 again into 70:30 (train:val)
# Total Data = 70% trainval + 30 % test
# 70% trainval = 70%train + 30% val


set.seed(2)
s=sample(1:nrow(ld), 0.7*nrow(ld))

ld_trainval = ld[s,]
ld_test = ld[-s,]

s1=sample(1:nrow(ld_trainval), 0.7*nrow(ld_trainval))
ld_train = ld[s1,]
ld_val = ld[-s1,]
      
glimpse(ld)
glimpse(ld_train)


#---------------------------------------

# Step 6 : Create model
fit = lm(Interest.Rate~. -ID, data = ld_train)
      
library(car) #vif is in car
sort(vif(fit))

# NOW HW_MORT has highest VIF.. Ideally vif should be less than 10
# remove highest vif column one by one untill all are below 10

fit = lm(Interest.Rate~. -ID -HW_MORT, data = ld_train)
sort(vif(fit))
summary(fit)


fit = lm(Interest.Rate~. -ID -HW_MORT -Amount.Requested , data = ld_train)
sort(vif(fit))

#Now all columns have vif >10. 
# Now use summary and see all columns are *. i.e. P values are below 0.05
summary(fit)
#Revolving.CREDIT.Balance has p value .94 so remove it

fit = lm(Interest.Rate~. -ID -HW_MORT -Amount.Requested -Revolving.CREDIT.Balance, data = ld_train)

summary(fit)

fit = lm(Interest.Rate~. -ID -HW_MORT -Amount.Requested -Revolving.CREDIT.Balance -el, data = ld_train)

summary(fit)


fit = lm(Interest.Rate~. -ID -HW_MORT -Amount.Requested -Revolving.CREDIT.Balance -el -Debt.To.Income.Ratio, data = ld_train)
summary(fit)

fit = lm(Interest.Rate~. -ID -HW_MORT -Amount.Requested -Revolving.CREDIT.Balance -el -Debt.To.Income.Ratio -LP_OTHER, data = ld_train)
summary(fit)

fit_train = fit

# Now all columns have star so our model is ready for train

# Lets do on val

fit_val=lm(Interest.Rate~. -ID, data=ld_val)
sort(vif(fit_val))

fit_val=lm(Interest.Rate~. -ID -HW_RENT, data=ld_val)
sort(vif(fit_val))

fit_val=lm(Interest.Rate~. -ID -HW_RENT -Amount.Requested, data=ld_val)
sort(vif(fit_val))

# all columns have vif <10

summary(fit_val)
#.. Check Pr values should be less than 0.05
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
fit_val=lm(Interest.Rate~. -ID -HW_RENT -Amount.Requested -Debt.To.Income.Ratio, data=ld_val)
summary(fit_val)

fit_val=lm(Interest.Rate~. -ID -HW_RENT -Amount.Requested -Debt.To.Income.Ratio -HW_OWN, data=ld_val)
summary(fit_val)

fit_val=lm(Interest.Rate~. -ID -HW_RENT -Amount.Requested -Debt.To.Income.Ratio -HW_OWN -Revolving.CREDIT.Balance, data=ld_val)
summary(fit_val)

fit_val=lm(Interest.Rate~. -ID -HW_RENT -Amount.Requested -Debt.To.Income.Ratio -HW_OWN -Revolving.CREDIT.Balance -el, data=ld_val)
summary(fit_val)


fit_val=lm(Interest.Rate~. -ID -HW_RENT -Amount.Requested -Debt.To.Income.Ratio -HW_OWN -Revolving.CREDIT.Balance -el -Monthly.Income, data=ld_val)
summary(fit_val)

fit_val=lm(Interest.Rate~. -ID -HW_RENT -Amount.Requested -Debt.To.Income.Ratio -HW_OWN -Revolving.CREDIT.Balance -el -Monthly.Income -LP_DC, data=ld_val)
summary(fit_val)


fit_val=lm(Interest.Rate~. -ID -HW_RENT -Amount.Requested -Debt.To.Income.Ratio -HW_OWN -Revolving.CREDIT.Balance -el -Monthly.Income -LP_DC -HW_MORT, data=ld_val)
summary(fit_val)

#now all columns are *


#Lets extract the common variables
train_vars=names(fit_train$coefficients)
val_vars=names(fit_val$coefficients)
train_vars
val_vars
train_vars[train_vars %in% val_vars][-1]

paste(train_vars[train_vars %in% val_vars][-1],collapse="+")

fit_final=lm(Interest.Rate~Amount.Funded.By.Investors+Open.CREDIT.Lines+Inquiries.in.the.Last.6.Months+FICO+LP_CC+LP_OTHER+LL_36,data=ld_train)
summary(fit_final)

#remove LP_CC
fit_final=lm(Interest.Rate~Amount.Funded.By.Investors+Open.CREDIT.Lines+Inquiries.in.the.Last.6.Months+FICO+LP_OTHER+LL_36,data=ld_train)
summary(fit_final)

#Our Final model is ready

fit_test=lm(Interest.Rate~Amount.Funded.By.Investors+Monthly.Income+Open.CREDIT.Lines+Inquiries.in.the.Last.6.Months+FICO+HW_RENT+HW_OWN+LP_DC+LP_CC+LL_36,data=ld_test)
summary(fit_test)




# 
#y equation y=mx + c : c would be the value of intercept and then m1x1+m2x2+.....
#normally we dont need y equaton to predict. we use prdict function instead
model_string=paste(fit_final$coefficients,names(fit_final$coefficients),sep="*",collapse = " + ")
strwrap(sub("\\*\\(Intercept\\)","",gsub("+ -","- ",model_string,fixed=TRUE)))


y_eq = strwrap(sub("\\*\\(Intercept\\)", "", gsub("+ -","- ",model_string,fixed=TRUE)))
paste(y_eq, collapse ="")
#y_eq is our y equation which can also be used to predict


#Now test our model
# Assumptions of LR

#Linearity
plot(fit_final,which=1)

#Normality of errors
plot(fit_final,which=2)

#Homosedatisity
plot(fit_final,which=3)

#Cooks Distance
plot(fit_final,which=4) #cook's distance is used to find outliers, row outliers

#Now Predit interst on our train data
ld_train$Prediteced.Int = predict(fit_final,newdata=ld_train)
glimpse(ld_train)

#rmse for train
mean((ld_train$Interest.Rate - ld_train$Prediteced.Int )**2) %>%  sqrt()
#2.000167


#Now Predit interst on our trainval data
ld_trainval$Prediteced.Int = predict(fit_final,newdata=ld_trainval)
glimpse(ld_trainval)

#rmse for test
mean((ld_trainval$Interest.Rate-ld_trainval$Prediteced.Int)**2) %>% sqrt()
#2.053324


#Now Predit interst on our test data
ld_test$Prediteced.Int = predict(fit_final,newdata=ld_test)
glimpse(ld_test)

#rmse for test
mean((ld_test$Interest.Rate-ld_test$Prediteced.Int)**2) %>% sqrt()
#1.991923








