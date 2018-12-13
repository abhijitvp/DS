### Set the work directory, change as per your work folder
# setwd("C:/Users/babycorn/Documents/Edupristine/market basket analysis")

#better way to set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


### Import/read the data
txn_data<-read.csv("Retail_Data.csv")

### See the data summary (verify Data)
head(txn_data)
tail(txn_data)
summary(txn_data)


library(arules)
##library(arulesSequences)
## taking subset of Data  
smpl_dat<-txn_data[1:8000,]
tail(smpl_dat)
# factorization of variables
for ( i in 1:ncol(smpl_dat))
{
  smpl_dat[,i]=as.factor(smpl_dat[,i])
}
#Plot of relative frequency
smpl_dat<-as(smpl_dat,"transactions")
itemFrequencyPlot(smpl_dat, topN = 9)

# Running aproiori command
basket_rules <- apriori(smpl_dat, parameter = list(sup = 0.005, conf = 0.01, target="rules",minlen=2,maxlen=3))

summary( basket_rules)
inspect(  basket_rules) # see all the rules
inspect(head(sort( basket_rules,by="lift"),20))	

