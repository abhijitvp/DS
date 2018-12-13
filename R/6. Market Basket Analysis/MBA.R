install.packages("arules")
install.packages("arulesViz")
library("arules")
library("arulesViz")
data("Groceries")
# Ploting Frequent items
itemFrequencyPlot(Groceries,topN=10)
# Now apply Rules alogorithm
rules = apriori(Groceries , parameter = list(confidence = .8 , support = .001))
# NOw sort rules
rules_sort = sort(rules, by = "confidence" , decreasing = T)
inspect(rules_sort[1:5])
# now for RHS defined
rules_milk = apriori(Groceries , parameter = list(confidence = .8 , support = .001) , appearance = list(default = "lhs" , rhs = "whole milk"))
inspect(rules_milk[1:5])
rules_sort_milk = sort(rules_milk, by = "confidence" , decreasing = T)

plot(rules_sort_milk[1:5],engine='interactive' , method = "graph")


