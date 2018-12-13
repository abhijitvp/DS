install.packages("arules")
install.packages("arulesViz")
install.packages("tk")


library(arules)
library(arulesviz)

data("Groceries")

#Plotting Frequency Items
itemFrequencyPlot(Groceries, topN=10)

# Now apply rules alogritm
rules = apriori(Groceries, parameter = list(confidence = 0.8, support=0.001))

# Now sort rules
rules_sort = sort(rules, by = "confidence", decreasing = T)
inspect(rules_sort[1:5])

# Now for rhs defined
rules_milk = apriori(Groceries, parameter = list(confidence = 0.8, support=0.001),
appearance = list(default="lhs", rhs="whole milk"))

rules_sort_milk = sort(rules_milk, by = "confidence", decreasing = T)
inspect(rules_sort_milk[1:5])

plot(rules_sort_milk[1:5],engine="interactive",method="graph")
