library(arules) 
groceries <- read.transactions("groceries.csv", format="basket", sep=",")  
  
groceryrules <- apriori(groceries,parameter=list(support=0.006, confidence = 0.25, minlen = 2))  

# Use the summary function to view summary information about the rule.
summary(groceryrules)

#queation(a)
# Use inpect function to see specific rules
inspect(groceryrules)

#see the rulse of high lift
ordered_groceryrules <- sort(groceryrules, by="lift")  
inspect(ordered_groceryrules[1:5]) 

#use the image to see 
plot(groceryrules)
plot(groceryrules, method="graph")

#question(b)
inspect(sort(groceryrules,by='support')[1:10])


#question(c)
eclat1 <- eclat(groceries,
                  parameter = list(minlen=2,maxlen=3,
                                   support=0.006,target = 'frequent itemsets'),
                  control = list(sort=-1))
inspect(sort(eclat1,by='support')[1:10])


#question(d) the ##times of user## is the time of run the function
system.time(groceryrules <- apriori(groceries,parameter=list(support=0.006, confidence = 0.25, minlen = 2))  )
system.time(eclat1 <- eclat(groceries,parameter = list(minlen=2,maxlen=3,support=0.006,target = 'frequent itemsets'),control = list(sort=-1)))