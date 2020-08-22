#Run in console `options("scipen"=100, "digits"=4)` for getting more user-friendly information

#required libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
#reordering the factor levels first according to their frequency `forcats package`
library(forcats)
library(arules)
#Interective vizualization
library(arulesViz)
library(htmlwidgets)
#Scaterplot/ for sequential_hcl
library(colorspace)
library(Rgraphviz)
#Shiny App pack
library(shinythemes)
#Manipulation with date and time (pack)
library(lubridate)
library(grid)
library(gridExtra)


#importing data
dataset_market <- read.csv(file = 'datasets_76340_171642_BreadBasket_DMS.csv')
head(dataset_market)

str(dataset_market)

#dataset for manipulation
dataset_market_test1 <- dataset_market
dataset_market_test1$Item <- fct_infreq(dataset_market_test1$Item)


#Grouping data by transactions and counting total number in one basket and 
#distinct items in one basket
dataset_market %>%
  group_by(Transaction) %>%
  summarise(
    total_bought_items = n(),
    distinct_bought_items = n_distinct(Item)
  )


#unique values in Item column
unique(dataset_market$Item)
length(unique(dataset_market$Item))

#Plotting the name of the item and the bought quantity
ggplot(dataset_market_test1, aes(x=Item, y=..count..))+
  geom_bar() +
  #coord_flip()
  theme(axis.text.x = element_text(angle = 90))
  
ggplot(dataset_market_test1, aes(x=Item, y=(..count..)/sum(..count..)))+
  geom_bar() +
  #coord_flip()
  theme(axis.text.x = element_text(angle = 90))
      
#Checking the amount of coffee items
dataset_market %>%
  filter(Item=='Coffee') %>%
  summarise(
    total_amount = n()
  )

#Counting the amount of baskets and the total bought items
dataset_market %>%
  select(Transaction) %>%
  summarise(
    number_of_baskets = n_distinct(Transaction),
    total = n()
  )

#We have unique 95 items in the shop
#And also we have 9531 number of baskets
n_items <- 95
n_baskets <- 9531

#Calculating the number of all possible baskets with Newton's Binom formula
2^95 
# RESULT: 39614081257132168796771975168

#Looping for the all possible values of basket fillings with different items in it
store = matrix(NA, nrow = 96, ncol = 2)
for (i in 0:n_items){
  store[i+1, ] = c(i, choose(n_items, i))
}
colnames(store)=c("size", "number_of_combination")
store

fun_nk = function(x) choose(n_items, x)
# Plotting
ggplot(data = data.frame(x = 0),    
       mapping = aes(x=x))+  
       stat_function(fun = fun_nk)+  
       xlim(0, n_items)+  
       xlab("Subset size")+  
       ylab("Number of subsets")


# Transactions per month
dataset_market %>%
  mutate(Month=as.factor(month(Date))) %>%
  group_by(Month) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=Month, y=Transactions)) +
  geom_bar(stat="identity", fill="mistyrose2", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per month") +
  theme_bw() 

# Transactions per weekday
dataset_market %>%
  mutate(WeekDay=as.factor(weekdays(as.Date(Date)))) %>%
  group_by(WeekDay) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=WeekDay, y=Transactions)) +
  geom_bar(stat="identity", fill="peachpuff2", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per weekday") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw() 

# Transactions per hour
dataset_market %>%
  mutate(Hour=as.factor(hour(hms(Time)))) %>%
  group_by(Hour) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=Hour, y=Transactions)) +
  geom_bar(stat="identity", fill="steelblue1", show.legend=FALSE, colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per hour") +
  theme_bw()


#Choice of support and confidence
# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.01, 0.005)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)


# Empty integers 
rules_sup10 <- integer(length=9)  #[OUT]: 0 0 0 0 0 0 0 0 0
rules_sup5 <- integer(length=9)   #[OUT]: 0 0 0 0 0 0 0 0 0
rules_sup1 <- integer(length=9)   #[OUT]: 0 0 0 0 0 0 0 0 0
rules_sup0.5 <- integer(length=9) #[OUT]: 0 0 0 0 0 0 0 0 0


# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(data_transactional, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup5[i] <- length(apriori(data_transactional, parameter=list(sup=supportLevels[2], 
                                                                      conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 1%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup1[i] <- length(apriori(data_transactional, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup0.5[i] <- length(apriori(data_transactional, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
}


# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") +
  theme_bw()

# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 1%
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 1%") + 
  scale_y_continuous(breaks=seq(0, 50, 10)) +
  theme_bw()

# Number of rules found with a support level of 0.5%
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 0.5%") + 
  scale_y_continuous(breaks=seq(0, 130, 20)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)



##Checking support range [2%:4%]

#Appending new support values in the vector
supportLevels <- append(supportLevels, c(0.02, 0.03, 0.04))

rules_sup2 <- integer(length=9)   #[OUT]: 0 0 0 0 0 0 0 0 0
rules_sup3 <- integer(length=9)   #[OUT]: 0 0 0 0 0 0 0 0 0
rules_sup4 <- integer(length=9)   #[OUT]: 0 0 0 0 0 0 0 0 0


# Apriori algorithm with a support level of 2%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup2[i] <- length(apriori(data_transactional, parameter=list(sup=supportLevels[5], 
                                                                       conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 3%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup3[i] <- length(apriori(data_transactional, parameter=list(sup=supportLevels[6], 
                                                                     conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 4%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup4[i] <- length(apriori(data_transactional, parameter=list(sup=supportLevels[7], 
                                                                     conf=confidenceLevels[i], target="rules")))
  
}


# Number of rules found with a support level of 2%
plot5 <- qplot(confidenceLevels, rules_sup2, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 2%") + 
  scale_y_continuous(breaks=seq(0, 50, 5)) +
  theme_bw()

# Number of rules found with a support level of 3%
plot6 <- qplot(confidenceLevels, rules_sup3, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 3%") + 
  scale_y_continuous(breaks=seq(0, 30, 2)) +
  theme_bw()

# Number of rules found with a support level of 4%
plot7 <- qplot(confidenceLevels, rules_sup4, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 4%") + 
  scale_y_continuous(breaks=seq(0, 30, 2)) +
  theme_bw()

# Subplot
grid.arrange(plot3, plot5, plot6, plot7, ncol=2)



# Data frame
num_rules <- data.frame(rules_sup10, 
                        rules_sup5, 
                        rules_sup4,
                        rules_sup3,
                        rules_sup2,
                        rules_sup1, 
                        rules_sup0.5, 
                        confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 4%, 3%, 2%, 1% and 0.5%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 1%)
  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  
  # Plot line and points (support level of 2%)
  geom_line(aes(y=rules_sup2, colour="Support level of 2%")) + 
  geom_point(aes(y=rules_sup2, colour="Support level of 2%")) +
  
  # Plot line and points (support level of 3%)
  geom_line(aes(y=rules_sup3, colour="Support level of 3%")) + 
  geom_point(aes(y=rules_sup3, colour="Support level of 3%")) +
  
  # Plot line and points (support level of 4%)
  geom_line(aes(y=rules_sup4, colour="Support level of 4%")) + 
  geom_point(aes(y=rules_sup4, colour="Support level of 4%")) +
  
  # Plot line and points (support level of 0.5%)
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())


#Create lists with split function
#1) Tranform transaction ID into factor
dataset_market_test1$Transaction <- factor(dataset_market_test1$Transaction)

#2) Split into groups
data_list <- split(dataset_market_test1$Item, dataset_market_test1$Transaction)
head(data_list)

#Transform to transaction class (transactional dataset)
data_transactional <- as(data_list, "transactions")

#Check the class og data_transactional
class(data_transactional)

str(data_transactional)

#Inspecting the transactional variable
inspect(head(data_transactional))

#Accessing specific transaction/s
inspect(data_transactional[25])
inspect(data_transactional[8:25])

#Summary of the transactional object
summary(data_transactional)

#plotting the data_transactional
image(data_transactional)

#Retrieve all frequent items
supp.all = apriori(data_transactional, 
                  parameter = list(
                    #Target
                    target = "frequent itemsets")
                  )

inspect(head(sort(supp.all,by="supp"),10))
inspect(supp.all)

#The apriory function for frequent itemsets (FREQUENT ITEMSETS)
supp.cb = apriori(data_transactional,
                  parameter = list(
                    #Minimum support 
                    supp = 0.01,
                    #Minimum confidence
                    conf = 0.2,
                    #Minimum lenght
                    minlen = 2,
                    #Target
                    target = "frequent itemsets"),
                  #Appearence argument
                  appearance = list(
                    items = c("Coffee", "Bread")
                  )
                  )
            
inspect(head(supp.cb))

#Apriori function for rules
rules.c.rhs = apriori(data_transactional,
                      parameter = list(
                        #Minimum support
                        supp = 0.01,
                        #Minimum confidence
                        conf = 0.01,
                        #Minimum length
                        minlen = 2,
                        target = "rules"),
                      appearance = list(rhs = "Coffee", default = "lhs")
                      )

inspect(head(rules.c.rhs))
inspect(rules.c.rhs)

#Rules with `Bread` on rhs
rules.b.rhs = apriori(data_transactional,
                      parameter = list(
                        #minlen = 2,
                        target = "rules"
                      )
                      #appearance = list(rhs="Bread", default="lhs")
                      )


inspect(head(rules.b.rhs))


#Call to the apriori function for rule generation with specific argument 
rulles.all = apriori(data_transactional,
                     parameter = list(supp = 0.01, conf = 0.2, minlen=2),
                     control = list(verbose=F) 
                     )
inspect(rulles.all)


redundant_rules = is.redundant(rulles.all)
length(redundant_rules)
non_redundant_rules = rulles.all[!redundant_rules] 

#Frequency plots
itemFrequencyPlot(data_transactional,
                  main = "Anbsolute item frequency plot",
                  type = "relative",
                  col = rainbow(95),
                  xlab = "Items",
                  cex.names = 0.6)

itemFrequencyPlot(data_transactional,
                  main = "Anbsolute item frequency plot",
                  type = "absolute",
                  topN = 15,
                  col = rainbow(15),
                  xlab = "Items")


itemFrequencyPlot(data_transactional,
                  main = "Relative item frequency plot",
                  type = "relative",
                  topN = 15,
                  col = "khaki",
                  xlab = "Items",
                  #font size of text undex x-axis
                  cex.names = 1.2,
                  horiz = TRUE)


#Plotting interective graphs
rules_for_interective = apriori(data_transactional, 
                                parameter = list(supp = 0.01, 
                                conf = 0.5,
                                minlen = 2)
                                )

#Interective table
inspectDT(rules_for_interective)

inspect(sort(rules_for_interective, by = "lift"))

## SCATERPLOT
##__________
plot(rules_for_interective)

## Scatterplot with custom colors
plot(rules_for_interective, control = list(col=sequential_hcl(20, palette="Inferno")))
plot(rules_for_interective, col=sequential_hcl(100, palette="OrYel"))
plot(rules_for_interective, col=grey.colors(50, alpha =.8))

## See all control options using verbose
plot(rules_for_interective, verbose = TRUE)

## Interactive plot (selected rules are returned)
## Not run:
sel <- plot(rules_for_interective, engine = "interactive")
## End(Not run)


## Create a html widget for interactive visualization
## Not run:
plot(rules_for_interective, col=sequential_hcl(15, palette="Inferno"), engine = "htmlwidget")
## End(Not run)

## Two-key plot (is a scatterplot with shading = "order")
plot(rules_for_interective, method = "two-key plot")
plot(rules_for_interective, method = "two-key plot", jitter=2)

## Matrix shading
## --------------
## The following techniques work better with fewer rules
subrules <- subset(rules_for_interective, lift>1.1)
subrules

## 2D matrix with shading
plot(subrules, method="matrix")

## 3D matrix
plot(subrules, method="matrix", engine = "3d")

## Matrix with two measures
plot(subrules, method="matrix", shading=c("lift", "confidence"))

## Interactive matrix plot (default interactive and as a html widget)
## Not run:
plot(subrules, method="scatterplot", engine="interactive")
plot(subrules, method="scatterplot", engine="htmlwidget")
## End(Not run)

## Grouped matrix plot
## -------------------
plot(rules_for_interective, method="grouped matrix")
plot(rules_for_interective, method="grouped matrix",
     col = grey.colors(10),
     gp_labels = gpar(col = "blue", cex=1, fontface="italic"))
## Interactive grouped matrix plot
## Not run:
plot(rules_for_interective, method="grouped", engine = "interactive")
## End(Not run)

## Graphs
## ------
plot(rules_for_interective, method = "graph")

## igraph layout generators can be used (see ? igraph::layout_)
plot(rules_for_interective, method="graph", control=list(layout=igraph::in_circle()))

plot(rules_for_interective, method="graph",
     layout=igraph::with_graphopt(spring.const=5, mass=50))

## Custom colors
plot(rules_for_interective, method="graph",
     nodeCol = grey.colors(100), edgeCol = grey(0.6), alpha = 1)


## Graph rendering using Graphviz
## Not run:
plot(rules_for_interective, method="graph", engine="graphviz")
## End(Not run)


## Interactive graph as a html widget (using igraph layout)
## Not run:
plot(rules_for_interective, method="graph", engine="htmlwidget")
plot(rules_for_interective, method="graph", engine="htmlwidget",
     igraphLayout = "layout_in_circle")


#Vizualizing rules
#Interactive rules
rules_html = plot(rules_for_interective, method = "graph", engine = "htmlwidget")
rules_html

#Extract graph-plot as .html file
saveWidget(rules_html, file = "rules_for_interective_graph.html")



## Parallel coordinates plot
## -------------------------
plot(rules_for_interective, method="paracoord")
plot(rules_for_interective, method="paracoord", control = list(reorder=TRUE))


## Doubledecker plot
## -----------------
## Note: only works for a single rule
oneRule <- sample(rules_for_interective, 1)
inspect(oneRule)
plot(oneRule, method="doubledecker", data = data_transactional)


## Itemsets
## --------
itemsets <- eclat(data_transactional, parameter = list(support = 0.01, minlen=2))
plot(itemsets)
plot(itemsets, method="graph")
plot(itemsets, method="paracoord", alpha=.5, reorder=TRUE)


## Add more quality measures to use for the scatterplot
## ----------------------------------------------------
quality(itemsets) <- interestMeasure(itemsets, trans=dataset_market_test1)
head(quality(itemsets))
plot(itemsets, measure=c("support", "allConfidence"), shading="lift")



plot(rules_for_interective, method = "grouped")



#Interective rules
plot(rules_for_interective, engine = "plotly")

#Rule explorer with Shiny App
ruleExplorer(rules_for_interective)


## What influenced coffee?
## Coffee as a consequent

# Extract rules with Yogurt on the right side
coffee_rules_rhs = apriori(data_transactional, parameter = list(supp = 0.01,
                                                                conf = 0.1), 
                                               appearance = list(default = "lhs",
                                                                 rhs = "Coffee"))
# Find first rules with highest lift
inspect(head(sort(coffee_rules_rhs, by="lift")))
# Summary of rules
summary(coffee_rules_rhs)


## What did coffee influence?
## Coffee as an antecedent
# Extract rules with Yogurt on the left side
coffee_rules_lhs = apriori(data_transactional, parameter = list(supp = 0.01,
                                                                conf = 0.1, 
                                                                minlen = 2), 
                                               appearance = list(default = "rhs",
                                                                 lhs = "Coffee"))

# Find first rules with highest lift
inspect(head(sort(coffee_rules_lhs, by="lift")))
# Summary of rules
summary(coffee_rules_lhs)