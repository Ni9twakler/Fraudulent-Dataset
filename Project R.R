#IST 421: Project Code
dev.off()
cat('\014') 
rm(list=ls())

# basic Rweka packages
library(RWeka)       # Weka
library(party)       # A computational toolbox for recursive partitioning
library(partykit)    # A toolkit with infrastructure for representing, summarizing, 
              #and visualizing tree-structured regression and classification models.
library(rsample)
# Helper packages
library(dplyr)       # for data wrangling
library(ggExtra)     # for maginal plots
library(ggplot2)     # for awesome plotting

# Modeling packages
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application
library(AmesHousing) # dataset

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects

library(varhandle)   #used to ungroup factors
library(plyr)
library(arules)
library(arulesViz)
library(tidyverse)
library(lubridate)

library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)  # subfigure layout package
library(dendextend) # for comparing two dendrograms

#install.packages('varhandle')
setwd('~/Desktop/Classes/Spr 21/IST 421/Project/attachments/Fraud')

fraud_data_train <- read.csv('fraudTrain.csv')
fraud_data_test <- read.csv('fraudTest.csv')

#Clensing
fraud_data_train$trans_date_trans_time <- strptime(fraud_data_train$trans_date_trans_time, '%Y-%m-%d %H:%M:%OS')
fraud_data_train$trans_date_trans_time <- as.POSIXct(fraud_data_train$trans_date_trans_time)
fraud_data_train$merchant <- gsub('fraud_', '',fraud_data_train$merchant)
fraud_data_train$merchant <- factor(fraud_data_train$merchant)
fraud_data_train$category <- factor(fraud_data_train$category) 
fraud_data_train$lat <- as.double(fraud_data_train$lat)
fraud_data_train$long <- as.double(fraud_data_train$long)
fraud_data_train$job <- factor(fraud_data_train$job) #494 different jobs

fraud_data_test$trans_date_trans_time <- strptime(fraud_data_test$trans_date_trans_time, '%Y-%m-%d %H:%M:%OS')
fraud_data_test$trans_date_trans_time <- as.POSIXct(fraud_data_test$trans_date_trans_time)
fraud_data_test$merchant <- gsub('fraud_', '',fraud_data_test$merchant)
fraud_data_test$merchant <- factor(fraud_data_test$merchant)
fraud_data_test$category <- factor(fraud_data_test$category) 
fraud_data_test$job <- factor(fraud_data_test$job) #494 different jobs

fraud_data_train <- na.omit(fraud_data_train)
fraud_data_test <- na.omit(fraud_data_test)

fraud <- fraud_data_train %>% filter(is_fraud == 1) %>%
  select(trans_date_trans_time, merchant, category, amt, gender, city, city_pop, job, state, long, lat, merch_long, merch_lat)
nFraud <- fraud_data_train %>% filter(is_fraud != 1 )%>%
  select(trans_date_trans_time, merchant, category, amt, gender, city, city_pop, job, state, long, lat, merch_long, merch_lat)

sample2 <- read.csv('fraudTrain_sample2.csv')
sample1 <- read.csv('fraudTest_sample.csv')

fraud_train <- lapply(fraud_data_train, as.factor)
fraud_test <- lapply(fraud_data_test, as.factor)

#join the data based off state
states <- map_data('state')
state.info <- inner_join(data.frame(state=states.name, 
                                    long=states.center$x, lat=state.center$y,tringsAsFactors=FALSE),
                         data.frame(state=state.name, abbrev=state.abb))
fraud_mapping <- fraud
fraud_mapping$abbrev <- fraud$state
#str(fraud_mapping)
#fraud_mapping <- fraud_mapping[!(fraud_mapping$state=='AK' | fraud_mapping=='HI'),]
prac <- merge(fraud_mapping,state.info, by="abbrev",all.x=T)
prac$long.x <- as.double(prac$long.x)
prac$lat.x <- as.double(prac$lat.x)

View(fraud)
str(fraud)

myVars = c('trans_date_trans_time', 'merchant', 'category', 'gender', 'job', 'is_fraud')
myExtraVars = c('trans_date_trans_time', 'merchant', 'category', 'gender', 'job', 'long', 'lat', 'merch_long', 'merch_lat','is_fraud')

new_ft <- fraud_train[myVars]
new_fta <- fraud_train[myExtraVars]




##### Plots ########
#I think you still have some work to do here to make your plots more accessible.  
#Overplotting is a real problem in the last graph - I'd go with mean / total over time, 
#instead of a scatter.  You could bin by day / week.  You should also consider average 
#stolen as a function of weekday.  Also, for a single distribution, use a histogram 
#instead of a box plot.  Finally - not seeing a difference between plots 1 & 2, 
#other than a breakdown by gender?  So, you'll want to get right of one of them.

#how i got 60 M dollars for consumer spending
#.24/4040
#percent of consumer spending/ amount of total retail 
#links: https://www.digitalcommerce360.com/article/e-commerce-sales-retail-sales-ten-year-review/
#link: https://www.mprnews.org/story/2008/10/30/consumer-spending-accounts-for-two-thirds-of-us-economy
#Link: https://www.statista.com/statistics/1099933/travel-and-tourism-share-of-gdp/




#4 dimension distribution
ggplot(fraud)+geom_boxplot(aes(y=amt, fill=category))
ggplot(fraud)+geom_boxplot(aes(y=amt, fill=category, color= category))
ggplot(fraud)+geom_boxplot(aes(y=amt, x=gender,fill=category, color= category))
ggplot(fraud)+geom_boxplot(aes(y=amt, x=gender,fill=category, color= category)) +
  ggtitle('Dollars Stolen') +ylab('Dollars Stolen')
ggsave('4 dimension distribution.png')
#This graph conveys the difference in spending when it comes to transaction purposes
#and categories. from the overall distribution, you can see that Men and Women both have 
#fraudulent activities in the travel and shopping point of sales, one trend that is noticeable in
#females section of the distribution is the variance in miscellaneous transactions.

#Amount stole per category
ggplot(fraud)+geom_boxplot(aes(y=amt,x=category, color=category, fill=category)) +
  ggtitle('Dollars Stolen per Category')+theme(axis.text.x = element_text(angle=45,hjust = 1))
ggsave('amt per cat.png')
summary(fraud_data_train$unix_time)
#is a visual rep of the outlines and other norm data. Can see that once you get
#over 10000$ there must be another variable influencing this data.They have been grouped
#by their merchant and orgainzed based off Unixtime.

#plotting map for fraud
ggplot()+geom_polygon(data= states, aes(long, lat, group=group), color= 'black', fill='white') +
  geom_point(aes(prac$long.x, prac$lat.x))
ggplot()+geom_polygon(data= states, aes(long, lat, group=group), color= 'black', fill='white') +
  geom_point(aes(prac$long.x, prac$lat.x), color=prac$city_pop)
ggplot()+geom_polygon(data= states, aes(long, lat, group=group), color= 'black', fill='white') +
  geom_point(aes(prac$long.x, prac$lat.x), color=as.character(as.numeric(prac$merchant)))
ggplot()+geom_polygon(data= states, aes(long, lat, group=group), color= 'black', fill='white') +
  geom_point(aes(prac$long.x, prac$lat.x), color=as.numeric(prac$category)) +xlim(-125, -65) +ylim(c(23, 50))
ggplot()+geom_polygon(data= states, aes(long, lat, group=group), color= 'black', fill='white') +
  geom_point(aes(prac$long.x, prac$lat.x), color=as.numeric(prac$category)) +xlim(-125, -65) +ylim(c(23, 50))+
  theme(axis.title = element_blank(), axis.ticks= element_blank(), axis.text = element_blank())
  
mapping <- merge(fraud_mapping,state.info, by="abbrev",all.x=T)
mapping$long.x <- as.double(mapping$long.x)
mapping$lat.x <- as.double(mapping$lat.x)

ggplot()+geom_polygon(data= states, aes(long, lat, group=group), color= 'black', fill='white') +
  geom_point(data= mapping, aes(long.x, lat.x, color=category)) +xlim(-125, -65) +ylim(c(23, 50))+
  theme(axis.title = element_blank(), axis.ticks= element_blank(), axis.text = element_blank())+
  ggtitle('Locations of Fraudulent Theft')

ggsave('US map.png')




mapping %>% 




count(mapping, vars =mapping$category)
count()

mapping %>% select(c(date_time = mapping$trans_date_trans_time, mapping$merchant, mapping$category, 
                   mapping$abbrev, state = mapping$state.x))

colnames(mapping)

#use chloro path to color states, find max theft cat per state  
prac <- subset(fraud, select=-c(merchant))
chloro <- states %>% group_by(region)


prac %>% group_by(category) %>% mutate(tamt = ) %>% arrange(-amt)

rm(agg.data)

#steps: 
  #filter data max theft grouping by states and cat
  #summarise total amt (or count=n())
  #group by state, filtering count=max(count)
  #join map with above 
  #color fill with cat and scale opacity to count of theft
  legend(-120, 30, legend = c(as.numeric(prac$category)),lty=1:2, cex=0.8,title="Line types", text.font=4, bg='lightblue')

ggsave('plot1.7.png')




ggplot(fraud, aes(x=as.Date(as.POSIXct(trans_date_trans_time, tz='UTC')), y=amt)) +
  geom_line() + 
  xlab("")+
  scale_x_date(limit=c(as.Date("2019-01-01"),as.Date("2019-6-01")))


ts1 <- ggplot(data = fraud, aes(x=as.Date(trans_date_trans_time, tz='UTC'), y=amt, color=category ))+
  ylab('Dollars Stolen') + xlab('')+
  geom_line(data = fraud[fraud$category == 'shopping_pos' |fraud$category == 'shopping_net' ,],aes(y=amt ), size=1, alpha=.5)   +
  theme(legend.position=c(.1,.85))+
  scale_x_date(limit=c(as.Date("2019-01-01"),as.Date("2019-6-01")))

ggplot(data = fraud, aes(x=as.Date(trans_date_trans_time, tz='UTC'), y=amt, color=category ))+
  ylab('Dollars Stolen') + xlab('')+
  geom_line(data = fraud[fraud$category == 'grocery_pos' ,],aes(y=amt ), size=1, alpha=.5)   +
  geom_line()+
  theme(legend.position='none')+
  scale_x_date(limit=c(as.Date("2019-01-01"),as.Date("2019-6-01")))


ggplot(data=fraud, aes(x=amt, group=category, fill=category)) +
  geom_density(adjust=1.5)+ggtitle('Distrubtion of Amount stolen by Category')
#ggsave('Distrubtion of Amount stolen by Category.png')
ggplot(data=fraud, aes(x=amt, group=category, fill=category)) +
  geom_density(adjust=1.5)+ xlim(0,150)+ggtitle('$0 to $50')+theme_minimal()

ggplot(data=fraud, aes(x=amt, group=category, fill=category)) +
  geom_density(adjust=1.5)+ xlim(75,180)
ggsave('distro voom.pdf')






ggplot(data = fraud, aes(x=as.Date(trans_date_trans_time, tz='UTC'),y=amt)) + geom_point() + geom_smooth()




#distribution of amount stolen
hist(fraud$amt, breaks=100, main='Distrubtion of Amount Stolen', xlab = 'Dollars Stolen')
#possible color by max count
ggsave('Distrubtion of Amount Stolen.pdf')

#Second distrubtion to reinforce therft in shopping and misc
p <- ggplot(fraud, aes(x=city_pop,y=amt, color=category)) +
  geom_point() +
  theme(legend.position="none")+ xlim(0e+00, 250000) + 
  xlab('City Population') + ylab('Dollars Stolen')
p1 <- ggMarginal(p, type="histogram")



ggplot(fraud)+ geom_point(aes(y=amt, x=category))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+ xlab('')
ggsave('Project 1.1.pdf')


#density plot
d <- density(fraud_data_train$amt, fraud_data_train$unix_time)
plot(d,main='Density of stolen amount')
#from the density plot I can observe there is a significant of movement in the
#beginning of this data, the later observations have no influence on the amount stole
#because they are all similar




#boxplots
ggplot(fraud_data_train)+geom_boxplot(aes(y=amt))+ylim(0,200)+ggtitle('Boxplot of Amount')

summary(fraud_data_train$amt)
#This plot makes me want to ask questions such as why are these not apart of the normal
#distribution. what is the range of the IQR and stat data


plot(scale(fraud$amt))
#Both of these graphs are the same yet, I noticed most of the data is under
#100$ amount with some extreme cases. As we continuously look at other obersevations
#its becomes more common to see spikes of large amounts






###### Advanced Code #########
## CLUSTERING
myVars = c('amt', 'city_pop')
cdf <- fraud[myVars]
cdf <- scale(cdf)
d <- dist(cdf, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)

practice <- fraud_data_train


## APORORI
fraud_big <- subset(practice, select = -c(X, street, dob, trans_num, cc_num, unix_time)) 
fraud_big <- subset(fraud_big, select = -c(trans_date_trans_time))
fraud_big <- subset(fraud_big, select = -c(first, last, lat, long, merch_lat, merch_long,fraud_big))

## this is the same as your data cleaning above
fraud_big$merchant <- gsub('fraud_', '',fraud_big$merchant)
fraud_big$merchant <- factor(fraud_big$merchant)
fraud_big$category <- factor(fraud_big$category) 
fraud_big$job <- factor(fraud_big$job) #494 different jobs
fraud_big$gender <-factor(fraud_big$gender)

## factorizing?? the numerical stuff
fraud_big$amt <- cut(fraud_big$amt, breaks = c(0,10,50,100,300,500,1000,Inf),labels=c("$0-$10","$10-50","$50-$100","$100-$300","$300-$500","$500-$1000","$1000+"))
fraud_big$city_pop <- cut(fraud_big$city_pop, breaks = c(0,100,1000,10000,100000,1000000,Inf),labels=c(">100",">1000",">10000",">100000",">1000000","1000000+"))


fraud_big<- na.omit(fraud_big)

## i have to change 0 to no and 1 to yes. idk why it just doesnt work if i don't
fraud_big$is_fraud <- recode(fraud_big$is_fraud, "0"="no")
fraud_big$is_fraud <- recode(fraud_big$is_fraud, "1"="yes")
fraud_big$is_fraud <-factor(fraud_big$is_fraud)

View(fraud_big)

rules<-apriori(data=fraud_big, parameter=list(supp=0.0009,conf = 0.9, maxlen=10), 
               appearance = list(default="lhs",rhs="is_fraud=yes"), control = list (verbose=F))

rules<-sort(rules, decreasing=TRUE,by="support")


inspect(rules[1:100])


## DECISION TREE 2
nFraud_train$trans_date_trans_time <- strptime(nFraud_train$trans_date_trans_time, '%Y-%m-%d %H:%M:%OS')
nFraud_train$trans_date_trans_time <- as.POSIXct(nFraud_train$trans_date_trans_time)
nFraud_train$merchant <- gsub('fraud_', '',nFraud_train$merchant)
nFraud_train$merchant <- factor(nFraud_train$merchant)
nFraud_train$category <- factor(nFraud_train$category) 
nFraud_train$job <- factor(nFraud_train$job) 

nFraud_test$trans_date_trans_time <- strptime(nFraud_test$trans_date_trans_time, '%Y-%m-%d %H:%M:%OS')
nFraud_test$trans_date_trans_time <- as.POSIXct(nFraud_test$trans_date_trans_time)
nFraud_test$merchant <- gsub('fraud_', '',nFraud_test$merchant)
nFraud_test$merchant <- factor(nFraud_test$merchant)
nFraud_test$category <- factor(nFraud_test$category) 
nFraud_test$job <- factor(nFraud_test$job) 

nFraud_train <- na.omit(nFraud_train)
nFraud_test <- na.omit(nFraud_test)

nFraud_train <- lapply(nFraud_train, as.factor)
nFraud_test <- lapply(nFraud_test, as.factor)

nFraud_test<- nFraud_test[!(nFraud_test$city == "Aurora" | nFraud_test$city == "Cassatt" | 
                                    nFraud_test$city == "Collegeville" | 
                                    nFraud_test$city == "Deltona" | nFraud_test$city == "Cross" |
                                    nFraud_test$city == "Greenville" | nFraud_test$city == "Harmony" |
                                    nFraud_test$city == "Hedley" | nFraud_test$city == "Heislerville" |
                                    nFraud_test$city == "Ironton" | nFraud_test$city == "Irwinton" |
                                    nFraud_test$city == "Jackson" | nFraud_test$city == "Kings Bay" |
                                    nFraud_test$city == "Kittery Point" | nFraud_test$city == "Knowlesville" |
                                    nFraud_test$city == "Linthicum Heights" | nFraud_test$city == "Los Angeles" |
                                    nFraud_test$city == "Loving" | nFraud_test$city == "Mc Clellandtown" |
                                    nFraud_test$city == "Michigan" | nFraud_test$city == "Moscow" |
                                    nFraud_test$city == "New Franken" | nFraud_test$city == "North Augusta" |
                                    
                                    nFraud_test$city == "North Las Vegas" | nFraud_test$city == "Oakford" |
                                    nFraud_test$city == "Pea Ridge" | nFraud_test$city == "Port Charlotte" |
                                    nFraud_test$city == "Port Richey" | nFraud_test$city == "Premier" |
                                    nFraud_test$city == "Scotts Mills" | nFraud_test$city == "Skytop" |
                                    nFraud_test$city == "Spirit Lake" | nFraud_test$city == "Springfield Gardens" |
                                    nFraud_test$city == "Springville" | nFraud_test$city == "Stittville" |
                                    
                                    nFraud_test$city == "Stoneham" | nFraud_test$city == "Thornville" |
                                    nFraud_test$city == "Vancouver" | nFraud_test$city == "Veedersburg" |
                                    nFraud_test$city == "Wendel" | nFraud_test$city == "West Bethel" |
                                    nFraud_test$city == "Williams" | nFraud_test$city == "Williamsburg" |
                                    nFraud_test$city == "Winslow" 
),]

nFraud_test<- nFraud_test[!(nFraud_test$merchant == "Breitenberg LLC" | nFraud_test$merchant == "Collier Inc" |
                                    nFraud_test$merchant == "Jakubowski Group" | nFraud_test$merchant == "Kessler Group" | 
                                    nFraud_test$merchant == "Kilback" | nFraud_test$merchant == "Nitzsche and Leffler" |
                                    nFraud_test$merchant == "Larson" |
                                    nFraud_test$merchant == "Quitzon and Spencer" | nFraud_test$merchant == "Paucek-Wiza" | 
                                    nFraud_test$merchant == "Kilback, Nitzsche and Leffler" |
                                    nFraud_test$merchant == "Larson, Quitzon and Spencer"),]


tree_1<- rpart(is_fraud ~.,
               nFraud_train,
               control=rpart.control(cp = .001, minbucket = 10))

tree.att <- predict(tree_1, fraud_test, type='class')
## use Predict to predict Test Set, Class is label

confusionMatrix(tree.att, fraud_test$is_fraud)

## forget that i odnt think it worked. let's try KNN idk

#set up data for training; only useful columns
myVars=c("category", "merchant", "amt", "gender", "city", "state", "is_fraud")

new_fraud_train=fraud_data_train[myVars]
library(dplyr) ## need to change the 0 and 1s to something else in order to classify
new_fraud_train$is_fraud <- recode(new_fraud_train$is_fraud, "0"="no")
new_fraud_train$is_fraud <- recode(new_fraud_train$is_fraud, "1"="yes")
View(new_fraud_train)

new_fraud_test=fraud_data_test[myVars]
## removing new levels that are in test but not training

new_fraud_test<- new_fraud_test[!(new_fraud_test$city == "Aurora" | new_fraud_test$city == "Cassatt" | 
                                    new_fraud_test$city == "Collegeville" | 
                                    new_fraud_test$city == "Deltona" | new_fraud_test$city == "Cross" |
                                    new_fraud_test$city == "Greenville" | new_fraud_test$city == "Harmony" |
                                    new_fraud_test$city == "Hedley" | new_fraud_test$city == "Heislerville" |
                                    new_fraud_test$city == "Ironton" | new_fraud_test$city == "Irwinton" |
                                    new_fraud_test$city == "Jackson" | new_fraud_test$city == "Kings Bay" |
                                    new_fraud_test$city == "Kittery Point" | new_fraud_test$city == "Knowlesville" |
                                    new_fraud_test$city == "Linthicum Heights" | new_fraud_test$city == "Los Angeles" |
                                    new_fraud_test$city == "Loving" | new_fraud_test$city == "Mc Clellandtown" |
                                    new_fraud_test$city == "Michigan" | new_fraud_test$city == "Moscow" |
                                    new_fraud_test$city == "New Franken" | new_fraud_test$city == "North Augusta" |
                                    
                                    new_fraud_test$city == "North Las Vegas" | new_fraud_test$city == "Oakford" |
                                    new_fraud_test$city == "Pea Ridge" | new_fraud_test$city == "Port Charlotte" |
                                    new_fraud_test$city == "Port Richey" | new_fraud_test$city == "Premier" |
                                    new_fraud_test$city == "Scotts Mills" | new_fraud_test$city == "Skytop" |
                                    new_fraud_test$city == "Spirit Lake" | new_fraud_test$city == "Springfield Gardens" |
                                    new_fraud_test$city == "Springville" | new_fraud_test$city == "Stittville" |
                                    
                                    new_fraud_test$city == "Stoneham" | new_fraud_test$city == "Thornville" |
                                    new_fraud_test$city == "Vancouver" | new_fraud_test$city == "Veedersburg" |
                                    new_fraud_test$city == "Wendel" | new_fraud_test$city == "West Bethel" |
                                    new_fraud_test$city == "Williams" | new_fraud_test$city == "Williamsburg" |
                                    new_fraud_test$city == "Winslow" 
),]

new_fraud_test<- new_fraud_test[!(new_fraud_test$merchant == "Breitenberg LLC" | new_fraud_test$merchant == "Collier Inc" |
                                    new_fraud_test$merchant == "Jakubowski Group" | new_fraud_test$merchant == "Kessler Group" | 
                                    new_fraud_test$merchant == "Kilback" | new_fraud_test$merchant == "Nitzsche and Leffler" |
                                    new_fraud_test$merchant == "Larson" |
                                    new_fraud_test$merchant == "Quitzon and Spencer" | new_fraud_test$merchant == "Paucek-Wiza" | 
                                    new_fraud_test$merchant == "Kilback, Nitzsche and Leffler" |
                                    new_fraud_test$merchant == "Larson, Quitzon and Spencer"),]
View(new_fraud_test)


## Run a KNN model
search_grid = expand.grid(k = c(1,2)) ## less values so less time

# set up 3-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 2
) ## just a simple one so it doesn't take 30 years


# train model
knn <- train(is_fraud ~ .,
             data = new_fraud_train,
             method = "knn",
             trControl = train_control,
             tuneGrid = search_grid
)

## verify performance and test on test set

View(knn)

# top 5 models
knn$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# results for best model
confusionMatrix(knn)

## testing on test data.. i think this part was unnecessary though so ignore it

pred <- predict(knn, newdata = new_fraud_test)

## write pred to file i guess

id <- rownames(new_fraud_test)
new_fraud_test <- cbind(id=id, new_fraud_test)

View(new_fraud_test)

combined_pred=cbind(id, pred)
head(combined_pred)

colnames(combined_pred)=c("Transaction ID", "Fraud")

write.csv(combined_pred, file="transactionfraud-KNN-pred.csv", row.names=FALSE)

View(combined_pred)

######## SVM algorithm #########
### SVM with Linear Kernel

# set up 3-fold cross validation procedure
search_grid = expand.grid(C = seq(0, 2, length = 10))

train_control <- trainControl(
  method = "cv", 
  number = 3
)

svm.m1 = train(is_fraud ~., data = new_fraud_train, 
               method = "svmLinear", 
               trControl = train_control,
               tuneGrid = search_grid)

# results for best model
confusionMatrix(svm.m1)


### RANDOM FOREST #### 

# 
search_grid = expand.grid(.mtry = (1:5)) 

# set up 3-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

rf = train(is_fraud ~., data = new_fraud_train, 
           method = "rf",
           metric = 'Accuracy',
           trControl = train_control,
           tuneGrid = search_grid)

# top 5 models
rf$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# results for best model
confusionMatrix(rf)

pred <- predict(rf, newdata = digittest)
confusionMatrix(pred, digittest$label)


