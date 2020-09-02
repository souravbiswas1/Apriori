
#---Setting the working directory---

setwd("D:\\Zencode\\Imp\\MIQ\\Sr_Analyst_project\\Data")

#---Loading the packages---
library(sqldf)
library(plyr)
library(dplyr)
library(Hmisc)
library(arules)
library(arulesViz)
library(reshape2)
library(tidyr)

#---Reading the file
sample<-read.csv("samplecsv.csv",na.strings = c("-"," ","undefined","null","NA"))



#sample<-sample[!sample$timestamp=='',]

#---Finding the summary of the dataset---
summary(sample)

#---Finding the structure of the dataset---
str(sample)



#Some exploration for unique uid being zero and nonzero---
#---Excluding the unique uid being 0---#
sample1<-sqldf("select * from sample where unique_uid <> 0")



#---Finding the distribution of revenue in each percentile---
describe(sample)



#---Total no of missing values in the dataframe---
sum(is.na(sample))


#---omitting the missing values---
#sample<-na.omit(sample)
#sample1<-na.omit(sample1)

#---Finding the standard deviation of the revenue including & excluding the unique uid being 0---

sqrt(sum((sample$revenue-mean(sample$revenue))^2/(length(sample$revenue)-1)))
sqrt(sum((sample1$revenue-mean(sample1$revenue))^2/(length(sample1$revenue)-1)))


mean(sample$revenue,na.rm = TRUE)
mean(sample1$revenue,na.rm = TRUE)


#---writing the dataframe into a csv file---
#write.csv(sample,"fashion_df.csv")



#Finding the no of single purchaser--
single_purchaser<-sqldf("select * from sample group by user_id having count(distinct order_id)=1")


#Writing the file in csv format---
# write.csv(single_purchaser,"single_purchaser.csv")



#Finding the no of purchaser who bought multiple times--
multiple_purchaser<-sqldf("select * from sample group by user_id having count(distinct order_id) > 1")



#Writing the file in csv format---
# write.csv(multiple_purchaser,"multiple_purchaser.csv")



seg1<- select(sample,order_id,product_name)
lon1<- separate_rows(seg1, product_name, sep = ',')

#writing the file and saved in csv format---
# write.csv(lon1,"top_sold_product.csv")



seg2<- select(sample,product_name,city)
lon2<- separate_rows(seg2, product_name, sep = ',')

#writing the file and saved in csv format---
# write.csv(lon2,"top_city.csv")



seg3<- select(sample,order_id,Age.Group)
#writing the file and saved in csv format---
# write.csv(seg3,"age_product.csv")


seg4<- select(sample,product_name,revenue)
lon4<- separate_rows(seg4, product_name, sep = ',')

#writing the file and saved in csv format---
# write.csv(lon4,"top_revenue_product.csv")


#---------------------------------------------------------------------------------------------------------------------#

#Deviding the whole dataset into 3 parts---
#---for partition1---

partition1<-sqldf("select * from sample where Date between '01-10-2017' and '07-10-2017'")

sample1<-select(partition1,product_name,user_id)

long1<-separate_rows(sample1, product_name, sep=',')


trans1 <- as(split(long1[,"product_name"], long1[,"user_id"]), "transactions")

#frequentItems1 <- eclat (trans1, parameter = list(supp = 0.00002, maxlen = 5)) # calculates support for frequent items
#inspect(frequentItems1)
itemFrequencyPlot(trans1, topN=10, type="absolute", main="Item Frequency") # plot frequent items

# set the minimum support to 0.00002---
# set the minimum confidence of 0.95---
# Association rules for market basket analysis---
rules1<-apriori(trans1,parameter = list(supp = 0.00007, conf = 0.95,minlen=2 ,maxlen=5))

options(digits = 2)

rules1_sorted<-sort(rules1, by = "confidence", decreasing = T)

inspect(rules1_sorted[1:10])

df_basket1 <- as(rules1_sorted, "data.frame")

# write.csv(df_basket1,"basket1.csv")

#---------------------------------------------------------------------------------------------------------------------#

#---for partition2---

partition2<-sqldf("select * from sample where Date between '08-10-2017' and '14-10-2017'")

sample2<-select(partition2,product_name,user_id)

long2<-separate_rows(sample2, product_name, sep=',')

trans2 <- as(split(long2[,"product_name"], long2[,"user_id"]), "transactions")

#frequentItems2 <- eclat (trans2, parameter = list(supp = 0.00002, maxlen = 5)) # calculates support for frequent items
#inspect(frequentItems2)
itemFrequencyPlot(trans2, topN=10, type="absolute", main="Item Frequency") # plot frequent items

# set the minimum support to 0.00002---
# set the minimum confidence of 0.95---
# Association rules for market basket analysis---
rules2<-apriori(trans2,parameter = list(supp = 0.00007, conf = 0.95,minlen=2 ,maxlen=5))

options(digits = 2)

rules2_sorted<-sort(rules2, by = "confidence", decreasing = T)

inspect(rules2_sorted[1:10])

df_basket2 <- as(rules2_sorted, "data.frame")

# write.csv(df_basket2,"basket2.csv")


#--------------------------------------------------------------------------------------------------------------------#


#---for partition3---

partition3<-sqldf("select * from sample where Date between '15-10-2017' and '21-10-2017'")

sample3<-select(partition3,product_name,user_id)

long3<-separate_rows(sample3, product_name, sep=',')

trans3 <- as(split(long3[,"product_name"], long3[,"user_id"]), "transactions")

#frequentItems3 <- eclat (trans3, parameter = list(supp = 0.00002, maxlen = 5)) # calculates support for frequent items
#inspect(frequentItems3)
itemFrequencyPlot(trans3, topN=10, type="absolute", main="Item Frequency") # plot frequent items

# set the minimum support to 0.00002---
# set the minimum confidence of 0.95---
# Association rules for market basket analysis---
rules3<-apriori(trans3,parameter = list(supp = 0.00009, conf = 0.95,minlen=2 ,maxlen=5))

options(digits = 2)

rules3_sorted<-sort(rules3, by = "confidence", decreasing = T)

inspect(rules3_sorted[1:10])

df_basket3 <- as(rules3_sorted, "data.frame")

# write.csv(df_basket3,"basket3.csv")


#--------------------------------------------------------------------------------------------------------------#

#Cluster analysis for the statistical segmentation of the brand's audience based on Revenue---#

#Loading the necessary packages---

library(cluster)

#loading the data---
samp <- read.csv("samp.csv")

sum(is.na(samp$revenue))
summary(samp)
str(samp)


#Subsetting Data---
dat<-select(samp,revenue)


#Changing the type from numeric to dataframe---
#dat<-as.data.frame(dat)



#Omitting the missing values---
#sample <- sample[!is.na(sample$revenue),]


##--------------------------------------Step2 : Scaling the data ---------------------------------------------
#(column  - mean(column))/sd(column)

(samp[,"revenue"]-mean(samp[,"revenue"]))/sd(samp[,"revenue"])



#Finding the Z score of the all the 98831 observations---
#Creating a row no field and merging it with the scaled data---

list<-names(dat)
scaled_data<-data.frame(rownum<-1:98830)
for(i in 1:length(list))
{
  
  x<-(dat[,i]-mean(dat[,i]))/(sd(dat[,i]))
  scaled_data<-cbind(scaled_data,x)
  names(scaled_data)[i+1]<-paste("scaled_",list[i])
  print(list[i])
  
}

head(scaled_data)


#Omitting the 1st row---
scaled_data<-scaled_data[,-1]

#Binding 2 dataframes--- 
dat<-cbind(dat,scaled_data)
names(dat)


##--------------------------------------Step3 : kmeans algorithm ---------------------------------------------

#syntax : kmeans(scaled_data,k) ; where k refers to the number of clusters
set.seed(200)
names(samp)
fit.km<-kmeans(samp[,2],3)

#We will get a list object
fit.km$size #No of observations in each cluster
fit.km$withinss #Within sum of squares metric for each cluster
fit.km$totss #The total sum of squares
fit.km$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)
fit.km$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss
head(fit.km$cluster)


##--------------------------------------Step4 : find the optimal number of clusters (k value) ---------------------------------------------

#Create a screeplot-plot of cluster's tot.withinss wrt number of clusters

wss<-1:15
number<-1:15

for (i in 1:15)
  
{
  wss[i]<-kmeans(samp[,2],i)$withinss
}

#Shortlised optimal number of clusters : here the optimum no cluster is 3---

#Better plot using ggplot2---
library(ggplot2)
data<-data.frame(wss,number)
p<-ggplot(data,aes(x=number,y=wss),color="red")
p+geom_point()+scale_x_continuous(breaks=seq(1,20,1))


##--------------------------------------Step5a : Rerun the algorithm with k=3(optimal no)---------------------------------------------

#Build 4 cluster model
set.seed(100)
fit.km<-kmeans(samp[,2],3)

##Merging the cluster output with original data

head(fit.km$cluster)
samp$cluster<-fit.km$cluster


##--------------------------------------Step5b : Profile the clusters---------------------------------------------

#Cluster wise Aggregates
cmeans<-aggregate(samp[,2],by=list(samp$cluster),FUN=mean)
cmeans
dim(cmeans)


#apply(dat[,1],2,mean)
#apply(dat[,1],2,sd)

#Population Aggregates---

mean(samp[,2])
mean(samp$revenue)
  
sd(samp[,2])
sd(samp$revenue)

list1<-names(cmeans)

#Z score calculation
#z score = Cluster mean - population_mean /population_sd
y<-(cmeans[,"x"] - mean(samp[,2]))/sd(samp[,2])
cmeans<-cbind(cmeans,y)
names(cmeans)[3]<-paste("z","Score",sep="_")
names(cmeans)

# write.csv(cmeans,"cmeans.csv",row.names = F)
# write.csv(samp,"Clustered_data.csv")

#------------------------------------------------------------------------------------------------------------------#


#Merging the cluster data with the original dataset---
final_data <- merge(x = samp, y = sample, by = "user_id", all.y = TRUE)

# write.csv(final_data,"final_data.csv")

#------------------------------------------------------------------------------------------------------------------#


#Analysis of temperature and other factors---

temp_syd<- read.csv("temp_syd.csv")
temp_sydney<- read.csv("temp_sydney.csv")


temp_mel <- read.csv("temp_mel.csv")
temp_melbourne <- read.csv("temp_melbourne.csv")

temp_auc <- read.csv("temp_auc.csv")
temp_auckland <- read.csv("temp_auckland.csv")


#Merging the temperature dataset with city data---

temperature_sydney <- merge(x = temp_syd, y = temp_sydney, by = "Day", all.y = TRUE)
temperature_auckland <- merge(x = temp_auc, y = temp_auckland, by = "Day", all.y = TRUE)
temperature_melbourne <- merge(x = temp_mel, y = temp_melbourne, by = "Day", all.y = TRUE)



seg5<- select(temperature_auckland,temp,order_id)
#writing the file and saved in csv format---
# write.csv(seg5,"auckland_temp.csv")



seg6<- select(temperature_sydney,temp,order_id)
#writing the file and saved in csv format---
# write.csv(seg6,"sydney_temp.csv")




seg7<- select(temperature_auckland,dew,order_id)
#writing the file and saved in csv format---
# write.csv(seg7,"auckland_dewpoint.csv")



seg8<- select(temperature_sydney,dew,order_id)
#writing the file and saved in csv format---
# write.csv(seg8,"sydney_dewpoint.csv")



seg9<- select(temperature_auckland,wind_mph,order_id)
#writing the file and saved in csv format---
# write.csv(seg9,"auckland_wind.csv")



seg10<- select(temperature_sydney,wind_mph,order_id)
#writing the file and saved in csv format---
# write.csv(seg10,"sydney_wind.csv")



#Writing them back into csv formats---
# write.csv(temperature_auckland,"temperature_auckland.csv")
# write.csv(temperature_sydney,"temperature_sydney.csv")
# write.csv(temperature_melbourne,"temperature_melbourne.csv")


#--------------------------------------------------------------------------------------------------------------

#To find the average revenue per product---
avg_rev<-select(sample,product_name,revenue)

avg_rev_product<-separate_rows(avg_rev, product_name, sep=',')

# write.csv(avg_rev_product,"avg_rev_product.csv")









