#############Polyamorous Mate Preferences -- Budget Allocation: Analysis Script #################
####Ashley J Coventry, Tamsin German, Dan Conroy-Beam########



###load packages 
library(psych) #for scree plot
library(ggplot2) #For generating other plots
library(reshape2) #For reshaping the data
library(dplyr)
library(jmv)
library(ggpubr)
library(car)
library(tidyverse)

###Functions#### courtesy of ben lol
#numfind<-function(variablename){
 # variablename<-deparse(substitute(variablename))
  #a<-which(colnames(data)==(variablename))
  #ifelse(a>0,print(a),NA)}
#numfind(PIN)


###set seed###
set.seed(112822)


###load data###
data <- read.csv("/Users/ashle/Desktop/Research/Polyamory Research/PolyPrefs.nosync/Human Data/Processed Data/Processed Data20221129 152029.csv")


#Remove NAs ###
#necessary when merging back into the original data frame because kfits is omitting NAs. 

nacheck <- apply(data[,14:27], 1, function(x) sum(is.na(x))>0)
longData<- data[!nacheck,]



###reshape data wide --> long and only keep columns relevant for kmeans

#change col names for attract so that long form doesn't have attract

colnames(longData)[c(15, 22)] <- c("idealBlue", "idealOrange" ) 
    #so that we can separate ideal orange and ideal blue
    

#create separate dfs for each trait

longdata_attract <-melt(longData[,c(102, 5, 1, 15, 22)], id.vars=c("PIN", "gender", "age"))
colnames(longdata_attract) <- c("ID", "gender", "age", "partnertype", "attractiveness")
longdata_finance <-melt(longData[,c(102, 20, 27)], id.vars=c("PIN"))
colnames(longdata_finance) <- c("ID2", "partnertyperepeat", "financial_prospects")
longdata_ambition <-melt(longData[,c(102,14,21)], id.vars=c("PIN"))
colnames(longdata_ambition) <- c("ID3", "partnertyperepeat2", "ambition")
longdata_kind <-melt(longData[,c(102,18,25)], id.vars=c("PIN"))
colnames(longdata_kind) <- c("ID4", "partnertyperepeat3", "kindness")
longdata_intel <-melt(longData[,c(102,16,23)], id.vars=c("PIN"))
colnames(longdata_intel) <- c("ID5", "partnertyperepeat4", "intelligence")
longdata_sexy <-melt(longData[,c(102,17,24)], id.vars=c("PIN"))
colnames(longdata_sexy) <- c("ID6", "partnertyperepeat5", "sexiness")
longdata_status <-melt(longData[,c(102, 19, 26)], id.vars=c("PIN"))
colnames(longdata_status) <- c("ID7", "partnertyperepeat6", "status")



#rbind 

longData <- cbind(longdata_attract, longdata_finance, longdata_ambition, longdata_kind,
               longdata_intel, longdata_sexy, longdata_status)


#remove extra partnertype columns and extra ID columns

longData <- subset(longData, select= -c(partnertyperepeat, partnertyperepeat2, 
                                  partnertyperepeat3, partnertyperepeat4, 
                                  partnertyperepeat5, partnertyperepeat6, 
                                  ID2, ID3, ID4, ID5, ID6, ID7))



###Ipsatizing
#takes mean rating of all traits and subtracting that mean from indiv ratings
#excludes age

longData[,5:11] <- as.numeric(unlist(longData[,5:11]))

##z-score 
longData[,5:11] <- apply(longData[,5:11],2,scale)


##ipsatize z-scored value 
longData$mean<-sapply(unique(longData$ID),function(x) 
  mean(unlist(longData[longData$ID==x,5:11])))

longData[,5:11]<-longData[,5:11] - longData$mean



###K-Means Cluster Analysis###

#Extract 1 group
kfit1<-kmeans((longData[,5:11]),1)
#Extract 2 groups
kfit2<-kmeans((longData[,5:11]),2)
#Extract 3 groups
kfit3<-kmeans((longData[,5:11]),3)
#Extract 4 groups
kfit4<-kmeans((longData[,5:11]),4)
#Extract 5 groups
kfit5<-kmeans((longData[,5:11]),5)
#Extract 6 groups
kfit6<-kmeans((longData[,5:11]),6)
#Extract 7 groups
kfit7<-kmeans((longData[,5:11]),7)


##Scree Plot

#Extract the total withinss from each model and assign it to a data structure, k

k<-c(kfit1$tot.withinss, kfit2$tot.withinss, kfit3$tot.withinss, kfit4$tot.withinss, kfit5$tot.withinss, kfit6$tot.withinss, kfit7$tot.withinss)

#generate scree plot

dim<-c(1:7)
screedata<-data.frame(dim,k)
screeplot<-qplot(screedata$dim,screedata$k)
screeplot

##see how many clusters

diff(sapply(1:7, function(x) kmeans(longData[,5:11], x)$tot.withinss))


##Add this classification to the original dataframe

longData$kfit3<-kfit3$cluster


###Create vectors of means of each trait for every cluster (w/out age)###

##Group 1

m1a <-mean(longData$attractiveness[longData$kfit3==1])
m1b <-mean(longData$financial_prospects[longData$kfit3==1])
m1c <-mean(longData$ambition[longData$kfit3==1])
m1d <-mean(longData$kindness[longData$kfit3==1])
m1e <-mean(longData$sexiness[longData$kfit3==1])
m1f <-mean(longData$status[longData$kfit3==1])
m1g <-mean(longData$intelligence[longData$kfit3==1])

table(longData$gender[longData$kfit3==1]) #look at gender separately

g1m <- c(m1a, m1b, m1c, m1d, m1e, m1f, m1g) 

##Group 2##

m2a <-mean(longData$attractiveness[longData$kfit3==2])
m2b <-mean(longData$financial_prospects[longData$kfit3==2])
m2c <-mean(longData$ambition[longData$kfit3==2])
m2d <-mean(longData$kindness[longData$kfit3==2])
m2e <-mean(longData$sexiness[longData$kfit3==2])
m2f <-mean(longData$status[longData$kfit3==2])
m2g <-mean(longData$intelligence[longData$kfit3==2])

table(longData$gender[longData$kfit3==2])

g2m <- c(m2a, m2b, m2c, m2d, m2e, m2f, m2g)


##Group 3##

m3a <-mean(longData$attractiveness[longData$kfit3==3])
m3b <-mean(longData$financial_prospects[longData$kfit3==3])
m3c <-mean(longData$ambition[longData$kfit3==3])
m3d <-mean(longData$kindness[longData$kfit3==3])
m3e <-mean(longData$sexiness[longData$kfit3==3])
m3f <-mean(longData$status[longData$kfit3==3])
m3g <-mean(longData$intelligence[longData$kfit3==3])

table(longData$gender[longData$kfit3==3])

g3m <- c(m3a, m3b, m3c, m3d, m3e, m3f, m3g)


###compute variance between trait ratings for each cluster

#cluster 1
var(g1m)
#cluster 2
var(g2m)
#cluster 3
var(g3m)



###Compute same or diff variable for ideal orange v ideal blue
#do people want both of their ideal partners in the same cluster or in different ones? 

##reshape data again: long --> wide
#note: why does it need to be reshaped? 

dataWide <- dcast(longData, ID + gender + age ~ partnertype, value.var="kfit3")
#ID, gender, and age = ID variables, separate kfit by partnertype (orange v blue)

#Rename the columns in our new wide dataframe
colnames(dataWide)<-c("ID","gender","age","kfitBlue","kfitOrange")


##Compute sameordiff
dataWide$sameordiff<-ifelse(dataWide$kfitOrange == dataWide$kfitBlue, 0, 1)
table(dataWide$sameordiff[dataWide$gender==1]) #1 = female
table(dataWide$sameordiff[dataWide$gender==2]) #2 = male
  #does it matter if we do sex v gender?

##Computing average differentness
avgdiff <- mean(dataWide$sameordiff)

###create variable listing cluster of each partner 

#make kfit cluster a factor 
dataWide$kfitOrange <- as.factor(dataWide$kfitOrange)
dataWide$kfitBlue <- as.factor(dataWide$kfitBlue)

#create variable
dataWide$kfitab <- ifelse(dataWide$kfitOrange == 1& dataWide$kfitBlue ==1, "1, 1",
                          ifelse(dataWide$kfitOrange ==1 & dataWide$kfitBlue ==2, "1, 2",
                                 ifelse(dataWide$kfitOrange ==1 & dataWide$kfitBlue ==3, "1, 3",
                                        ifelse(dataWide$kfitOrange ==2 & dataWide$kfitBlue ==1, "2, 1",
                                               ifelse(dataWide$kfitOrange ==2 & dataWide$kfitBlue ==2, "2, 2",
                                                      ifelse(dataWide$kfitOrange ==2 & dataWide$kfitBlue ==3, "2, 3",
                                                             ifelse(dataWide$kfitOrange ==3 & dataWide$kfitBlue ==1, "3, 1",
                                                                    ifelse(dataWide$kfitOrange ==3 & dataWide$kfitBlue ==2, "3, 2",
                                                                           ifelse(dataWide$kfitOrange ==3 & dataWide$kfitBlue ==3, "3, 3",NA)))))))))







### CHI SQUARE ###

##are men and women are choosing combos of partner orange and blue at diff rates?
chisq.test(table(dataWide$gender, dataWide$kfitab))

##do preferences for partner orange predict preferences for partner blue?
chisq.test(table(dataWide$kfitOrange, dataWide$kfitBlue)) 

##raw numbers in each cluster combo
table(dataWide$kfitOrange, dataWide$kfitBlue)

##looking at proportions of men v women choosing combos of partner a and b in each cluster
table(dataWide$gender[dataWide$kfitOrange == 1 & dataWide$kfitBlue ==1])
table(dataWide$gender[dataWide$kfitOrange == 1 & dataWide$kfitBlue ==2])
table(dataWide$gender[dataWide$kfitOrange == 1 & dataWide$kfitBlue ==3])
table(dataWide$gender[dataWide$kfitOrange == 2 & dataWide$kfitBlue ==1])
table(dataWide$gender[dataWide$kfitOrange == 2 & dataWide$kfitBlue ==2])
table(dataWide$gender[dataWide$kfitOrange == 2 & dataWide$kfitBlue ==3])
table(dataWide$gender[dataWide$kfitOrange == 3 & dataWide$kfitBlue ==1])
table(dataWide$gender[dataWide$kfitOrange == 3 & dataWide$kfitBlue ==2])
table(dataWide$gender[dataWide$kfitOrange == 3 & dataWide$kfitBlue ==3])#/table(dataWide$gender)



### PERMUTATIONS ###

##create blank data frame to store null distribution averages
nullDistAvg <- data.frame(matrix(0,1,10000))

##for loop to generate data of null dist
for(a in 1:10000){
  #creating vector of clusters that are random, keeping proportions of each group the same
  nullClusterVector <- sample(longData$kfit3)
  dataLongNull <- cbind(longData[,1:13], nullClusterVector) 
  dataWideNull <- dcast(dataLongNull, ID + gender + age ~ partnertype, value.var="nullClusterVector")
  #Rename the columns in our new wide dataframe
  colnames(dataWideNull)<-c("ID","gender","age","kfitBlue","kfitOrange")
  #Compute sameordiff
  dataWideNull$sameordiff<-ifelse(dataWideNull$kfitBlue == dataWideNull$kfitOrange, 0, 1)
  #Computing average differentness
  avgDiffNull <- mean(dataWideNull$sameordiff)
  #stat we want to save in the matrix
  nullDistAvg[1,a]<-avgDiffNull
}



##see whether we are in extremes of null dist (compare output to our avgdiff)

nullDistHigh<-quantile(nullDistAvg[,1:10000],c(0.975))
nullDistLow<-quantile(nullDistAvg[,1:10000],c(0.025)) 

##see proportion of null dist values that are smaller than our avgdiff 
sum(unlist(nullDistAvg) < avgdiff) 



#convert to p-value, divide by number of shuffles (from for loop)
sum(unlist(nullDistAvg) < avgdiff) /10000 


###How many ideal partners are in each cluster?

##for people who wanted both partners in same cluster
table(dataWide$kfitOrange[dataWide$sameordiff ==0])

##for people who wanted partners in different clusters
table(dataWide$kfitOrange[dataWide$sameordiff ==1])  
table(dataWide$kfitBlue[dataWide$sameordiff ==1]) 

###More Chi Squares: proportion of participants with at least 1 partner in specific clusters 
#(run depending on results of previous analyses)

##Create blank columns in dataWide
dataWide$oneAttractive <- NA
dataWide$oneWealthy <- NA

##for loop to fill columns
for(i in 1:NROW(dataWide)){
  ifelse(dataWide$kfitOrange[i] == 1 | dataWide$kfitBlue[i] == 1, dataWide$oneAttractive[i]<-1, dataWide$oneAttractive[i]<-0)
  
}
for(i in 1:NROW(dataWide)){
  ifelse(dataWide$kfitOrange[i] == 3 | dataWide$kfitBlue[i] == 3, dataWide$oneWealthy[i]<-1, dataWide$oneWealthy[i]<-0)
  
}

chisq.test(table(dataWide$gender, dataWide$oneAttractive))
chisq.test(table(dataWide$gender, dataWide$oneWealthy))






### Plotting ###

##plot bar graph with each trait mean for each 3 clusters (# clusters depends on scree)
meanTrait <- c(g1m, g2m, g3m)
mateType <-c(rep("1", 7), rep("2", 7), rep("3", 7))
trait <- c(rep(c("Attractiveness", "Resources", "Ambition", "Kindness", "Good in Bed", "Status", "Intelligence"), 3))  
plotting <- data.frame(meanTrait, mateType, trait)
kfitPlot <- ggplot(data=plotting, aes(x=mateType, y=meanTrait, fill=trait)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal(base_size = 15) + xlab("Type of Mate") + ylab("Relative Desired Trait Level") +
  scale_fill_discrete(name = "Trait")
ggsave("3ClustersAllTraitsByPart.jpeg", plot=last_plot(), width=200, height=150, units="mm", path ="/Users/ashle/Desktop/Research/Polyamory Research/PolyPrefs.nosync/Figures", scale = 1, dpi=300, limitsize=TRUE)

##multipanel figure

#first create individual plot for each cluster

#cluster 1 (title will change based on clusters)
#meanTrait1 <- g1m
#trait1 <- c("Attractiveness", "Resources", "Ambition", "Kindness", "Good in Bed", "Status", "Intelligence")
#plotting1 <- data.frame(meanTrait1, trait1)
#plot1 <- ggplot(data=plotting1, aes(x=trait1, y=meanTrait1)) +
  #geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  #theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-0.7,0.7) +
 #ggtitle("Attractive & Good in Bed") +theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 (title will change based on clusters)
#meanTrait2 <- g2m
#trait2 <- c("Attractiveness", "Resources", "Ambition", "Kindness", "Good in Bed", "Status", "Intelligence")
#plotting2 <- data.frame(meanTrait2, trait2)
#plot2 <- ggplot(data=plotting2, aes(x=trait2, y=meantraitlevel2)) +
  #geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  #theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-0.7,0.7) +
  #ggtitle("Well-Rounded") +theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 (title will change based on clusters)
#meanTrait3 <- g3m
#trait3 <- c("Attractiveness", "Resources", "Ambition", "Kindness", "Good in Bed", "Status", "Intelligence")
#plottingdf3 <- data.frame(meanTrait3, trait3)
#plot3 <- ggplot(data=plotting3, aes(x=trait3, y=meanTrait3)) +
  #geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  #theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-0.7,0.7) +
  #ggtitle("Smart & Wealthy") +theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))


#combine clusters into one graph
#panelPlot<-ggarrange(plot1,plot2,plot3,labels=c("A","B","C"), nrow=1, ncol=3,font.label = list(size = 14, color = "black"))
#panelPlot








