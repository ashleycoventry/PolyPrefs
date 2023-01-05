#############Polyamorous Mate Preferences -- Budget Allocation: Analysis Script #################
####Ashley J Coventry, Tamsin German, Dan Conroy-Beam########



###load packages 
library(psych) #for scree plot
library(ggplot2) #For generating other plots
library(data.table) #for reshaping data
library(stringr) #for permutation analy data reorganizing
library(reshape2) # for permutation analysis data reorganizing

###set seed###
set.seed(112822)


###load data###
data <- read.csv("Human Data/Processed Data/Processed Data20221129 152029.csv")


#Remove NAs ###
#necessary when merging back into the original data frame because kfits is omitting NAs. 

nacheck <- apply(data[,14:27], 1, function(x) sum(is.na(x))>0)
data<- data[!nacheck,]


#excluding people who don't identify as either men or women

data<-data[data$gender<3,]


###reshape data wide --> long and only keep columns relevant for kmeans

#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

longData<-melt(as.data.table(data),id.vars=c("PIN","gender","age"),
               measure.vars=list(c(14,21),
                                 c(15,22),
                                 c(16,23),
                                 c(17,24),
                                 c(18,25),
                                 c(19,26),
                                 c(20,27)))


#Relabel columns

colnames(longData)[4:11]<-c("partner","amb","attract",
                            "intel","sexy","kind",
                            "stat","finPros")


#Relabel partner type values
longData$partner<-as.factor(ifelse(longData$partner==1,"idealBlue","idealOrange"))





###K-Means Cluster Analysis###


#extract kmeans wSs
kfitWss<-sapply(1:7,function(x) kmeans(longData[,5:11],x)$tot.withinss)

#scree plot
##note: qplot isn't working
screePlot<-qplot(1:7,kfitWss)


##compute differences in within ss across k for k-means clustering
wssDiffs<-diff(kfitWss)


##Add this classification to the original dataframe

longData$kfit3<-kmeans(longData[,5:11],3)$cluster


##Create vectors of preference means for each cluster (without age)
#note: idk how this works instead of the indiv mean calculations we did
clustCenters<-kmeans(longData[,5:11],3)$centers

##Look at gender breakdown by cluster #1 = women, #2 = men
clustGender<-table(longData$gender,longData$kfit3)

##compute variance between trait ratings for each cluster
#to see if maybe one cluster is more well rounded than others
clustVars<-apply(clustCenters,1,var)




###Compute same or diff variable for ideal orange v ideal blue
#do people want both of their ideal partners in the same cluster or in different ones? 

##Transfer partner clusters back to data (since "data" is wide)
data$blueClust<-longData$kfit3[longData$partner=="idealBlue"]
data$orangeClust<-longData$kfit3[longData$partner=="idealOrange"]

##Determine for each participant whether their orange and blue partners are in the same cluster
#same = 1, diff = 0
data$sameOrDiff<-apply(data[,132:133],1,function(x)
  sum(duplicated(x))
)

##get sameOrDiff as function of gender
genderDiff<-table(data$sameOrDiff,data$gender)


##Computing average differentness
avgDiff <- mean(data$sameOrDiff)

###create variable listing cluster of each partner 

data$kFitab<-apply(data[,132:133],1,function(x) paste0(sort(as.numeric(x)),collapse=","))


### CHI SQUARE -- Fisher's exact test since some clusters are rare###

#are men and women are choosing combos of partner orange and blue at diff rates?
#Some combinations still too rare; try a Fisher's exact test instead
chisqGender<-chisq.test(table(data$gender,data$kFitab))
fisherGender <- fisher.test(table(data$gender, data$kFitab))

##do preferences for partner orange predict preferences for partner blue?
chisqClust<-chisq.test(table(data$blueClust, data$orangeClust)) 
fisherClust <- fisher.test(table(data$blueClust, data$orangeClust))

##raw numbers in each cluster combo by gender
clustComboGender<-table(data$blueClust,data$orangeClust,data$gender)



### PERMUTATIONS ###


##create blank data frame to store null distribution averages
nullDistAvg <- data.frame(matrix(0,1,10000))

##for loop to generate data of null dist ##NOT WORKING
for(a in 1:10000){
  #creating vector of clusters that are random, keeping proportions of each group the same
  nullClusterVector <- sample(data$kFitab)
  cols <- c(1, 2, 5, 102)
  dataNull <- cbind(data[,cols], nullClusterVector)  
  #split kFitab into kFita and kFitb so we can compare sameOrDiff
  dataNull[c('kFita', 'KFitb')] <- str_split_fixed(dataNull$kFitab, ',', 2)
  #Compute sameordiff
  dataNull$sameOrDiff<-ifelse(dataNull$kfita == dataNull$kfitb, 0, 1)
  #Computing average differentness
  avgDiffNull <- mean(dataNull$sameOrDiff)
  #stat we want to save in the matrix
  nullDistAvg[1,a]<-avgDiffNull
}



##see whether we are in extremes of null dist (compare output to our avgdiff)

nullDistHigh<-quantile(nullDistAvg[,1:10000],c(0.975))
nullDistLow<-quantile(nullDistAvg[,1:10000],c(0.025)) 

##see proportion of null dist values that are smaller than our avgdiff 
propDiff <- sum(unlist(nullDistAvg) < avgdiff) 


#convert to p-value, divide by number of shuffles (from for loop)
pValueDiff <- sum(unlist(nullDistAvg) < avgdiff) /10000 


###How many ideal partners are in each cluster?

##for people who wanted both partners in same cluster
sameClust <- table(data$kfita[data$sameordiff == 0]) 

##for people who wanted partners in different clusters
diffClustA <- table(data$kfita[data$sameordiff ==1])  
diffClustB <- table(data$kfitb[data$sameordiff ==1]) 

###More Chi Squares: proportion of participants with at least 1 partner in specific clusters 

##Create blank columns in dataWide
data$oneKind <- NA #for cluster 1
data$oneWellRounded <- NA #for cluster 3
data$oneSexy<- NA #for cluster 2

##for loop to fill columns
for(i in 1:NROW(data)){
  ifelse(data$kFitb[i] == 1 | data$kFita[i] == 1, data$oneKind[i]<-1, data$oneKind[i]<-0)
  
}
for(i in 1:NROW(data)){
  ifelse(data$kFitb[i] == 3 | data$kFita[i] == 3, data$oneWellRounded[i]<-1, data$oneWellRounded[i]<-0)
  
}

for(i in 1:NROW(data)){
  ifelse(data$kFitb[i] == 2 | data$kFita[i] == 2, data$oneSexy[i]<-1, data$oneSexy[i]<-0)
  
}

chisqKind <- chisq.test(table(data$gender, data$oneKind))
chisqWellRounded <- chisq.test(table(data$gender, data$oneWellRounded))
chisqSexy <- chisq.test(table(data$gender, data$oneSexy))







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





#######Bisexual Poly Prefs #######

##how many bisexual people are in our sample?
biSamp <- table(data$sex_orient)

##get only bi dataframe
biData <- subset(data, data$sex_orient == 3)




