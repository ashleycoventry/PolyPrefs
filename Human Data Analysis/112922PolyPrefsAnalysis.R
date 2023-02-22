#############Polyamorous Mate Preferences -- Budget Allocation: Analysis Script #################
####Ashley J Coventry, Tamsin German, Dan Conroy-Beam########



###load packages 
library(psych) #for scree plot
library(ggplot2) #For generating other plots
library(ggpubr)
library(data.table) #for reshaping data
library(stringr) #for permutation analy data reorganizing
library(lme4) #for the logistic regression

###set seed###
set.seed(112822)


###load data###
data <- read.csv("Human Data/Processed Data/Processed Data20230108 181609.csv")


#Remove NAs ###
#necessary when merging back into the original data frame because kfits is omitting NAs. 

nacheck <- apply(data[,14:27], 1, function(x) sum(is.na(x))>0)
data<- data[!nacheck,]


#excluding people who don't identify as either men or women

data<-data[data$gender<3,]
data <- data[data$orange_gender <3,]
data <- data[data$blue_gender <3,]

#recoding gender to be 0 and 1 (move to processing script)
data$gender <- ifelse(data$gender == 1, 0, 1)
data$orange_gender <- ifelse(data$orange_gender ==1, 0, 1)
data$blue_gender <- ifelse(data$blue_gender ==1 , 0, 1)


###reshape data wide --> long and only keep columns relevant for kmeans

#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

longData<-melt(as.data.table(data),id.vars=c("PIN","gender","age"),
               measure.vars=list(c(30,28),
                                 c(14,21),
                                 c(15,22),
                                 c(16,23),
                                 c(17,24),
                                 c(18,25),
                                 c(19,26),
                                 c(20,27)))


#Relabel columns

colnames(longData)[4:12]<-c("partner","idealGender","amb","attract",
                            "intel","sexy","kind",
                            "stat","finPros")


#Relabel partner type values
longData$partner<-as.factor(ifelse(longData$partner==1,"idealBlue","idealOrange"))



###K-Means Cluster Analysis###


#extract kmeans wSs
kfitWss<-sapply(1:7,function(x) kmeans(longData[,6:12],x)$tot.withinss)

#scree plot
screePlot<-qplot(1:7,kfitWss)


##compute differences in within ss across k for k-means clustering
wssDiffs<-diff(kfitWss)


##Add this classification to the original dataframe

kFit<-kmeans(longData[,6:12],3)
longData$kFitab <- kFit$cluster


##Create vectors of preference means for each cluster (without age)
clustCenters<-kFit$centers

##Look at gender breakdown by cluster
clustGender<-table(longData$gender,longData$kFitab)

##compute variance between trait ratings for each cluster
#to see if maybe one cluster is more well rounded than others
clustVars<-apply(clustCenters,1,var)




###Compute same or diff variable for ideal orange v ideal blue
#do people want both of their ideal partners in the same cluster or in different ones? 

##Transfer partner clusters back to data (since "data" is wide)
data$blueClust<-longData$kFitab[longData$partner=="idealBlue"]
data$orangeClust<-longData$kFitab[longData$partner=="idealOrange"]

##Determine for each participant whether their orange and blue partners are in the same cluster
#same = 1, diff = 0
data$sameOrDiff<-apply(data[,133:134],1,function(x)
  sum(duplicated(x))
)

##get sameOrDiff as function of gender
genderDiff<-table(data$sameOrDiff,data$gender)


##Computing average differentness
avgDiff <- mean(data$sameOrDiff)

###create variable listing cluster of each partner 

data$kFitab<-apply(data[,133:134],1,function(x) paste0(sort(as.numeric(x)),collapse=","))


### CHI SQUARE 

#are men and women are choosing combos of partner orange and blue at diff rates?

chisqGender<-chisq.test(table(data$gender,data$kFitab))


##do preferences for partner orange predict preferences for partner blue?
chisqClust<-chisq.test(table(data$blueClust, data$orangeClust)) 

##raw numbers in each cluster combo by gender
clustComboGender<-table(data$blueClust,data$orangeClust,data$gender)



### Permuation Analysis ###


##create a vector to store null distribution averages
nullDistAvg <- rep(0,100000)

##for loop to generate data of null dist 
for(a in 1:100000){
  
  aNull<-sample(data$blueClust)
  bNull<-sample(data$orangeClust)
  
  #Compute sameordiff
  sodNull<-ifelse(aNull==bNull, 0, 1)
  
  #Compute and saving average differentness
  nullDistAvg[a]<-mean(sodNull)
  
}



##see whether we are in extremes of null dist (compare output to our avgdiff)

nullDistHigh<-quantile(nullDistAvg[1:100000],c(0.975)) 
nullDistLow<-quantile(nullDistAvg[1:100000],c(0.025)) 


#Compute p-value comparing observed sameordiff to null distribution
pValueDiff <- sum(nullDistAvg < avgDiff) /100000 #unlike in study 1, no more similar/diff than chance 


###How many ideal partners are in each cluster?

##for people who wanted both partners in same cluster
sameClust <- table(data$blueClust[data$sameOrDiff == 0]) 

##for people who wanted partners in different clusters
#don't need to run separately for clusters A and B bc made order arbitrary
diffClust <- table(data$blueClust[data$sameOrDiff ==1])  


###More Chi Squares: proportion of participants with at least 1 partner in specific clusters 

##Create blank columns in data
data$clust1 <- NA #for cluster 1
data$clust2<- NA #for cluster 2
data$clust3 <- NA #for cluster 3


##for loop to fill columns
for(i in 1:NROW(data)){
  ifelse(data$orangeClust[i] == 1 | data$blueClust[i] == 1, data$clust1[i]<-1, data$clust1[i]<-0)
  
}

for(i in 1:NROW(data)){
  ifelse(data$orangeClust[i] == 2 | data$blueClust[i] == 2, data$clust2[i]<-1, data$clust2[i]<-0)
  
}

for(i in 1:NROW(data)){
  ifelse(data$orangeClust[i] == 3 | data$blueClust[i] == 3, data$clust3[i]<-1, data$clust3[i]<-0)
  
}



chisqClust1 <- chisq.test(table(data$gender, data$clust1))
chisqClust2 <- chisq.test(table(data$gender, data$clust2))
chisqClust3 <- chisq.test(table(data$gender, data$clust3))

###Ideal gender analysis

##fisher test

#partner blue
fisherIdealGenderBlue <- fisher.test(table(longData$idealGender[longData$partner == "idealBlue"], longData$kFitab[longData$partner == "idealBlue"])) 
idealGenderClustBlue <- table(longData$idealGender[longData$partner == "idealBlue"], longData$kFitab[longData$partner == "idealBlue"])

#partner orange
fisherIdealGenderOrange <- fisher.test(table(longData$idealGender[longData$partner == "idealOrange"], longData$kFitab[longData$partner == "idealOrange"])) 
idealGenderClustOrange <- table(longData$idealGender[longData$partner == "idealOrange"], longData$kFitab[longData$partner == "idealOrange"])




##logistic regression
#predicting partner sex from cluster?


logRegModelPsex <- glmer(idealGender ~ kFitab + (1|PIN), data = longData, family = "binomial") 






### Plotting ###

##plot bar graph with each trait mean for each 3 clusters (# clusters depends on scree)
#clusters aren't mapping on in the right order! double check centers with graph after running
meanTrait <- c(clustCenters[1,], clustCenters[2,], clustCenters[3,])
mateType <-c(rep("1", 7), rep("2", 7), rep("3", 7))
trait <- c(rep(c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources"), 3))  
plotting <- data.frame(meanTrait, mateType, trait)
kFitPlot <- ggplot(data=plotting, aes(x=mateType, y=meanTrait, fill=trait)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal(base_size = 15) + xlab("Type of Mate") + ylab("Desired Trait Level") +
  scale_fill_discrete(name = "Trait")


##multipanel figure
#first create individual plot for each cluster

#cluster 1 (title will change based on clusters)
meanTrait1 <- clustCenters[1,]
trait1 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting1 <- data.frame(meanTrait1, trait1)
plot1 <- ggplot(data=plotting1, aes(x=trait1, y=meanTrait1)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(0,8) +
  ggtitle("Kind and Smart") +theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2 <- clustCenters[2,]
trait2 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2 <- data.frame(meanTrait2, trait2)
plot2 <- ggplot(data=plotting2, aes(x=trait2, y=meanTrait2)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(0,8) +
ggtitle("Attractive and Good in Bed") +theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3 <- clustCenters[3,]
trait3 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting3 <- data.frame(meanTrait3, trait3)
plot3 <- ggplot(data=plotting3, aes(x=trait3, y=meanTrait3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(0,8) +
ggtitle("Well-Rounded") +theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#combine clusters into one graph
panelPlot<-ggarrange(plot1,plot2,plot3,labels=c("A","B","C"), nrow=1, ncol=3,font.label = list(size = 14, color = "black"))


###Mate Point Allocation###

#question: are people who want partners in different clusters allocating a sig different # of points to each partner?

##calculate total # of points allocated per row for traits

#for partner Orange
data$totalOrange <- NA
for(i in 1:nrow(data)) {
  data$totalOrange[i] <- sum(data[i, 21:27])
}

#for partner Blue
data$totalBlue <- NA
for(i in 1:nrow(data)) {
  data$totalBlue[i] <- sum(data[i, 14:20])
}


##compare between partner blue and orange for each participant
data$alloComp <- NA

#if blue > orange, give value 2; if blue = orange, give value 1
#if neither is true ( so blue < orange), give 0. 
for(i in 1:nrow(data)){
  if(data$totalBlue[i] > data$totalOrange[i]) {
    data$alloComp[i] <- 2 
  } else {
    if(data$totalBlue[i] == data$totalOrange[i]) {
      data$alloComp[i] <- 1
    } else{
      data$alloComp[i] <- 0 
      }
    }
}

##look at cluster and alloComp values?
alloTable <- table(data$alloComp)
alloClust <- table(data$alloComp, data$blueClust)
#ok so idk how to analyze --> do i need to just add to longData?


  
  
  
  
  





###################Supplemental Analyses################

#######Bisexual Poly Prefs #######

##how many bisexual people are in our sample?
biSamp <- table(data$sex_orient)

##get only bi dataframe
biData <- subset(data, data$sex_orient == 3)

biSampGender <- table(biData$gender) #22 women, 9 men

##get sameOrDiff as function of gender
biGenderDiff<-table(biData$sameOrDiff,biData$gender)


##Computing average differentness
biAvgDiff <- mean(biData$sameOrDiff) ##way higher than reg avg diff

### CHI SQUARE -- some use Fisher's exact test since some clusters are rare###

#are bi men and women are choosing combos of partner orange and blue at diff rates than het folks?

chisqSexOrient <-chisq.test(table(data$sex_orient,data$kFitab)) #error bc size too small
#fisherSexOrient <- fisher.test(table(data$sex_orient, data$kFitab))
#get error with Fisher's test

#are bi men and bi women choosing combos at diff rates

fisherGenderBi <- fisher.test(table(biData$gender, biData$kFitab))



##in bisexual participants, do preferences for partner orange predict preferences for partner blue?
fisherClustBi <- fisher.test(table(biData$blueClust, biData$orangeClust))

##raw numbers in each cluster combo by gender
clustComboGenderBi<-table(biData$blueClust,biData$orangeClust,biData$gender)


#######Sex-Separated Cluster Analyses##########

###Women Only : K-Means Cluster Analysis###

longDataF <- longData[longData$gender == 0]
#extract kmeans wSs
kfitWssF<-sapply(1:7,function(x) kmeans(longDataF[,6:12],x)$tot.withinss)

#scree plot
screePlotF<-qplot(1:7,kfitWssF)


##compute differences in within ss across k for k-means clustering
wssDiffsF<-diff(kfitWssF)


##Add this classification to the original dataframe

kFitF<-kmeans(longDataF[,6:12],4)
longDataF$kFitF <- kFitF$cluster


##Create vectors of preference means for each cluster (without age)
clustCentersF<-kFitF$centers

##Look at cluster breakdown
clustF<-table(longDataF$kFitF)

##compute variance between trait ratings for each cluster
#to see if maybe one cluster is more well rounded than others
clustVarsF<-apply(clustCentersF,1,var)


##plotting clusters (only women)

#cluster 1 (title will change based on clusters)
meanTrait1F <- clustCentersF[1,]
trait1F <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting1F <- data.frame(meanTrait1F, trait1F)
plot1F <- ggplot(data=plotting1F, aes(x=trait1F, y=meanTrait1F)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(0,9) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2F <- clustCentersF[2,]
trait2F <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2F <- data.frame(meanTrait2F, trait2F)
plot2F <- ggplot(data=plotting2F, aes(x=trait2F, y=meanTrait2F)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(0,9) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3F <- clustCentersF[3,]
trait3F <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting3F <- data.frame(meanTrait3F, trait3F)
plot3F <- ggplot(data=plotting3F, aes(x=trait3F, y=meanTrait3F)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(0,9) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 4 
meanTrait4F <- clustCentersF[4,]
trait4F <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting4F <- data.frame(meanTrait4F, trait4F)
plot4F <- ggplot(data=plotting4F, aes(x=trait4F, y=meanTrait4F)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "blue")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(0,9) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))



#Panel Plot
panelPlotF<-ggarrange(plot1F,plot2F,plot3F, plot4F, labels=c("A","B","C","D"), nrow=1, ncol=4,font.label = list(size = 14, color = "black"))




###Men only : K-Means Cluster Analysis

###K-Means Cluster Analysis###

longDataM <- longData[longData$gender == 1]

#extract kmeans wSs
kfitWssM<-sapply(1:7,function(x) kmeans(longDataM[,6:12],x)$tot.withinss)

#scree plot
screePlotM<-qplot(1:7,kfitWssM)


##compute differences in within ss across k for k-means clustering
wssDiffsM<-diff(kfitWssM)


##Add this classification to the original dataframe

kFitM<-kmeans(longDataM[,6:12],3)
longDataM$kFitM <- kFitM$cluster


##Create vectors of preference means for each cluster (without age)
clustCentersM<-kFitM$centers

##Look at gender breakdown by cluster
clustM<-table(longDataM$kFitM)

##compute variance between trait ratings for each cluster
#to see if maybe one cluster is more well rounded than others
clustVarsM<-apply(clustCentersM,1,var)


##plotting clusters (only men)

#cluster 1 (title will change based on clusters)
meanTrait1M <- clustCentersM[1,]
trait1M <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting1M <- data.frame(meanTrait1M, trait1M)
plot1M <- ggplot(data=plotting1M, aes(x=trait1M, y=meanTrait1M)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(0,9) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2M <- clustCentersM[2,]
trait2M <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2M <- data.frame(meanTrait2M, trait2M)
plot2M <- ggplot(data=plotting2M, aes(x=trait2M, y=meanTrait2M)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(0,9) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3M <- clustCentersM[3,]
trait3M <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting3M <- data.frame(meanTrait3M, trait3M)
plot3M <- ggplot(data=plotting3M, aes(x=trait3M, y=meanTrait3M)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(0,9) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#panel plot
panelPlotM<-ggarrange(plot1M,plot2M,plot3M,labels=c("A","B","C"), nrow=1, ncol=3,font.label = list(size = 14, color = "black"))

