#############Polyamorous Mate Preferences 2 -- Budget Allocation: Analysis Script #################
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
data <- read.csv("/Users/ashle/Desktop/Research/Polyamory Research/PolyPrefs.nosync/Human Data/Processed Data/PolyPrefs2. Processed Data20221129 152029.csv")


#Remove NAs ###
#necessary when merging back into the original data frame because kfits is omitting NAs. 

nacheck <- apply(data[,14:27], 1, function(x) sum(is.na(x))>0)
data<- data[!nacheck,]


#excluding people who don't identify as either men or women --> should move to processing script

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
data$sameOrDiff<-apply(data[,132:133],1,function(x)
  sum(duplicated(x))
)

##get sameOrDiff as function of gender
genderDiff<-table(data$sameOrDiff,data$gender)


##Computing average differentness
avgDiff <- mean(data$sameOrDiff)

###create variable listing cluster of each partner 

data$kFitab<-apply(data[,132:133],1,function(x) paste0(sort(as.numeric(x)),collapse=","))


### CHI SQUARE 

#are men and women are choosing combos of partner orange and blue at diff rates?

chisqGender<-chisq.test(table(data$gender,data$kFitab))


##do preferences for partner orange predict preferences for partner blue?
chisqClust<-chisq.test(table(data$blueClust, data$orangeClust)) 

##raw numbers in each cluster combo by gender
clustComboGender<-table(data$blueClust,data$orangeClust,data$gender)






####confusion matrices
#(rearrange factor levels for well rounded in middle, relabel)

##overall clust combo frequency
#create matrix dataframe
overallMatrix <- data.frame(((table(data$blueClust, data$orangeClust))/sum(table(data$blueClust, data$orangeClust)))*100)
#relabel column names
colnames(overallMatrix) <- c("blueCluster", "orangeCluster", "comboFrequency")
#round all numbers to 2 decimal places
overallMatrix[,3] <-round(overallMatrix[,3],2)

#switch the order of the clusters in the chart (so, hot, then WR, then rich. 1, 2, 3 --> 2, 3, 1)
matrixOrder <- c(2,3,1)
overallMatrix$blueCluster <- factor(overallMatrix$blueCluster, levels = matrixOrder)
overallMatrix$orangeCluster <- factor(overallMatrix$orangeCluster, levels = matrixOrder)

#combine cluster combos so only occur once (e.g., 2,1 = 1,2 & 2,3 = 3,2)
overallMatrix[4,3] <- overallMatrix[2,3]+ overallMatrix[4,3]
overallMatrix[2,3] <- NA

overallMatrix[7,3] <- overallMatrix[3,3] + overallMatrix[7,3]
overallMatrix[3,3] <- NA

overallMatrix[6,3] <- overallMatrix[8,3] + overallMatrix[6,3]
overallMatrix[8,3] <- NA

#plot matrix
overallMatrixPlot <- ggplot(overallMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = overallMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Attractive & Good in Bed','Well-Rounded','Kind & Smart')) +
  scale_y_discrete(labels = c('Attractive & Good in Bed','Well-Rounded','Kind & Smart')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13))

#ggsave("overallMatrix.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



##clust combo freq by gender

#men

#create matrix dataframe
maleMatrix <- data.frame(((table(data$blueClust[data$gender == 1], data$orangeClust[data$gender == 1]))/sum(table(data$blueClust[data$gender == 1], data$orangeClust[data$gender == 1])))*100)
#relabel column names
colnames(maleMatrix) <- c("blueCluster", "orangeCluster", "comboFrequency")
#round all numbers to 2 decimal places
maleMatrix[,3] <-round(maleMatrix[,3],2)

#switch the order of the clusters in the chart (so, hot, then WR, then rich. 1, 2, 3 --> 1, 3, 2)
maleMatrix$blueCluster <- factor(maleMatrix$blueCluster, levels = matrixOrder)
maleMatrix$orangeCluster <- factor(maleMatrix$orangeCluster, levels = matrixOrder)

#combine cluster combos so only occur once (e.g., 2,1 = 1,2 & 2,3 = 3,2)
maleMatrix[4,3] <- maleMatrix[2,3]+ maleMatrix[4,3]
maleMatrix[2,3] <- NA

maleMatrix[7,3] <- maleMatrix[3,3] + maleMatrix[7,3]
maleMatrix[3,3] <- NA

maleMatrix[6,3] <- maleMatrix[8,3] + maleMatrix[6,3]
maleMatrix[8,3] <- NA

#plot matrix
maleMatrixPlot <- ggplot(maleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = maleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Attractive & Good in Bed','Well-Rounded','Kind & Smart')) +
  scale_y_discrete(labels = c('Attractive & Good in Bed','Well-Rounded','Kind & Smart')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13))


#women


#create matrix dataframe
femaleMatrix <- data.frame(((table(data$blueClust[data$gender == 0], data$orangeClust[data$gender == 0]))/sum(table(data$blueClust[data$gender == 0], data$orangeClust[data$gender == 0])))*100)
#relabel column names
colnames(femaleMatrix) <- c("blueCluster", "orangeCluster", "comboFrequency")
#round all numbers to 2 decimal places
femaleMatrix[,3] <-round(femaleMatrix[,3],2)

#switch the order of the clusters in the chart (so, hot, then WR, then rich. 1, 2, 3 --> 1, 3, 2)
femaleMatrix$blueCluster <- factor(femaleMatrix$blueCluster, levels = matrixOrder)
femaleMatrix$orangeCluster <- factor(femaleMatrix$orangeCluster, levels = matrixOrder)

#combine cluster combos so only occur once (e.g., 2,1 = 1,2 & 2,3 = 3,2)
femaleMatrix[4,3] <- femaleMatrix[2,3]+ femaleMatrix[4,3]
femaleMatrix[2,3] <- NA

femaleMatrix[7,3] <- femaleMatrix[3,3] + femaleMatrix[7,3]
femaleMatrix[3,3] <- NA

femaleMatrix[6,3] <- femaleMatrix[8,3] + femaleMatrix[6,3]
femaleMatrix[8,3] <- NA

#plot matrix
femaleMatrixPlot <- ggplot(femaleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = femaleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Attractive & Good in Bed','Well-Rounded','Kind & Smart')) +
  scale_y_discrete(labels = c('Attractive & Good in Bed','Well-Rounded','Kind & Smart')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13))







### Permuation Analysis ###


##create a vector to store null distribution averages
nullDistAvg <- rep(0,100000)

##for loop to generate data of null dist 
for(a in 1:100000){
  
  aNull<-sample(data$blueClust)
  bNull<-sample(data$orangeClust)
  
  #Compute sameordiff
  sodNull<-ifelse(aNull==bNull, 1, 0)
  
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
sameClust <- table(data$blueClust[data$sameOrDiff == 1]) 

##for people who wanted partners in different clusters
#don't need to run separately for clusters A and B bc made order arbitrary
diffClust <- table(data$blueClust[data$sameOrDiff ==0])  


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
logRegModelSex <- glmer(gender ~kFitab + (1|PIN), data= longData, family = "binomial")





### Plotting ###

##plot bar graph with each trait mean for each 3 clusters (# clusters depends on scree)

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
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "darkorchid3")+
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level")  +ylim(0,8) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))+
  ggtitle("(A) Kind and Smart")

#cluster 2 
meanTrait2 <- clustCenters[2,]
trait2 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2 <- data.frame(meanTrait2, trait2)
plot2 <- ggplot(data=plotting2, aes(x=trait2, y=meanTrait2)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "orangered3")+ 
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(0,8) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))+
  ggtitle("(B) Attractive and Good in Bed")

#cluster 3 
meanTrait3 <- clustCenters[3,]
trait3 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting3 <- data.frame(meanTrait3, trait3)
plot3 <- ggplot(data=plotting3, aes(x=trait3, y=meanTrait3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "springgreen4")+ 
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(0,8) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))+
  ggtitle("(C) Well-Rounded")

#combine clusters into one graph
panelPlot<-ggarrange(plot1, plot2, plot3, nrow=1, ncol=3,font.label = list(size = 14, color = "black"))













######Mean-Centered Plots

##graphs with scores centered around 0 (mean centering scores?)

#subtract mean of each row from values in each row
clustCentersC <- apply(clustCenters, 1, function(row) {
  row - mean(row)
})
clustCentersC <- t(clustCentersC)

#cluster 1 (title will change based on clusters)
meanTrait1C <- clustCentersC[1,]
trait1C <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting1C <- data.frame(meanTrait1C, trait1C)
plot1C <- ggplot(data=plotting1C, aes(x=trait1C, y=meanTrait1C)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-4,4) +
  ggtitle("Kind and Smart") +theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2C <- clustCentersC[2,]
trait2C <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2C <- data.frame(meanTrait2C, trait2C)
plot2C <- ggplot(data=plotting2C, aes(x=trait2C, y=meanTrait2C)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-4,4) +
  ggtitle("Attractive and Good in Bed") +theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3C <- clustCentersC[3,]
trait3C <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting3C <- data.frame(meanTrait3C, trait3C)
plot3C <- ggplot(data=plotting3C, aes(x=trait3C, y=meanTrait3C)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-4,4) +
  ggtitle("Well-Rounded") +theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#combine clusters into one graph
panelPlotC<-ggarrange(plot2C,plot1C,plot3C,labels=c("A","B","C"), nrow=1, ncol=3,font.label = list(size = 14, color = "black"))



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

#if blue > orange, give value 1; if blue = orange, give value 0
#if neither is true ( so blue < orange), give -1. 
for(i in 1:nrow(data)){
  if(data$totalBlue[i] > data$totalOrange[i]) {
    data$alloComp[i] <- 1 
  } else {
    if(data$totalBlue[i] == data$totalOrange[i]) {
      data$alloComp[i] <- 0
    } else{
      data$alloComp[i] <- -1 
      }
    }
}


#create dataframe excluding rows where alloComp = 0 (bc we just want to look at unequal distribution for now)
alloData <- data[data$alloComp != 0,]


#create columns with cluster of high allocation partner and cluster of low allocation partner

alloData$highClust <- NA
alloData$lowClust <- NA

for(i in 1:nrow(alloData)){
  if(alloData$alloComp[i] == 1){
    alloData$highClust[i] <- alloData$blueClust[i]
  } else {
    alloData$highClust[i] <- alloData$orangeClust[i]
  }
    
}

for(i in 1:nrow(alloData)){
  if(alloData$alloComp[i] == -1){
    alloData$lowClust[i] <- alloData$blueClust[i]
  } else {
    alloData$lowClust[i] <- alloData$orangeClust[i]
  }
  
}

#run chisq test with high allo x low allo 
chisqAllo <- chisq.test((table(alloData$highClust, alloData$lowClust))) #significant
  
#table (1st variable listed is down side, 2nd is across top of table)
alloTable <- table(alloData$highClust, alloData$lowClust)

  

#run fisher test for allo of only women
fisherAlloF <- fisher.test((table(alloData$highClust[alloData$sex == 0], alloData$lowClust[alloData$sex == 0]))) #significant

alloTableF <- table(alloData$highClust[alloData$sex == 0], alloData$lowClust[alloData$sex == 0])


#run chisq test for allo of only men
fisherAlloM <- fisher.test((table(alloData$highClust[alloData$sex == 1], alloData$lowClust[alloData$sex == 1]))) #significant

alloTableM <- table(alloData$highClust[alloData$sex == 1], alloData$lowClust[alloData$sex == 1])



