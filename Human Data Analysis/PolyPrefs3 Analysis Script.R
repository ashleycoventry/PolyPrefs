#############Polyamorous Mate Preferences 3 -- Budget Allocation: Analysis Script #################
####Ashley J Coventry, Tamsin German, Dan Conroy-Beam########



###load packages 
library(psych) #for scree plot
library(ggplot2) #For generating other plots
library(ggpubr)
library(data.table) #for reshaping data
library(stringr) #for permutation analy data reorganizing
library(lme4) #for the logistic regression

###set seed###
set.seed(040623)


###load data###
data <- read.csv("/Users/ashle/Desktop/Research/Polyamory Research/PolyPrefs.nosync/Human Data/Processed Data/PolyPrefs3 Processed Data20230404 222801.csv")


#Remove NAs ###
#necessary when merging back into the original data frame because kfits is omitting NAs. 

nacheck <- apply(data[,14:27], 1, function(x) sum(is.na(x))>0)
data<- data[!nacheck,]


#excluding people who don't identify as either men or women

data<-data[data$gender<2,]
data <- data[data$orange_gender <2,]
data <- data[data$blue_gender <2,]


###reshape data wide --> long and only keep columns relevant for kmeans

#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

longData<-melt(as.data.table(data),id.vars=c("PIN","gender","age"),
               measure.vars=list(c(31,29), #gender (first listed = blue, second = orange)
                                 c(14,21), #ambition
                                 c(15,22), #attractiveness
                                 c(16,23), #intelligence
                                 c(17,24), #good in bed
                                 c(18,25), #kindness
                                 c(19,26), #status
                                 c(20,27))) #wealth


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
data$sameOrDiff<-apply(data[,143:144],1,function(x)
  sum(duplicated(x))
)

##get sameOrDiff as function of gender
genderDiff<-table(data$sameOrDiff,data$gender)


##Computing average differentness
avgDiff <- mean(data$sameOrDiff)

###create variable listing cluster of each partner 

data$kFitab<-apply(data[,143:144],1,function(x) paste0(sort(as.numeric(x)),collapse=","))


### CHI SQUARE 

#are men and women are choosing combos of partner orange and blue at diff rates?

fisherGender <- fisher.test(table(data$gender, data$kFitab), simulate.p.value = TRUE)
  #2,2 for men has only 2 people so too small for chi sq
  #workplace size issue 


##do preferences for partner orange predict preferences for partner blue?
chisqClust<-chisq.test(table(data$blueClust, data$orangeClust)) #yes

##raw numbers in each cluster combo by gender
clustComboGender<-table(data$blueClust,data$orangeClust,data$gender)


###confusion matrices
#(rearrange factor levels for well rounded in middle, relabel)

##overall clust combo frequency
#create matrix dataframe
overallMatrix <- data.frame(((table(data$blueClust, data$orangeClust))/sum(table(data$blueClust, data$orangeClust)))*100)
#relabel column names
colnames(overallMatrix) <- c("blueCluster", "orangeCluster", "comboFrequency")
#round all numbers to 2 decimal places
overallMatrix[,3] <-round(overallMatrix[,3],2) #the "-1" excludes column 1

#switch the order of the clusters in the chart (so, hot, then WR, then rich. 1, 2, 3 --> 1, 3, 2)
matrixOrder <- c(1, 3, 2)
overallMatrix$blueCluster <- factor(overallMatrix$blueCluster, levels = matrixOrder)
overallMatrix$orangeCluster <- factor(overallMatrix$orangeCluster, levels = matrixOrder)

#combine cluster combos so only occur once (e.g., 2,1 = 1,2 & 2,3 = 3,2)
overallMatrix[2,3] <- overallMatrix[2,3]+ overallMatrix[4,3]
overallMatrix[4,3] <- NA

overallMatrix[3,3] <- overallMatrix[3,3] + overallMatrix[7,3]
overallMatrix[7,3] <- NA

overallMatrix[8,3] <- overallMatrix[8,3] + overallMatrix[6,3]
overallMatrix[6,3] <- NA

#plot matrix
overallMatrixPlot <- ggplot(overallMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = overallMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Good in Bed & Attractive','Well-Rounded','Wealthy & Kind')) +
  scale_y_discrete(labels = c('Good in Bed & Attractive','Well-Rounded','Wealthy & Kind')) +
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
maleMatrix[2,3] <- maleMatrix[2,3]+ maleMatrix[4,3]
maleMatrix[4,3] <- NA

maleMatrix[3,3] <- maleMatrix[3,3] + maleMatrix[7,3]
maleMatrix[7,3] <- NA

maleMatrix[8,3] <- maleMatrix[8,3] + maleMatrix[6,3]
maleMatrix[6,3] <- NA

#plot matrix
maleMatrixPlot <- ggplot(maleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = maleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Good in Bed & Attractive','Well-Rounded','Wealthy & Kind')) +
  scale_y_discrete(labels = c('Good in Bed & Attractive','Well-Rounded','Wealthy & Kind')) +
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
femaleMatrix[2,3] <- femaleMatrix[2,3]+ femaleMatrix[4,3]
femaleMatrix[4,3] <- NA

femaleMatrix[3,3] <- femaleMatrix[3,3] + femaleMatrix[7,3]
femaleMatrix[7,3] <- NA

femaleMatrix[8,3] <- femaleMatrix[8,3] + femaleMatrix[6,3]
femaleMatrix[6,3] <- NA

#plot matrix
femaleMatrixPlot <- ggplot(femaleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = femaleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Good in Bed & Attractive','Well-Rounded','Wealthy & Kind')) +
  scale_y_discrete(labels = c('Good in Bed & Attractive','Well-Rounded','Wealthy & Kind')) +
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
  sodNull<-ifelse(aNull==bNull, 0, 1)
  
  #Compute and saving average differentness
  nullDistAvg[a]<-mean(sodNull)
  
}



##see whether we are in extremes of null dist (compare output to our avgdiff)

nullDistHigh<-quantile(nullDistAvg[1:100000],c(0.975)) 
nullDistLow<-quantile(nullDistAvg[1:100000],c(0.025)) 


#Compute p-value comparing observed sameordiff to null distribution
pValueDiff <- sum(nullDistAvg < avgDiff) /100000 #more diff than chance? 


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



chisqClust1 <- chisq.test(table(data$gender, data$clust1)) #sig
chisqClust2 <- chisq.test(table(data$gender, data$clust2)) #sig
chisqClust3 <- chisq.test(table(data$gender, data$clust3)) #not sig

###Ideal gender analysis


#partner blue

chisqIdealGenderBlue <- chisq.test(table(longData$idealGender[longData$partner == "idealBlue"], longData$kFitab[longData$partner == "idealBlue"])) 
idealGenderClustBlue <- table(longData$idealGender[longData$partner == "idealBlue"], longData$kFitab[longData$partner == "idealBlue"])

#partner orange
chisqIdealGenderOrange <- chisq.test(table(longData$idealGender[longData$partner == "idealOrange"], longData$kFitab[longData$partner == "idealOrange"])) 
idealGenderClustOrange <- table(longData$idealGender[longData$partner == "idealOrange"], longData$kFitab[longData$partner == "idealOrange"])




##logistic regression
#predicting partner sex from cluster?


logRegModelPsex <- glmer(idealGender ~ kFitab + (1|PIN), data = longData, family = "binomial") 

logRegModelSex <- glmer(gender ~kFitab + (1|PIN), data= longData, family = "binomial")


###Investment Questions Analysis###

##Q1: is there a greater deviation from the center (equal invest.) if 2 partners are in diff clusters

##make long dataframe with investment info
#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

investData<-melt(as.data.table(data),id.vars=c("PIN", "gender", "fin_invest","time_invest","emot_close", "sameOrDiff"), #sameOrDiff: same = 1, diff = 0
               measure.vars=list(c(143,144))) #clust (first listed = blue, second = orange)

#renaming columns
colnames(investData) <- c("PIN", "gender", "finInvest", "timeInvest", "emotClose", "sameOrDiff", "partner", "cluster")


##calculating deviation from equal invest
investData <- investData %>% 
  mutate(finDeviation = abs(finInvest - 4),
         timeDeviation = abs(timeInvest - 4),
         emotDeviation = abs(emotClose - 4))


#look at deviation descriptively
summary(investData$finDeviation)
table(investData$finDeviation)
table(investData[investData$sameOrDiff == 1]$finDeviation)
table(investData[investData$sameOrDiff == 0]$finDeviation) #more deviation if clusters are different

summary(investData$timeDeviation)
table(investData$timeDeviation)
table(investData[investData$sameOrDiff == 1]$timeDeviation)
table(investData[investData$sameOrDiff == 0]$timeDeviation) #more deviation if clusters are different

summary(investData$emotDeviation)
table(investData$emotDeviation)
table(investData[investData$sameOrDiff == 1]$emotDeviation)
table(investData[investData$sameOrDiff == 0]$emotDeviation) #more deviation if clusters are different


##anova comparing deviation for same vs diff clusters
investData$sameOrDiff <- as.factor(investData$sameOrDiff)
investData$gender <- as.factor(investData$gender)

finInvestAnova <- aov(finDeviation ~ sameOrDiff + gender, data = investData)
finInvestAnovaInt <- aov(finDeviation ~ sameOrDiff*gender, data = investData)

finHSD <- TukeyHSD(finInvestAnovaInt)

timeInvestAnova <- aov(timeDeviation ~ sameOrDiff + gender, data = investData)
timeInvestAnovaInt <- aov(timeDeviation ~ sameOrDiff*gender, data = investData)

timeHSD <- TukeyHSD(timeInvestAnovaInt)

emotCloseAnova <- aov(emotDeviation ~ sameOrDiff + gender, data = investData)
emotCloseAnovaInt <- aov(emotDeviation ~ sameOrDiff*gender, data = investData)




##Q2: does this investment vary by cluster and also cluster of other partner?
data$blueClust <- as.factor(data$blueClust)
data$orangeClust <- as.factor(data$orangeClust)

##fin invest variance based on cluster type

finInvestClustInt <- aov(fin_invest ~ blueClust*orangeClust, data = data) #no sig int
finInvestClustMain <- aov(fin_invest ~ blueClust+orangeClust, data = data)

tukeyFinInvestClust <- TukeyHSD(finInvestClustMain)


#time invest variance based on cluster type
timeInvestClustInt <- aov(time_invest ~ blueClust*orangeClust, data = data) #no sig int
timeInvestClustMain <- aov(time_invest ~ blueClust+orangeClust, data = data) 

tukeyTimeInvestClust <- TukeyHSD(timeInvestClustMain)

#emotional closeness variance based on cluster type
emotCloseClustInt <- aov(emot_close ~ blueClust*orangeClust, data = data) #sig interaction

tukeyEmotCloseClust <- TukeyHSD(emotCloseClustInt)









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
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "orangered3") +
  geom_hline(yintercept = mean(meanTrait1), color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level")  +ylim(0,8) +
  theme(axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2 <- clustCenters[2,]
trait2 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2 <- data.frame(meanTrait2, trait2)
plot2 <- ggplot(data=plotting2, aes(x=trait2, y=meanTrait2)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "darkorchid3")+ 
  geom_hline(yintercept = mean(meanTrait2), color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(0,8) +
  theme(axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3 <- clustCenters[3,]
trait3 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting3 <- data.frame(meanTrait3, trait3)
plot3 <- ggplot(data=plotting3, aes(x=trait3, y=meanTrait3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "springgreen4")+ 
  geom_hline(yintercept = mean(meanTrait3), color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(0,8) +
  theme(axis.text.x = element_text(angle = 90))

#combine clusters into one graph
panelPlot<-ggarrange(plot1, plot2, plot3, nrow=1, ncol=3,font.label = list(size = 14, color = "black"))






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
chisqAllo <- chisq.test((table(alloData$highClust, alloData$lowClust))) #not sig

#table (1st variable listed is down side, 2nd is across top of table)
alloTable <- table(alloData$highClust, alloData$lowClust)



#run fisher test for allo of only women
fisherAlloF <- fisher.test((table(alloData$highClust[alloData$sex == 0], alloData$lowClust[alloData$sex == 0])))

alloTableF <- table(alloData$highClust[alloData$sex == 0], alloData$lowClust[alloData$sex == 0])


#run chisq test for allo of only men
fisherAlloM <- fisher.test((table(alloData$highClust[alloData$sex == 1], alloData$lowClust[alloData$sex == 1]))) 

alloTableM <- table(alloData$highClust[alloData$sex == 1], alloData$lowClust[alloData$sex == 1])

