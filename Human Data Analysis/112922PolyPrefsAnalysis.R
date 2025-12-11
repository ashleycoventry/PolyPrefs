#############Polyamorous Mate Preferences 2 -- Budget Allocation: Analysis Script #################



###load packages 
library(psych) #for scree plot
library(ggplot2) #For generating other plots
library(ggpubr)
library(data.table) #for reshaping data
library(stringr) #for permutation analy data reorganizing
library(lme4) #for the logistic regression
library(rcompanion) #for the cohenW effect size calculations

###set seed###
set.seed(112822)


###load data###
data <- read.csv("Human Data/Processed Data/PolyPrefs2_ProcessedData20250118 122054.csv")


#Remove NAs ###
#necessary when merging back into the original data frame because kfits is omitting NAs. 

nacheck <- apply(data[,14:27], 1, function(x) sum(is.na(x))>0)
data<- data[!nacheck,]



###reshape data wide --> long and only keep columns relevant for kmeans

#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

longData<-melt(as.data.table(data),id.vars=c("PIN","gender","age"),
               measure.vars=list(c(30,28), #gender
                                 c(14,21), #ambition
                                 c(15,22), #attractiveness
                                 c(16,23), #intelligence
                                 c(17,24), #sexy
                                 c(18,25), #kindness
                                 c(19,26), #status
                                 c(20,27)))#wealth


#Relabel columns

colnames(longData)[4:12]<-c("partner","idealGender","amb","attract",
                            "intel","sexy","kind",
                            "stat","finPros")


#Relabel partner type values
longData$partner<-as.factor(ifelse(longData$partner==1,"idealBlue","idealOrange"))




###K-Means Cluster Analysis###


#extract kmeans wSs
kfitWss<-sapply(1:7,function(x) kmeans(longData[,6:12],x, nstart= 100)$tot.withinss)

#scree plot
screePlot<-qplot(1:7,kfitWss)


##compute differences in within ss across k for k-means clustering
wssDiffs<-diff(kfitWss)


##Add this classification to the original dataframe

kFit<-kmeans(longData[,6:12],2)
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
data$sameOrDiff<-apply(data[,134:135],1,function(x)
  sum(duplicated(x))
) #checking for duplicates (if same cluster = duplicate = 1; if diff clusters = 0)

##get sameOrDiff as function of gender
genderDiff<-table(data$sameOrDiff,data$gender)


##Computing average differentness
avgDiff <- mean(data$sameOrDiff) #same is 1, so closer to one means more same

###create variable listing cluster of each partner 
#this combo order is arbitrary (numerically smaller cluster always listed first)
#arbitrary-ness is necessary for gender, cluster combo chisq later

data$kFitab<-apply(data[,134:135],1,function(x) paste0(sort(as.numeric(x)),collapse=","))




### CHI SQUARE 

##are men and women are choosing combos of partner orange and blue at diff rates?

chisqGender<-chisq.test(table(data$gender,data$kFitab))
chisqGenderW <- cohenW(x = chisqGender$observed, p = chisqGender$expected) #effect size


##do preferences for partner orange predict preferences for partner blue?
chisqClust<-chisq.test(table(data$blueClust, data$orangeClust)) 
chisqClustW <- cohenW(x = chisqClust$observed, p = chisqClust$expected)  #effect size

##raw numbers in each cluster combo by gender
clustComboGender<-table(data$blueClust,data$orangeClust,data$gender)


##Proportion of participants with at least 1 partner in specific clusters 

##Create blank columns in data
data$clust1 <- NA #for cluster 1
data$clust2<- NA #for cluster 2


##for loop to fill columns
for(i in 1:NROW(data)){
  ifelse(data$orangeClust[i] == 1 | data$blueClust[i] == 1, data$clust1[i]<-1, data$clust1[i]<-0)
  
}

for(i in 1:NROW(data)){
  ifelse(data$orangeClust[i] == 2 | data$blueClust[i] == 2, data$clust2[i]<-1, data$clust2[i]<-0)
  
}





chisqClust1 <- chisq.test(table(data$gender, data$clust1))
chisqClust1W <- cohenW( x = chisqClust1$observed, p = chisqClust1$expected) #effSize

chisqClust2 <- chisq.test(table(data$gender, data$clust2))
chisqClust2W <- cohenW( x = chisqClust2$observed, p = chisqClust2$expected) #effect size




### Permuation Analysis ###


##create a vector to store null distribution averages
nullDistAvg <- rep(0,100000)

##for loop to generate data of null dist 
#creating a null distribution by randomly shuffling cluster assignments
for(a in 1:100000){
  
  aNull<-sample(data$blueClust)
  bNull<-sample(data$orangeClust)
  
  #Compute sameordiff (whether cluster A and cluster B are the same or different)
  sodNull<-ifelse(aNull==bNull, 1, 0)
  
  #Compute and saving average differentness
  nullDistAvg[a]<-mean(sodNull)
  
}



##see whether we are in extremes of null dist (compare output to our avgdiff)
#these are confidence intervals

nullDistHigh<-quantile(nullDistAvg[1:100000],c(0.975)) 
nullDistLow<-quantile(nullDistAvg[1:100000],c(0.025)) 


###How many ideal partners are in each cluster?

##for people who wanted both partners in same cluster
sameClust <- table(data$blueClust[data$sameOrDiff == 1]) 

##for people who wanted partners in different clusters
#don't need to run separately for clusters A and B bc made order arbitrary
diffClust <- table(data$blueClust[data$sameOrDiff ==0])  



###Mate Point Allocation###

#question: are people who want partners in different clusters allocating a sig different # of points to each partner?

##calculate total # of points allocated per row for traits

# Compute total points allocated to each partner
data$totalOrange <- rowSums(data[, 21:27], na.rm = TRUE)
data$totalBlue <- rowSums(data[, 14:20], na.rm = TRUE)

##compare between partner blue and orange for each participant
#if blue > orange, give value 1; if blue = orange, give value 0
#if neither is true ( so blue < orange), give -1. 
data$alloComp <- sign(data$totalBlue - data$totalOrange)

#create dataframe excluding rows where alloComp = 0 
#(bc we just want to look at unequal distribution for now)
alloData <- subset(data, alloComp != 0)

#create columns with cluster of high allocation partner and cluster of low allocation partner

alloData$highClust <- ifelse(alloData$alloComp == 1, alloData$blueClust, alloData$orangeClust)
alloData$lowClust <- ifelse(alloData$alloComp == -1, alloData$blueClust, alloData$orangeClust)

#run chisq test with high allo x low allo 
chisqAllo <- chisq.test(table(alloData$highClust, alloData$lowClust))

#table (1st variable listed is down side, 2nd is across top of table)
alloTable <- table(alloData$highClust, alloData$lowClust)



#run fisher test for allo of only women
fisherAlloF <- fisher.test((table(alloData$highClust[alloData$sex == 0], alloData$lowClust[alloData$sex == 0]))) 

alloTableF <- table(alloData$highClust[alloData$sex == 0], alloData$lowClust[alloData$sex == 0])


#run chisq test for allo of only men
fisherAlloM <- fisher.test((table(alloData$highClust[alloData$sex == 1], alloData$lowClust[alloData$sex == 1]))) 

alloTableM <- table(alloData$highClust[alloData$sex == 1], alloData$lowClust[alloData$sex == 1])

###save data for comparison with poly sample from later study
#write.csv(data, "Human Data/Processed Data/PP2AnalyzedData.csv", row.names = FALSE)



### Plotting ###

##plot bar graph with each trait mean for each 3 clusters (# clusters depends on scree)

meanTrait <- c(clustCenters[1,], clustCenters[2,])
mateType <-c(rep("1", 7), rep("2", 7))
trait <- c(rep(c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources"), 2))  
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
  theme_minimal(base_size = 10) + xlab("Trait") + ylab("Average Desired Trait Level")  +ylim(0,8) +
  theme(plot.title = element_text(size = 10), axis.text.x = element_text(angle = 90))+
  ggtitle("Well-Rounded")


#cluster 2 
meanTrait2 <- clustCenters[2,]
trait2 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2 <- data.frame(meanTrait2, trait2)
plot2 <- ggplot(data=plotting2, aes(x=trait2, y=meanTrait2)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "orangered3")+ 
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 10) + xlab("Trait") + ylab("Average Desired Trait Level") +ylim(0,8) +
  theme(plot.title = element_text(size = 10), axis.text.x = element_text(angle = 90))+
  ggtitle("Good in Bed & Attractive")

#combine clusters into one graph
panelPlot<-ggarrange(plot1, plot2, nrow=1, ncol=2,font.label = list(size = 10, color = "black"))





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
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-4,4)+
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2C <- clustCentersC[2,]
trait2C <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2C <- data.frame(meanTrait2C, trait2C)
plot2C <- ggplot(data=plotting2C, aes(x=trait2C, y=meanTrait2C)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-4,4)+
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#combine clusters into one graph
panelPlotC<-ggarrange(plot1C,plot2C,labels=c("A","B"), nrow=1, ncol=3,font.label = list(size = 14, color = "black"))



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
matrixOrder <- c(1,2)
overallMatrix$blueCluster <- factor(overallMatrix$blueCluster, levels = matrixOrder)
overallMatrix$orangeCluster <- factor(overallMatrix$orangeCluster, levels = matrixOrder)

#combine cluster combos so only occur once (e.g., 2,1 = 1,2 & 2,3 = 3,2)
overallMatrix[2,3] <- overallMatrix[2,3]+ overallMatrix[3,3]
overallMatrix[3,3] <- NA


#plot matrix
overallMatrixPlot <- ggplot(overallMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = overallMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Well-Rounded','Good in Bed \n & Attractive')) +
  scale_y_discrete(labels = c('Well-Rounded','Good in Bed \n & Attractive')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13))

#ggsave("overallMatrix.jpeg", plot=last_plot(), width=150, height=100, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



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
maleMatrix[2,3] <- maleMatrix[2,3]+ maleMatrix[3,3]
maleMatrix[3,3] <- NA


#plot matrix
maleMatrixPlot <- ggplot(maleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = maleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Well-Rounded','Good in Bed \n & Attractive')) +
  scale_y_discrete(labels = c('Well-Rounded','Good in Bed \n & Attractive')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13))+
  ggtitle("(B) Male Participants")


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
femaleMatrix[2,3] <- femaleMatrix[2,3]+ femaleMatrix[3,3]
femaleMatrix[3,3] <- NA


#plot matrix
femaleMatrixPlot <- ggplot(femaleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = femaleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Well-Rounded','Good in Bed \n & Attractive')) +
  scale_y_discrete(labels = c('Well-Rounded','Good in Bed \n & Attractive')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13))+
  ggtitle("(A) Female Participants")


#panel plot of both of these graphs
MatrixPlotPanel <- ggarrange(femaleMatrixPlot, maleMatrixPlot, nrow=1, ncol=2, 
                             common.legend = TRUE, legend = "right")
#ggsave("PP2MatrixPlotPanel.jpeg", plot=last_plot(), width=275, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)






#####Alternative cluster analysis

##make data wide (1 row per participant)
wideData <- data[,c(103, 5, 14:28, 30)]



#Cluster Analysis


#extract kmeans wSs
kfitWssAlt<-sapply(1:7,function(x) kmeans(wideData[,3:16],x, nstart= 100)$tot.withinss)

#scree plot
screePlotAlt<-qplot(1:7,kfitWssAlt)


##compute differences in within ss across k for k-means clustering
wssDiffsAlt<-diff(kfitWssAlt)


##Add this classification to the original dataframe

kFitAlt<-kmeans(wideData[,3:16],2)
wideData$kFitAlt <- kFitAlt$cluster


##Create vectors of preference means for each cluster (without age)
clustCentersAlt<-kFitAlt$centers

##Look at gender breakdown by cluster
clustGenderAlt<-table(wideData$gender,wideData$kFitAlt)

##compute variance between trait ratings for each cluster
#to see if maybe one cluster is more well rounded than others
clustVarsAlt<-apply(clustCentersAlt,1,var)


##multipanel figure
#first create individual plot for each cluster

#cluster 1 (title will change based on clusters)
meanTraitAlt1 <- clustCentersAlt[1,]
traitAlt1 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
partner <- c(rep("Blue", 7), rep("Orange", 7))
plottingAlt1 <- data.frame(meanTraitAlt1, traitAlt1, partner)
plotAlt1 <- ggplot(data=plottingAlt1, aes(x=traitAlt1, y=meanTraitAlt1, fill = partner)) +
  geom_bar(stat="identity", color="black", position=position_dodge(width = .9))+
  scale_fill_manual(values = c("Blue" = "lightblue", "Orange" = "#FFC067")) +
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 10) + xlab("Trait") + ylab("Average Desired Trait Level")  +ylim(0,8) +
  theme(plot.title = element_text(size = 10), axis.text.x = element_text(angle = 90))+
  ggtitle("Cluster 1")


#cluster 2 
meanTraitAlt2 <- clustCentersAlt[2,]
traitAlt2 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
partner <- c(rep("Blue", 7), rep("Orange", 7))
plottingAlt2 <- data.frame(meanTraitAlt2, traitAlt2, partner)
plotAlt2 <- ggplot(data=plottingAlt2, aes(x=traitAlt2, y=meanTraitAlt2, fill = partner)) +
  geom_bar(stat="identity", color="black", position=position_dodge(width = .9))+
  scale_fill_manual(values = c("Blue" = "lightblue", "Orange" = "#FFC067")) +
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 10) + xlab("Trait") + ylab("Average Desired Trait Level")  +ylim(0,8) +
  theme(plot.title = element_text(size = 10), axis.text.x = element_text(angle = 90))+
  ggtitle("Cluster 2")

#combine clusters into one graph
panelPlot<-ggarrange(plot1, plot2, nrow=1, ncol=2,font.label = list(size = 10, color = "black"))



#alternative graphing
#cluster 1 
meanTraitAlt1Blue <- clustCentersAlt[1,1:7]
traitAlt1Blue <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plottingAlt1Blue <- data.frame(meanTraitAlt1Blue, traitAlt1Blue)
plotAlt1Blue <- ggplot(data=plottingAlt1Blue, aes(x=traitAlt1Blue, y=meanTraitAlt1Blue)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "lightblue")+
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 12) + xlab("Trait") + ylab("Average Desired Trait Level")  +ylim(0,8) +
  theme(plot.title = element_text(size = 12), axis.text.x = element_text(angle = 90, size = 12), axis.text.y = element_text(size = 12))+
  ggtitle("Cluster 1 Blue")

meanTraitAlt1Orange <- clustCentersAlt[1,8:14]
traitAlt1Orange <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plottingAlt1Orange <- data.frame(meanTraitAlt1Orange, traitAlt1Orange)
plotAlt1Orange <- ggplot(data=plottingAlt1Orange, aes(x=traitAlt1Orange, y=meanTraitAlt1Orange)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "#FFC067")+
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 12) + xlab("Trait") + ylab("Average Desired Trait Level")  +ylim(0,8) +
  theme(plot.title = element_text(size = 12), axis.text.x = element_text(angle = 90, size = 12), axis.text.y = element_text(size = 12))+
  ggtitle("Cluster 1 Orange")


#combine clusters into one graph
panelPlotAltCluster1<-ggarrange(plotAlt1Blue, plotAlt1Orange, nrow=1, ncol=2,font.label = list(size = 10, color = "black"))


#cluster 2
meanTraitAlt2Blue <- clustCentersAlt[2,1:7]
traitAlt2Blue <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plottingAlt2Blue <- data.frame(meanTraitAlt2Blue, traitAlt2Blue)
plotAlt2Blue <- ggplot(data=plottingAlt2Blue, aes(x=traitAlt2Blue, y=meanTraitAlt2Blue)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "lightblue")+
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 12) + xlab("Trait") + ylab("Average Desired Trait Level")  +ylim(0,8) +
  theme(plot.title = element_text(size = 12), axis.text.x = element_text(angle = 90, size = 12), axis.text.y = element_text(size = 12))+
  ggtitle("Cluster 2 Blue")

meanTraitAlt2Orange <- clustCentersAlt[2,8:14]
traitAlt2Orange <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plottingAlt2Orange <- data.frame(meanTraitAlt2Orange, traitAlt2Orange)
plotAlt2Orange <- ggplot(data=plottingAlt2Orange, aes(x=traitAlt2Orange, y=meanTraitAlt2Orange)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "#FFC067")+
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 12) + xlab("Trait") + ylab("Average Desired Trait Level")  +ylim(0,8) +
  theme(plot.title = element_text(size = 12), axis.text.x = element_text(angle = 90, size = 12), axis.text.y = element_text(size = 12))+
  ggtitle("Cluster 2 Orange")


#combine clusters into one graph
panelPlotAltClustersSeparated<-ggarrange(plotAlt1Blue, plotAlt1Orange, plotAlt2Blue, plotAlt2Orange, nrow=2, ncol=2,font.label = list(size = 12, color = "black"))

#ggsave("PP2MatrixPlotPanelAltAnalysis.jpeg", plot=last_plot(), width=275, height=275, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)

