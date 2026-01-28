######Polyamorous Mate Preferences 4#####

###packages
library(data.table) #for reshaping data
library(ggplot2) #for scree plot + visualizing data
library(rcompanion) #for cohenW test
library(ggpubr) #for ggarrange
library(ordinal) #for ordinal regression
library(ggeffects)
library(MetBrewer)
###set seed###
set.seed(010423)


data <- read.csv("Human Data/Processed Data/PolyPrefs4_ProcessedData20250120 135914.csv")


####Ideal Partner Cluster Analysis

##Remove NAs from budget allo items##
#necessary for cluster analysis because kfits is omitting NAs. 

nacheck <- apply(data[,15:28], 1, function(x) sum(is.na(x))>0)
data<- data[!nacheck,] 


#excluding people who don't identify as either men or women

data<-data[data$gender<2,]


###reshape data wide --> long and only keep columns relevant for kmeans

#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

longData<-data.table::melt(as.data.table(data),id.vars=c("PIN","gender","age"),
               measure.vars=list(c(32,30), #gender (first listed = blue, second = orange)
                                 c(15,22), #ambition
                                 c(16,23), #attractiveness
                                 c(17,24), #intelligence
                                 c(18,25), #good in bed
                                 c(19,26), #kindness
                                 c(20,27), #status
                                 c(21,28))) #wealth
#Relabel columns
colnames(longData)[4:12]<-c("partner","idealGender","amb","attract",
                            "intel","sexy","kind",
                            "stat","finPros")


#Relabel partner type values (1 = blue, 2 = orange)
longData$partner<-as.factor(ifelse(longData$partner==1,"idealBlue","idealOrange"))



###K-Means Cluster Analysis###

#extract kmeans wss
kfitWss<-sapply(1:7,function(x) kmeans(longData[,6:12],x, nstart = 100,)$tot.withinss)

#scree plot
screePlot<-qplot(1:7,kfitWss)

##compute differences in within ss across k for k-means clustering
wssDiffs<-diff(kfitWss)


##Add this classification (clusters) to the original dataframe

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
data$sameOrDiff<-apply(data[,171:172],1,function(x)
  sum(duplicated(x))
)

##get sameOrDiff as function of gender
genderDiff<-table(data$sameOrDiff,data$gender)


##Computing average differentness
avgDiff <- mean(data$sameOrDiff) 


###create variable listing cluster of each partner 

data$kFitab<-apply(data[,171:172],1,function(x) paste0(sort(as.numeric(x)),collapse=","))



### Chisq
#are men and women are choosing combos of partner orange and blue at diff rates?

chisqGender <- chisq.test(table(data$gender, data$kFitab))

##do preferences for partner orange predict preferences for partner blue?
chisqClust<-chisq.test(table(data$blueClust, data$orangeClust)) 
chisqClustW <- cohenW( x = chisqClust$observed, p = chisqClust$expected) #effect size


##raw numbers in each cluster combo by gender
clustComboGender<-table(data$blueClust,data$orangeClust,data$gender)


###More Chi Squares: proportion of participants with at least 1 partner in specific clusters 
#is one gender more likely than the other to want at least 1 partner in a given cluster?

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
chisqClust2 <- chisq.test(table(data$gender, data$clust2)) 





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







###How many ideal partners are in each cluster?

##for people who wanted both partners in same cluster
sameClust <- table(data$blueClust[data$sameOrDiff == 1]) 

##for people who wanted partners in different clusters
#don't need to run separately for clusters A and B bc made order arbitrary
diffClust <- table(data$blueClust[data$sameOrDiff ==0])  







####Figures

###confusion matrices

##overall clust combo frequency
#create matrix dataframe
overallMatrix <- data.frame(((table(data$blueClust, data$orangeClust))/sum(table(data$blueClust, data$orangeClust)))*100)
#relabel column names
colnames(overallMatrix) <- c("blueCluster", "orangeCluster", "comboFrequency")
#round all numbers to 2 decimal places
overallMatrix[,3] <-round(overallMatrix[,3],2) 

#switch the order of the clusters in the chart (so, kind & GiB, then WR)
matrixOrder <- c(2, 1)
overallMatrix$blueCluster <- factor(overallMatrix$blueCluster, levels = matrixOrder)
overallMatrix$orangeCluster <- factor(overallMatrix$orangeCluster, levels = matrixOrder)

#combine cluster combos so only occur once (e.g., 2,1 = 1,2 & 2,3 = 3,2)
overallMatrix[3,3] <- overallMatrix[2,3]+ overallMatrix[3,3]
overallMatrix[2,3] <- NA


#plot matrix
overallMatrixPlot <- ggplot(overallMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = overallMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Well-Rounded','Kind \n& Good in Bed')) +
  scale_y_discrete(labels = c('Well-Rounded','Kind \n& Good in Bed')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13)) #warning but just bc we intentionally have empty cells

#ggsave("PP4overallMatrix.jpeg", plot=last_plot(), width=150, height=100, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)

##clust combo freq by gender

#men

#create matrix dataframe
maleMatrix <- data.frame(((table(data$blueClust[data$gender == 1], data$orangeClust[data$gender == 1]))/sum(table(data$blueClust[data$gender == 1], data$orangeClust[data$gender == 1])))*100)
#relabel column names
colnames(maleMatrix) <- c("blueCluster", "orangeCluster", "comboFrequency")
#round all numbers to 2 decimal places
maleMatrix[,3] <-round(maleMatrix[,3],2)

#switch the order of the clusters in the chart
maleMatrix$blueCluster <- factor(maleMatrix$blueCluster, levels = matrixOrder)
maleMatrix$orangeCluster <- factor(maleMatrix$orangeCluster, levels = matrixOrder)

#combine cluster combos so only occur once (e.g., 2,1 = 1,2 & 2,3 = 3,2)
maleMatrix[3,3] <- maleMatrix[2,3]+ maleMatrix[3,3]
maleMatrix[2,3] <- NA

#plot matrix
maleMatrixPlot <- ggplot(maleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = maleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Well-Rounded','Kind & Good in Bed')) +
  scale_y_discrete(labels = c('Well-Rounded','Kind & Good in Bed')) +
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

#switch the order of the clusters in the chart 
femaleMatrix$blueCluster <- factor(femaleMatrix$blueCluster, levels = matrixOrder)
femaleMatrix$orangeCluster <- factor(femaleMatrix$orangeCluster, levels = matrixOrder)

#combine cluster combos so only occur once (e.g., 2,1 = 1,2 & 2,3 = 3,2)
femaleMatrix[3,3] <- femaleMatrix[2,3]+ femaleMatrix[3,3]
femaleMatrix[2,3] <- NA


#plot matrix
femaleMatrixPlot <- ggplot(femaleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = femaleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Well-Rounded','Kind & Good in Bed')) +
  scale_y_discrete(labels = c('Well-Rounded','Kind & Good in Bed')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13)) +
  ggtitle("(A) Female Participants")

#panel plot of both of these graphs
MatrixPlotPanel <- ggarrange(femaleMatrixPlot, maleMatrixPlot, nrow=1, ncol=2, 
                             common.legend = TRUE, legend = "right")
#ggsave("PP4MatrixPlotPanel.jpeg", plot=last_plot(), width=275, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)


###Plotting clusters
##multipanel figure
#first create individual plot for each cluster

#cluster 1 
meanTrait1 <- clustCenters[1,]
trait1 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting1 <- data.frame(meanTrait1, trait1)
plot1 <- ggplot(data=plotting1, aes(x=trait1, y=meanTrait1)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "orangered3") +
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 10) + xlab("Trait") + ylab("Average Desired Trait Level")  +ylim(0,8) +
  theme(plot.title = element_text(size = 10), axis.text.x = element_text(angle = 90))+
  ggtitle("Kind & Good in Bed")

#cluster 2 
meanTrait2 <- clustCenters[2,]
trait2 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2 <- data.frame(meanTrait2, trait2)
plot2 <- ggplot(data=plotting2, aes(x=trait2, y=meanTrait2)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "springgreen4")+ 
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 10) + xlab("Trait") + ylab("Average Desired Trait Level") +ylim(0,8) +
  theme(plot.title = element_text(size = 10),axis.text.x = element_text(angle = 90)) +
  ggtitle("Well-Rounded")

#combine clusters into one graph
panelPlot<-ggarrange(plot2, plot1, nrow=1, ncol=2,font.label = list(size = 10, color = "black"))

#ggsave("PP4panelPlot.jpeg", plot=last_plot(), width=150, height=125, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



###Investment Questions Analysis###

##Q1: is there a greater deviation from the center (equal invest.) if 2 partners are in diff clusters

##make long dataframe with investment info
#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

investData<-data.table::melt(as.data.table(data),id.vars=c("PIN", "gender", "fin_invest","time_invest","emot_close", "sameOrDiff"), #sameOrDiff: same = 1, diff = 0
                 measure.vars=list(c(171,172))) #clust (first listed = blue, second = orange)

#renaming columns
colnames(investData) <- c("PIN", "gender", "finInvest", "timeInvest", "emotClose", "sameOrDiff", "partner", "cluster")


##calculating deviation from equal invest
investData <- investData %>% 
  mutate(finDeviation = abs(finInvest - 4),
         timeDeviation = abs(timeInvest - 4),
         emotDeviation = abs(emotClose - 4))


##ordinal regressions comparing deviation for same vs diff clusters
investData$sameOrDiff <- as.factor(investData$sameOrDiff)
investData$gender <- as.factor(investData$gender)

#get rid of missing deviation data
nacheckInvest <- apply(investData[,9:11], 1, function(x) sum(is.na(x))>0)
investData<- investData[!nacheckInvest,]


##financial investment

investData$finDeviationOrd <- ordered(investData$finDeviation)
finDeviationOR <- clm(finDeviationOrd ~ sameOrDiff+gender, 
                 data = investData)
#test proportional odds assumption
oddsAssumptionCheckFin <- nominal_test(finDeviationOR) 



#for time and emotion, need to dichotomize deviation 
#model without it dichotomized violated proportional odds assumption 
investData$timeDevDich <- ifelse(investData$timeDeviation == 0, 0, 1)
investData$emotDevDich <- ifelse(investData$emotDeviation == 0, 0, 1)

##time investment
investData$timeDeviationOrd <- ordered(investData$timeDevDich)
timeDeviationOR <- clm(timeDeviationOrd ~ sameOrDiff+gender, data = investData)
#test proportional odds assumption
oddsAssumptionCheckTime <- nominal_test(timeDeviationOR) 


#plot predicted probabilities
ORPlotProbsTime <- ggpredict(timeDeviationOR, terms = c("sameOrDiff", "gender"))
ORPlotProbsTime$response.level <- factor(ORPlotProbsTime$response.level,
                                        levels = c(1, 2), labels = c("Equal", "Unequal"))

timeDeviationPlot <- ggplot(ORPlotProbsTime, aes(x = x, y = predicted, fill = response.level))+
  geom_col(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = .9), width = .2) +
  facet_wrap(~ group, labeller = as_labeller(c("0" = "Women", "1" = "Men"))) +
  scale_fill_met_d(name = "Egypt") +
  labs(x = "Different or Same Clusters", y = "Predicted Probability", fill = "Deviation from\nEqual Investment")+
  scale_x_discrete(labels = c("Different\nClusters", "Same\nCluster")) +
  theme_classic(base_size = 14)
#ggsave("PP4TimeDeviationPlot.jpeg", plot = last_plot(), width = 200, height = 150, units = "mm", path = "/Users/ashle/Desktop",scale = 1, dpi = 300, limitsize = TRUE)



##emotional closeness
investData$emotDeviationOrd <- ordered(investData$emotDevDich)
emotDeviationOR <- clm(emotDeviationOrd ~ sameOrDiff+gender, data = investData)
#test proportional odds assumption
oddsAssumptionCheckEmot <- nominal_test(emotDeviationOR) 

#plot predicted probabilities
ORPlotProbsEmot <- ggpredict(emotDeviationOR, terms = c("sameOrDiff", "gender"))
ORPlotProbsEmot$response.level <- factor(ORPlotProbsEmot$response.level,
                                         levels = c(1, 2), labels = c("Equal", "Unequal"))

emotDeviationPlot <- ggplot(ORPlotProbsEmot, aes(x = x, y = predicted, fill = response.level))+
  geom_col(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = .9), width = .2) +
  facet_wrap(~ group, labeller = as_labeller(c("0" = "Women", "1" = "Men"))) +
  scale_fill_met_d(name = "Egypt") +
  labs(x = "Different or Same Clusters", y = "Predicted Probability", fill = "Deviation from\nEqual Investment")+
  scale_x_discrete(labels = c("Different\nClusters", "Same\nCluster")) +
  theme_classic(base_size = 14)
#ggsave("PP4EmotDeviationPlot.jpeg", plot = last_plot(), width = 200, height = 150, units = "mm", path = "/Users/ashle/Desktop",scale = 1, dpi = 300, limitsize = TRUE)



#Q2:Is investment a function of cluster? Which cluster gets more investment?
##investment variables are on 1-7 (time, money) and 1-5 (emotional closeness) scales
##higher numbers on these scales = more investment in partner Orange compared to partner Blue

###financial investment (7-point scale)
finInvestClusterAnova <- aov(fin_invest ~ blueClust+orangeClust, data = data) #no sig interaction, so ran with main effect terms
finInvestClusterMeans <- tapply(data$fin_invest, list(data$blueClust, data$orangeClust), function(x) mean(x, na.rm = T))
finInvestClusterSDs <- tapply(data$fin_invest, list(data$blueClust, data$orangeClust), function(x) sd(x, na.rm = T))
###time investment (7 point scale)
timeInvestClusterAnova <- aov(time_invest ~ blueClust+orangeClust, data = data)
timeInvestClusterMeans <- tapply(data$time_invest, list(data$blueClust, data$orangeClust), function(x) mean(x, na.rm = T))
timeInvestClusterSDs <- tapply(data$time_invest, list(data$blueClust, data$orangeClust), function(x) sd(x, na.rm = T))
###emotional closeness (5 point scale)
emotCloseInvestClusterAnova <- aov(emot_close ~ blueClust+orangeClust, data = data)
emotCloseInvestClusterMeans <- tapply(data$emot_close, list(data$blueClust, data$orangeClust), function(x) mean(x, na.rm = T))



#####Monog vs Poly comparison

#loading in PP2 & 3 data (monog samples)
pp2 <- read.csv("Human Data/Processed Data/PP2AnalyzedData.csv")
pp3 <- read.csv("Human Data/Processed Data/PP3AnalyzedData.csv")

#create pp4 poly comparison df
pp4Comparison <- data[,c(2, 5, 8, 10, 171:174)]

#filter out unnecessary cols of pp2 and pp3 data
pp2comp <- pp2[,c(2, 5, 8, 10, 134:137)] #keeping only sex, gender, poly ID, group, clusters, & same or diff variables
pp3comp <- pp3[,c(2, 5, 8, 10, 144:147)]

#get rid of people that preferred not to say for poly identity
pp2comp <- pp2comp[pp2comp$poly_identity != 2,]
pp3comp <- pp3comp[pp3comp$poly_identity != 2,]

#add study columns
pp4Comparison$study <- "pp4"
pp2comp$study <- "pp2"
pp3comp$study <- "pp3"

#pool dfs together
polyMonogComparisonData <- rbind(pp2comp, pp3comp, pp4Comparison)

#chi square test: are poly participants more likely to have partners in the same cluster
polyMonogCompTest <- chisq.test(table(polyMonogComparisonData$poly_identity, polyMonogComparisonData$sameOrDiff))



#investment: are poly people more likely to have equal vs unequal investment (pp3 & pp4)
pp3Invest <- read.csv("Human Data/Processed Data/PP3InvestData.csv")
pp3Invest$study <- "pp3"

investData$study <- "pp4"

investDataComp <- rbind(pp3Invest, investData)
investDataComp$study <- as.factor(investDataComp$study)
investDataComp$finDeviationDich <- as.factor(ifelse(investDataComp$finDeviation == 0, 0, 1))

#financial
finInvestComp <- clm(finDeviationDich ~ study, data = investDataComp)
#test proportional odds assumption
oddsAssumptionCheckFinComp <- nominal_test(finInvestComp) 


#time
investDataComp$timeDevDich <- as.factor(investDataComp$timeDevDich)
timeInvestComp <- clm(timeDevDich ~ study, data = investDataComp)
#test proportional odds assumption
oddsAssumptionCheckTimeComp <- nominal_test(timeInvestComp) 

#emotional
investDataComp$emotDevDich <- as.factor(investDataComp$emotDevDich)
emotInvestComp <- clm(emotDevDich ~ study, data = investDataComp)
#test proportional odds assumption
oddsAssumptionCheckEmotComp <- nominal_test(emotInvestComp) 


