######Polyamorous Mate Preferences 4#####
####Ashley J Coventry, Tamsin German, Dan Conroy-Beam########

###packages
library(data.table) #for reshaping data
library(ggplot2) #for scree plot + visualizing data
library(rcompanion) #for cohenW test
library(ggpubr) #for ggarrange


###set seed###
set.seed(010423)


data <- read.csv("/Users/ashle/Desktop/Research/Polyamory Research/PolyPrefs.nosync/PolyPrefs.nosync/Human Data/Processed Data/PolyPrefs4Processed Data20240103 152520.csv")


####Ideal Partner Cluster Analysis

##Remove NAs from budget allo items##
#necessary for cluster analysis because kfits is omitting NAs. 

nacheck <- apply(data[,15:28], 1, function(x) sum(is.na(x))>0)
data<- data[!nacheck,] 


#excluding people and ideal partners who don't identify as either men or women

data<-data[data$gender<2,]
data <- data[data$orange_gender <2,]
data <- data[data$blue_gender <2,]


###reshape data wide --> long and only keep columns relevant for kmeans

#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

longData<-melt(as.data.table(data),id.vars=c("PIN","gender","age"),
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
kfitWss<-sapply(1:7,function(x) kmeans(longData[,6:12],x)$tot.withinss)

#scree plot
screePlot<-qplot(1:7,kfitWss)

##compute differences in within ss across k for k-means clustering
wssDiffs<-diff(kfitWss)


##Add this classification (clusters) to the original dataframe

kFit<-kmeans(longData[,6:12],3)

longData$kFitab <- kFit$cluster

##Create vectors of preference means for each cluster (without age)
clustCenters<-kFit$centers

##Look at gender breakdown by cluster
clustGender<-table(longData$gender,longData$kFitab)

##compute variance between trait ratings for each cluster
#to see if maybe one cluster is more well rounded than others
clustVars<-apply(clustCenters,1,var)




###Plotting clusters
##multipanel figure
#first create individual plot for each cluster

#cluster 1 (title will change based on clusters)
meanTrait1 <- clustCenters[1,]
trait1 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting1 <- data.frame(meanTrait1, trait1)
plot1 <- ggplot(data=plotting1, aes(x=trait1, y=meanTrait1)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "springgreen4") +
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level")  +ylim(0,9) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("(C) Well-Rounded")

#cluster 2 
meanTrait2 <- clustCenters[2,]
trait2 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2 <- data.frame(meanTrait2, trait2)
plot2 <- ggplot(data=plotting2, aes(x=trait2, y=meanTrait2)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "darkorchid3")+ 
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(0,9) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("(A) Kind and Wealthy")

#cluster 3 
meanTrait3 <- clustCenters[3,]
trait3 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting3 <- data.frame(meanTrait3, trait3)
plot3 <- ggplot(data=plotting3, aes(x=trait3, y=meanTrait3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "orangered3")+ 
  geom_hline(yintercept = 5, color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(0,9) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("(B) Good in Bed, Kind & Hot")


#combine clusters into one graph
panelPlot<-ggarrange(plot2, plot3, plot1,  nrow=1, ncol=3,font.label = list(size = 14, color = "black"))

#ggsave("PP4panelPlot.jpeg", plot=last_plot(), width=250, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)




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

data$kFitab<-apply(data[,169:170],1,function(x) paste0(sort(as.numeric(x)),collapse=","))



### Chisq + Fisher tests
#are men and women are choosing combos of partner orange and blue at diff rates?

fisherGender <- fisher.test(table(data$gender, data$kFitab), simulate.p.value = TRUE)

##do preferences for partner orange predict preferences for partner blue?
chisqClust<-chisq.test(table(data$blueClust, data$orangeClust)) #yes
chisqClustW <- cohenW( x = chisqClust$observed, p = chisqClust$expected) #effect size


##raw numbers in each cluster combo by gender
clustComboGender<-table(data$blueClust,data$orangeClust,data$gender)


###confusion matrices

##overall clust combo frequency
#create matrix dataframe
overallMatrix <- data.frame(((table(data$blueClust, data$orangeClust))/sum(table(data$blueClust, data$orangeClust)))*100)
#relabel column names
colnames(overallMatrix) <- c("blueCluster", "orangeCluster", "comboFrequency")
#round all numbers to 2 decimal places
overallMatrix[,3] <-round(overallMatrix[,3],2) 

#switch the order of the clusters in the chart (so, hot, then WR, then rich. 1, 2, 3 --> 3, 1, 2)
matrixOrder <- c(3, 1, 2)
overallMatrix$blueCluster <- factor(overallMatrix$blueCluster, levels = matrixOrder)
overallMatrix$orangeCluster <- factor(overallMatrix$orangeCluster, levels = matrixOrder)

#combine cluster combos so only occur once (e.g., 2,1 = 1,2 & 2,3 = 3,2)
overallMatrix[2,3] <- overallMatrix[2,3]+ overallMatrix[4,3]
overallMatrix[4,3] <- NA

overallMatrix[7,3] <- overallMatrix[3,3] + overallMatrix[7,3]
overallMatrix[3,3] <- NA

overallMatrix[8,3] <- overallMatrix[8,3] + overallMatrix[6,3]
overallMatrix[6,3] <- NA

#plot matrix
overallMatrixPlot <- ggplot(overallMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = overallMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Good in Bed, Kind, & Hot','Well-Rounded','Kind & Wealthy')) +
  scale_y_discrete(labels = c('Good in Bed, Kind, & Hot','Well-Rounded','Kind & Wealthy')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13)) #warning but just bc we intentionally have empty cells

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

maleMatrix[7,3] <- maleMatrix[3,3] + maleMatrix[7,3]
maleMatrix[3,3] <- NA

maleMatrix[8,3] <- maleMatrix[8,3] + maleMatrix[6,3]
maleMatrix[6,3] <- NA

#plot matrix
maleMatrixPlot <- ggplot(maleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = maleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Good in Bed,\nKind, & Hot','Well-Rounded','Kind & Wealthy')) +
  scale_y_discrete(labels = c('Good in Bed,\nKind, & Hot','Well-Rounded','Kind & Wealthy')) +
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
femaleMatrix[2,3] <- femaleMatrix[2,3]+ femaleMatrix[4,3]
femaleMatrix[4,3] <- NA

femaleMatrix[7,3] <- femaleMatrix[3,3] + femaleMatrix[7,3]
femaleMatrix[3,3] <- NA

femaleMatrix[8,3] <- femaleMatrix[8,3] + femaleMatrix[6,3]
femaleMatrix[6,3] <- NA

#plot matrix
femaleMatrixPlot <- ggplot(femaleMatrix, aes(x= blueCluster, y = orangeCluster, fill = comboFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = femaleMatrix$comboFrequency)+
  scale_fill_gradient(low = "white", high = "#009900", na.value = "whitesmoke") +
  scale_x_discrete(labels = c('Good in Bed,\n Kind, & Hot','Well-Rounded','Kind & Wealthy')) +
  scale_y_discrete(labels = c('Good in Bed,\n Kind, & Hot','Well-Rounded','Kind & Wealthy')) +
  labs(x = "Partner Blue", y = "Partner Orange", fill = "Combination Freq.") +
  theme(text = element_text(size = 13)) +
  ggtitle("(A) Female Participants")

#panel plot of both of these graphs
MatrixPlotPanel <- ggarrange(femaleMatrixPlot, maleMatrixPlot, nrow=1, ncol=2, 
                             common.legend = TRUE, legend = "right")
#ggsave("PP4MatrixPlotPanel.jpeg", plot=last_plot(), width=275, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



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





###How many ideal partners are in each cluster?

##for people who wanted both partners in same cluster
sameClust <- table(data$blueClust[data$sameOrDiff == 0]) 

##for people who wanted partners in different clusters
#don't need to run separately for clusters A and B bc made order arbitrary
diffClust <- table(data$blueClust[data$sameOrDiff ==1])  



###More Chi Squares: proportion of participants with at least 1 partner in specific clusters 
#is one gender more likely than the other to want at least 1 partner in a given cluster?

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


chisqClust1 <- chisq.test(table(data$gender, data$clust1)) #not sig

chisqClust2 <- chisq.test(table(data$gender, data$clust2)) #not sig

chisqClust3 <- chisq.test(table(data$gender, data$clust3)) #not sig



###Ideal gender analysis
#is an ideal partner more likely to be in a specific cluster given that partner's gender

#partner blue

chisqIdealGenderBlue <- chisq.test(table(longData$idealGender[longData$partner == "idealBlue"], 
                                         longData$kFitab[longData$partner == "idealBlue"])) #not sig


#partner orange ??
chisqIdealGenderOrange <- chisq.test(table(longData$idealGender[longData$partner == "idealOrange"], longData$kFitab[longData$partner == "idealOrange"])) 
chisqIdealGenderOrangeW <- cohenW( x = chisqIdealGenderOrange$observed, p = chisqIdealGenderOrange$expected) #effect size

idealGenderClustOrange <- table(longData$idealGender[longData$partner == "idealOrange"], longData$kFitab[longData$partner == "idealOrange"])



###Investment Questions Analysis###

##Q1: is there a greater deviation from the center (equal invest.) if 2 partners are in diff clusters

##make long dataframe with investment info
#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

investData<-melt(as.data.table(data),id.vars=c("PIN", "gender", "fin_invest","time_invest","emot_close", "sameOrDiff"), #sameOrDiff: same = 1, diff = 0
                 measure.vars=list(c(169,170))) #clust (first listed = blue, second = orange)

#renaming columns
colnames(investData) <- c("PIN", "gender", "finInvest", "timeInvest", "emotClose", "sameOrDiff", "partner", "cluster")


##calculating deviation from equal invest
investData <- investData %>% 
  mutate(finDeviation = abs(finInvest - 4),
         timeDeviation = abs(timeInvest - 4),
         emotDeviation = abs(emotClose - 4))




##anova comparing deviation for same vs diff clusters
investData$sameOrDiff <- as.factor(investData$sameOrDiff)
investData$gender <- as.factor(investData$gender)

#get rid of missing deviation data
nacheckInvest <- apply(investData[,9:11], 1, function(x) sum(is.na(x))>0)
investData<- investData[!nacheckInvest,]

finInvestAnova <- aov(finDeviation ~ sameOrDiff + gender, data = investData)
timeInvestAnova <- aov(timeDeviation ~ sameOrDiff + gender, data = investData)
emotCloseAnova <- aov(emotDeviation ~ sameOrDiff + gender, data = investData)



##Q2: is investment related to whether a partner is well-rounded or not (not pre-registered)
data$blueClust <- as.factor(data$blueClust)
data$orangeClust <- as.factor(data$orangeClust)

##wellRounded investment
#is whether a partner is in the well-rounded cluster predicted by investment level

#create wellRounded variable (0 = wellRounded, 1 = not)

investData$wellRounded <- 
  ifelse(investData$cluster == 3, 0, 1)

#create recoded investment variable (so high number is just more investment in that partner, instead of in orange)

investData$finInvestRC <- investData$finInvest #duplicate finInvest variable
investData$timeInvestRC <- investData$timeInvest #duplicate timeInvest variable
investData$emotCloseRC <- investData$emotClose #duplicate emotClose variable


investData$finInvestRC <- ifelse(investData$partner == 'blueClust', 
                                 car::recode(investData$finInvestRC, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2;7=1"), 
                                 investData$finInvestRC)

investData$timeInvestRC <- ifelse(investData$partner == 'blueClust', 
                                  car::recode(investData$timeInvestRC, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2;7=1"), 
                                  investData$timeInvestRC)

investData$emotCloseRC <- ifelse(investData$partner == 'blueClust', 
                                 car::recode(investData$emotCloseRC, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2;7=1"), 
                                 investData$emotCloseRC)


#create composite investment variable (averaging 3 together)

investData$compInvest <- rowMeans(investData[,13:15])

#glm predicting wellrounded from investment
investData$partner <- as.factor(investData$partner)

wellRoundedInvestGLM <- glmer(wellRounded ~ compInvest + (1|PIN), family = binomial(), data = investData)

#use plotzing to graph
#use plotzing to graph 
compInvestGraph <- graph_line("compInvest", "wellRounded", 
                              setcolor = "purple",
                              setyaxislabel = "Investment in Partner\n(1 = entirely in other partner; 7 = entirely in this partner",
                              setxaxislabel = "Partner Cluster",
                              setxlevels = c("Well-Rounded", "Not\nWell-Rounded"),
                              setyaxissize = 11,
                              setxaxissize = 11,
                              setytitlesize = 10,
                              setxtitlesize = 10,
                              data = investData)



#ggsave("PP4InvestPlot.jpeg", plot=last_plot(), width=150, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)

##graph of investment with both monog and poly people

#import monog data
pp3InvestData <- read.csv("/Users/ashle/Desktop/PolyPrefs3InvestData.csv")
#create sample column
pp3InvestData$study <- "monogSamp"
#get only needed columns
pp3InvestData <- pp3InvestData[,c("wellRounded", "compInvest", "study")]

#same for pp4 data
pp4InvestData <- investData[,c("wellRounded", "compInvest")]
pp4InvestData$study <- "polySamp"

#combine two dfs into one
compareInvestData <- rbind(pp3InvestData, pp4InvestData)

#graph using plotzing

compInvestGraphMonogPoly <- graph_line("compInvest", "wellRounded", "study",
                              setcolors = c("blue", "hotpink"),
                              setyaxislabel = "Investment in Partner\n(1 = entirely in other partner; 7 = entirely in this partner",
                              setxaxislabel = "Partner Cluster",
                              setxlevels = c("Well-Rounded", "Not\nWell-Rounded"),
                              setlegendlevels = c("Monogamous Sample", "Polyamorous Sample"),
                              setlegendtitle = "Sample",
                              setyaxissize = 10,
                              setxaxissize = 10,
                              setytitlesize = 10,
                              setxtitlesize = 10,
                              data = compareInvestData)
#ggsave("CompInvestPlot.jpeg", plot=last_plot(), width=250, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)





###compare whether higher similarity in study 4 is a meaningful difference compared to study 3

#load study 3 dataframe
pp3Data <- read.csv("/Users/ashle/Desktop/Research/Polyamory Research/PolyPrefs.nosync/PolyPrefs.nosync/PP3AnalyzedData.csv")
pp3Data$study <- "monogSamp" #create sample column

#get only needed columns
pp3Sample <- pp3Data[,c("sameOrDiff", "study")]

#load study 2 dataframe
pp2Data <- read.csv("/Users/ashle/Desktop/Research/Polyamory Research/PolyPrefs.nosync/PolyPrefs.nosync/PP2AnalyzedData.csv")
pp2Data$study <- "monogSamp" #create sample column (same as for study 3 bc pooling the samples)

#get only needed columns
pp2Sample <- pp2Data[,c("sameOrDiff", "study")]


#add sample column to pp4 data
data$study <- "polySamp"

#get only needed columns
pp4Sample <- data[,c("sameOrDiff", "study")]


#comparing samples of study 3 to study 4:
#combine two dfs into one
compareData <- rbind(pp2Sample, pp3Sample, pp4Sample)

#chisq
compareChisq <- chisq.test(table(compareData$study, compareData$sameOrDiff)) #1 = same, 2 = different
comparechisqW <- cohenW( x = compareChisq$observed, p = compareChisq$expected) #effect size




####Actual Partner Cluster Analysis


analysesActual <- data[!(data[,10] == 1),]
columns <- c(1:2, 5, 10, 79, 96, 135, 143:156)
analysesActual <- data[,columns]


##reshape data wide --> long

#Remove NAs from budget allo items#
#necessary for cluster analysis because kfits is omitting NAs. 

nacheck2 <- apply(analysesActual[,8:21], 1, function(x) sum(is.na(x))>0)
analysesActual<- analysesActual[!nacheck2,] 


#excluding actual partners who don't identify as either men or women
analysesActual <- analysesActual[analysesActual$actual_a_gender <3,]
analysesActual <- analysesActual[analysesActual$actual_b_gender <3,]


###reshape data wide --> long and only keep columns relevant for kmeans

#melt function to have two rows (1 for orange, 1 for blue) with each trait rating

longDataActual<-melt(as.data.table(analysesActual),id.vars=c("PIN","gender","age", "sex", "num_partners"),
               measure.vars=list(c(5,6), #gender (first listed = partner a, second = partner b)
                                 c(8,15), #ambition
                                 c(11,18), #attractiveness
                                 c(9,16), #intelligence
                                 c(13,20), #good in bed
                                 c(10,17), #kindness
                                 c(14,21), #status
                                 c(12,19))) #wealth
#Relabel columns
colnames(longDataActual)[6:14]<-c("partner","partnerGender","amb","attract",
                            "intel","sexy","kind",
                            "stat","finPros")


#Relabel partner type values (1 = blue, 2 = orange)
longDataActual$partner<-as.factor(ifelse(longDataActual$partner==1,"idealA","idealB"))


###ipsatizing
#take means of traits for every PIN and subtract mean from indiv traits & place in new columns

longDataActual <- longDataActual %>%
  mutate(across(c("amb", "attract", "intel", "sexy", "kind", "stat", "finPros"),
                ~ . - mean(., na.rm = TRUE), .names = "{col}Ip"))





###K-Means Cluster Analysis###

#extract kmeans wss
kfitWssActual<-sapply(1:7,function(x) kmeans(longDataActual[,15:21],x)$tot.withinss)

#scree plot
screePlotActual<-qplot(1:7,kfitWssActual)

##compute differences in within ss across k for k-means clustering
wssDiffsActual<-diff(kfitWssActual)


##Add this classification to the original dataframe

kFitActual<-kmeans(longDataActual[,15:21],3)

longDataActual$kFitab <- kFitActual$cluster

##Create vectors of preference means for each cluster (without age)
clustCentersActual<-kFitActual$centers

##Look at gender breakdown by cluster
clustGenderActual<-table(longDataActual$gender,longDataActual$kFitab)

##compute variance between trait ratings for each cluster
#to see if maybe one cluster is more well rounded than others
clustVarsActual<-apply(clustCentersActual,1,var)





###Plotting clusters
##multipanel figure
#first create individual plot for each cluster

#cluster 1 (title will change based on clusters)
meanTrait1Act <- clustCentersActual[1,]
trait1Act <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting1Act <- data.frame(meanTrait1Act, trait1Act)
plot1Act <- ggplot(data=plotting1Act, aes(x=trait1Act, y=meanTrait1Act)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "orangered3") +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level")  +ylim(-2,2) +
  theme(axis.text.x = element_text(angle = 90)) 

#cluster 2 
meanTrait2Act <- clustCentersActual[2,]
trait2Act <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2Act <- data.frame(meanTrait2Act, trait2Act)
plot2Act <- ggplot(data=plotting2Act, aes(x=trait2Act, y=meanTrait2Act)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "darkorchid3")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(-2,2) +
  theme(axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3Act <- clustCentersActual[3,]
trait3Act <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting3Act <- data.frame(meanTrait3Act, trait3Act)
plot3Act <- ggplot(data=plotting3Act, aes(x=trait3Act, y=meanTrait3Act)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "springgreen4")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(-2,2) +
  theme(axis.text.x = element_text(angle = 90)) 




#combine clusters into one graph
panelPlotAct<-ggarrange(plot1Act, plot2Act, plot3Act, nrow=1, ncol=3,font.label = list(size = 14, color = "black"))


