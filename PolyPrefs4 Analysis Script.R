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


##Add this classification (4 clusters) to the original dataframe

kFit<-kmeans(longData[,6:12],4)

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
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "orangered3") +
  geom_hline(yintercept = mean(meanTrait1), color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level")  +ylim(0,9) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Attractive & \nGood in Bed")

#cluster 2 
meanTrait2 <- clustCenters[2,]
trait2 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting2 <- data.frame(meanTrait2, trait2)
plot2 <- ggplot(data=plotting2, aes(x=trait2, y=meanTrait2)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "darkorchid3")+ 
  geom_hline(yintercept = mean(meanTrait2), color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(0,9) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Kind & Smart")

#cluster 3 
meanTrait3 <- clustCenters[3,]
trait3 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting3 <- data.frame(meanTrait3, trait3)
plot3 <- ggplot(data=plotting3, aes(x=trait3, y=meanTrait3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "springgreen4")+ 
  geom_hline(yintercept = mean(meanTrait3), color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(0,9) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Really Attractive, \nGood in Bed, \n& Kind")


#cluster 4 
meanTrait4 <- clustCenters[4,]
trait4 <- c("Ambition", "Attractiveness", "Intelligence", "Good in Bed", "Kindness", "Status", "Resources")
plotting4 <- data.frame(meanTrait4, trait4)
plot4 <- ggplot(data=plotting4, aes(x=trait4, y=meanTrait4)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "lightpink")+ 
  geom_hline(yintercept = mean(meanTrait3), color="black", linetype = "dashed", linewidth = 1) +
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Absolute Desired Trait Level") +ylim(0,9) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Well-Rounded")

#combine clusters into one graph
panelPlot<-ggarrange(plot1, plot2, plot3, plot4, nrow=1, ncol=4,font.label = list(size = 14, color = "black"))





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



### Fisher tests
#are men and women are choosing combos of partner orange and blue at diff rates?

fisherGender <- fisher.test(table(data$gender, data$kFitab), simulate.p.value = TRUE)

##do preferences for partner orange predict preferences for partner blue?
fisherClust<-fisher.test(table(data$blueClust, data$orangeClust), simulate.p.value = TRUE) #yes
chisqClustW <- cohenW(x = chisqClust$observed, p = chisqClust$expected) #effect size

##raw numbers in each cluster combo by gender
clustComboGender<-table(data$blueClust,data$orangeClust,data$gender)




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
data$clust4 <- NA #cluster 4


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

for(i in 1:NROW(data)){
  ifelse(data$orangeClust[i] == 4 | data$blueClust[i] == 4, data$clust4[i]<-1, data$clust4[i]<-0)
  
}

chisqClust1 <- chisq.test(table(data$gender, data$clust1)) #not sig

chisqClust2 <- chisq.test(table(data$gender, data$clust2)) #not sig

chisqClust3 <- chisq.test(table(data$gender, data$clust3)) #not sig

chisqClust4 <- chisq.test(table(data$gender, data$clust4))  #not sig


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


#look at deviation descriptively
table(investData$finDeviation)
table(investData[investData$sameOrDiff == 1]$finDeviation)
table(investData[investData$sameOrDiff == 0]$finDeviation) 


table(investData$timeDeviation)
table(investData[investData$sameOrDiff == 1]$timeDeviation)
table(investData[investData$sameOrDiff == 0]$timeDeviation)


table(investData$emotDeviation)
table(investData[investData$sameOrDiff == 1]$emotDeviation)
table(investData[investData$sameOrDiff == 0]$emotDeviation) 


##anova comparing deviation for same vs diff clusters
investData$sameOrDiff <- as.factor(investData$sameOrDiff)
investData$gender <- as.factor(investData$gender)

finInvestAnova <- aov(finDeviation ~ sameOrDiff + gender, data = investData)
timeInvestAnova <- aov(timeDeviation ~ sameOrDiff + gender, data = investData)
emotCloseAnova <- aov(emotDeviation ~ sameOrDiff + gender, data = investData)



##Q2: does this investment vary by cluster and also cluster of other partner?
data$blueClust <- as.factor(data$blueClust)
data$orangeClust <- as.factor(data$orangeClust)

#fin invest variance based on cluster type

finInvestClustInt <- aov(fin_invest ~ blueClust*orangeClust, data = data)
finInvestClustMain <- aov(fin_invest ~ blueClust+orangeClust, data = data)


#time invest variance based on cluster type
timeInvestClustInt <- aov(time_invest ~ blueClust*orangeClust, data = data) 
timeInvestClustMain <- aov(time_invest ~ blueClust+orangeClust, data = data) 

tukeyTimeInvestClust <- TukeyHSD(timeInvestClustMain)

#emotional closeness variance based on cluster type
emotCloseClustInt <- aov(emot_close ~ blueClust*orangeClust, data = data) 
emotCloseClustMain <- aov(emot_close ~ blueClust+orangeClust, data = data) 

tukeyEmotCloseClust <- TukeyHSD(emotCloseClustMain)







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


##Add this classification (4 clusters) to the original dataframe

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


