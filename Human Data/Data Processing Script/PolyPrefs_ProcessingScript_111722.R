#############Processing Script: Polyamorous Mate Preferences -- Budget Allocation #################
  ####Ashley J Coventry, Tamsin German, Dan Conroy-Beam########

###load packages 
library(psych)
library(lmerTest)
library(lme4)
library(car)


###load data###
data<-read.csv(file.choose())


###eliminate title rows 
data<-data[-c(1:2),]


###eliminate unnecessary columns
data<-data[,18:116]

###eliminate generated data (prior to study launch)
  #how do I do this
  #can i just delete it from the excel file
  

###eliminate potential bots based on written responses

##eliminate obvious gibberish/bots 
##will need to be coded## --> column "textquality" will be added
#data <- subset(data, data$textquality != "b") ##b = data coded as bot


###also look for and eliminate duplicate responses in free responses since those are unlikely to be different people

#change blank responses to NA
for(i in 1:NROW(data)){
  ifelse(data$monog_attn[i] == "", 
         data$monog_attn[i] <- NA, 
         data$monog_attn[i]<-data$monog_attn[i])
}

for(i in 1:NROW(data)){
  ifelse(data$poly_attn[i] == "", 
         data$poly_attn[i] <- NA, 
         data$poly_attn[i]<-data$poly_attn[i])
}


##check for duplicates 

#for monogamous attn check: 
data$monog_attn_dup <- data$monog_attn %in%
  data$monog_attn[duplicated(data$monog_attn, incomparables = NA)] 
  #adjust the T/F vector to have one or two word answers as F (makes sense that those would be dupes)
data$monog_attn_dup<-ifelse(nchar(data$monog_attn)<12,FALSE,data$monog_attn_dup)

#for polyamorous attention check:
data$poly_attn_dup <- data$poly_attn %in%
  data$poly_attn[duplicated(data$poly_attn, incomparables = NA)]
data$poly_attn_dup<-ifelse(nchar(data$poly_attn)<12,FALSE,data$poly_attn_dup)

##only keep participants with unique sentence answers 

data<-data[is.na(data$monog_attn_dup) | data$monog_attn_dup==F,]
data<-data[is.na(data$poly_attn_dup) | data$poly_attn_dup==F,]



###create pin

data$PIN<-as.character(sample(1:nrow(data)))



####This below does not work rn bc for some reason the data exported as words and not numbers and also only in one column

###change column names for race
#colnames(data)[7:13] <- c("race_asian", "race_black", "race_hispanic", "race_mena", "race_native", "race_white", "race_na")

###recode self and actual partner  ratings into 0-10 scale instead of 1:11(excluding age and gender)
#data[,c(31:44, 49:62, 66:79, 83:96)] <- (data[,c(31:44, 49:62, 66:79, 83:96)]-1)

###combine self ratings and actual partner ratings into composites
  #1 composite for the 2 questions of each trait
  #create data frame of just trait ratings 

#ratings <- data[,c(31:44, 49:62, 66:79, 83:96)]

##compute composite ratings 
  #go into ratings and take first two columns and first row and calculate mean
  #do this for all rows and pairs of columns in ratings

#comps <- sapply(seq(1,56,2), function(x) rowMeans(ratings[,x:(x+1)]))

  #take every other column name in ratings and put in vector

#compnames <- colnames(ratings)[seq(1,56,2)]

  #eliminate the 1 from each column name and replace with nothing (e.g. intell_1 to intell)

#compnames<-gsub("1","",compnames)

  #add compnames as column names in comps dataset

#colnames(comps)<- compnames

  #add comps data set to data

#data<-cbind(data, comps)


###create group variable 
#single monogamous, partnered monogamous, single polyamorous, one partner polyamorous more than one partner polyamorous
#data$group<-
  #ifelse(data$poly_identity == 1 & data$rel_status == 1, "single_poly", 
         #ifelse(data$poly_identity == 1 & data$num_partners == 1, "one_poly",
               # ifelse(data$poly_identity == 1 & data$num_partners != 1, "multi_poly", 
                       #ifelse(data$poly_identity == 0 & data$rel_status == 1, "single_monog", "partnered_monog"))))



###save processed dataframe as a csv
#date<-format(Sys.time(),format="%Y%m%d %H%M%S")

#write.csv(data,paste0("/Users/ashle/Desktop/Research/Polyamory Research/PolyPrefs/Human Data/Processed Data",date,".csv"), row.names = FALSE)

