#############Polyamorous Mate Preferences 4: Processing Script #################
####Ashley J Coventry, Tamsin German, Dan Conroy-Beam#######

#packages
library(dplyr)



###load data 
data<-read.csv("Human Data/Raw Data/Poly Prefs Study 4_January 20, 2025_14.18.csv")


###eliminate title rows 
data<-data[-c(1:2),]

###only keep needed columns
data<-data[,18:151]


###only keep participants who still identify as polyamorous
data <- subset(data, data$poly_identity == "1") 




###eliminate participants who could be bots (gibberish responses to open-ended question)
#gibberish or not coded in raw datafile manually (0 = okay, 1 = gibberish)
data <- subset(data, data$textQuality == "0") 


 ###eliminate duplicate responses to open-ended since those are likely to be same person

##change blank responses to NA

for(i in 1:NROW(data)){
  ifelse(data$poly_attn[i] == "", 
         data$poly_attn[i] <- NA, 
         data$poly_attn[i]<-data$poly_attn[i])
}


##check for duplicates 

#for polyamorous attention check:
data$poly_attn_dup <- data$poly_attn %in%
  data$poly_attn[duplicated(data$poly_attn, incomparables = NA)]
data$poly_attn_dup<-ifelse(nchar(data$poly_attn)<12,FALSE,data$poly_attn_dup)

##only keep participants with unique sentence answers 

data<-data[is.na(data$poly_attn_dup) | data$poly_attn_dup==F,] #no one eliminated


###eliminate participants who failed the attn check

#attn check 9 (in budget allo task, so everyone got this. No NAs.)
data <- subset(data, data$attncheck9 == "9") 

#attn check 8 #in self ratings task, so everyone got this. No NAs.)
data <- subset(data, data$attn_check8 == "8") 

#attn_check10a (in actual partner a traits, only some people got this.)
#keeping everyone who answered correctly or whose response is blank (didn't get question)
#correct answer is 11 because I forgot to recode in qualtrics
data <- subset(data, data$attn_check10a == "11" | data$attn_check10a == "")

#attn check 10b (in actual traits for ppl with 1 partner, so only some got this)
#keeping everyone who answered correctly or whose response is blank (didn't get question)
data <- subset(data, data$attn_check10b == "10" | data$attn_check10b == "")



##create PIN
data$PIN<-as.character(sample(1:nrow(data)))


##rename budget allocation columns
colnames(data)[15:28] <- c("idealAmbitionBlue", "idealAttractBlue", "idealIntelBlue", 
                           "idealSexyBlue", "idealKindBlue", "idealStatusBlue", "idealWealthBlue", 
                           "idealAmbitionOrange", "idealAttractOrange", "idealIntelOrange", 
                           "idealSexyOrange", "idealKindOrange", "idealStatusOrange", "idealWealthOrange")



##race
data <- data %>%
  mutate(raceText = case_when(
    is.na(race) ~ NA_character_, #any NAs stay as NA
    race == 1 ~ "Asian or Pacific Islander",
    race == 2 ~ "Black or African American",
    race == 3 ~ "Hispanic or Latino",
    race == 4 ~ "Middle Eastern or North African",
    race == 5 ~ "Native American or American Indian", 
    race == 6 ~ "White",
    race == 7 ~ "Prefer not to say",
    TRUE ~ "Multi-racial", #any combos
  ))


##change values from characters to numeric
data[,c(1, 10, 15:29, 34:60, 63:78, 81:95, 98:112, 115:130)]<-as.numeric(unlist(data[,c(1, 10, 15:29, 34:60, 63:78, 81:95, 98:112, 115:130)]))

###recode self and actual partner  ratings into 0-10 scale instead of 1:11(excluding age and gender)
data[,c(34:35, 37:48, 63:64, 66:77, 81:94, 115:127, 129)] <- (data[,c(34:35, 37:48, 63:64, 66:77, 81:94, 115:127, 129)]-1)


###combine self ratings and actual partner ratings into composites
#1 composite for the 2 questions of each trait

##create data frame of just trait ratings 
ratings <- data[,c(34:35, 37:48, 63:64, 66:77, 81:94, 115:127, 129)]

##compute composite ratings 
#go into ratings and take first two columns and first row and calculate mean
#do this for all rows and pairs of columns in ratings
comps <- sapply(seq(1,56,2), function(x) rowMeans(ratings[,x:(x+1)]))

##take every other column name in ratings and put in vector
compNames <- colnames(ratings)[seq(1,56,2)]

##eliminate the 1 from each column name and replace with nothing (e.g. intell_1 to intell)
compNames<-gsub("1","",compNames)

##add compnames as column names in comps dataset
colnames(comps)<- compNames

##add comps data set to data
data<-cbind(data, comps)


###SOI-R composite
##Behavior facet (avg of first 3 items) (everyone completed these items)
data$soiBehavior <- rowMeans(data[,c("soi_partners", "soi_oneencounter", "soi_shortterm")], na.rm = TRUE)

##Attitude facet (items 4-6, with 6 reverse coded)

#reverse code item 6 (1 - 9 scale)
data$soi_longtermR <- 10 - data$soi_longterm_reverse_1

#avg items 4-6
data$soiAttitude <- rowMeans(data[,c("soi_sexonly_1", "soi_casual_agreement_1", "soi_longtermR")], na.rm = TRUE)

##Desire facet (items 7-9)
data$soiDesire <- rowMeans(data[,c("soi_fantasies1", "soi_arousal", "soi_fantasies2")], na.rm = TRUE)

##combined for overall global sociosexual orientation
#avg of all 9 items
data$soiGlobal <- rowMeans(data[, c("soi_partners","soi_oneencounter","soi_shortterm", 
                                    "soi_sexonly_1", "soi_casual_agreement_1", "soi_longtermR",
                                    "soi_fantasies1", "soi_arousal", "soi_fantasies2")], na.rm = TRUE)



#Remove people who didn't complete the budget allocation
nacheck <- apply(data[,15:28], 1, function(x) sum(is.na(x))>0)
data<- data[!nacheck,]


#Remove participants who don't identify as either a man or woman
#or selected "prefer not to say"

data<-data[data$gender<2,]


###save processed dataframe as a csv
date<-format(Sys.time(),format="%Y%m%d %H%M%S")

write.csv(data,paste0("Human Data/Processed Data/", "PolyPrefs4_ProcessedData",date,".csv"), row.names = FALSE)

