#############Processing Script: Polyamorous Mate Preferences 3 -- Budget Allocation 2 #################
####Ashley J Coventry, Tamsin German, Dan Conroy-Beam########



###load packages 
library(psych)
library(lmerTest)
library(lme4)
library(car)
library(dplyr)




###load data###
data<-read.csv('Human Data/Raw Data/Poly Prefs Study 3_January 18, 2025_13.35.csv')



###eliminate title rows 
data<-data[-c(1:2),]


###eliminate unnecessary columns
data<-data[,18:127]



###eliminate potential bots based on written responses
#was coded --> column "textQuality" added, 1 = bot, 0 = not
data <- subset(data, data$textQuality == "0") 

###also eliminate duplicate responses since those are likely to be same person

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


###remove participants who failed the attention checks (28 -- attncheck9, 35 -- attn_check8, 59 -- attn_check10, 105 -- attn_check10.1, )
#not everyone got every attention check so ignore NAs 

#attncheck9 (in budget allo task, so everyone got this. No NAs.)
data <- subset(data, data$attncheck9 == "9") 

#attn_check8 (in self traits section, so everyone got this one. No NAs.)
data <- subset(data, data$attn_check8 == "8")

#attn_check10 (in actual partner a traits, only some people got this.)
  #forgot to recode in qualtrics, so correct answer is 11 here
  #keeping everyone who answered correctly or whose response is blank (didn't get question)
data <- subset(data, data$attn_check10 == "11" | data$attn_check10 == "")


#attn_check10.1 (in actual partner traits, only some got this)
  #correct answer is 10
  #keeping everyone who answered correctly or whose response is blank (didn't get question)
data <- subset(data, data$attn_check10.1 == "10" | data$attn_check10.1 == "")

#attn check brings 297 initial observation to 231 (eliminates 66 responses)

###create pin

data$PIN<-as.character(sample(1:nrow(data)))



###rename budget allocation columns
colnames(data)[14:27] <- c("idealAmbitionBlue", "idealAttractBlue", "idealIntelBlue", 
                           "idealSexyBlue", "idealKindBlue", "idealStatusBlue", "idealWealthBlue", 
                           "idealAmbitionOrange", "idealAttractOrange", "idealIntelOrange", 
                           "idealSexyOrange", "idealKindOrange", "idealStatusOrange", "idealWealthOrange")


###race

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
    TRUE ~ NA_character_ # for undefined mappings
  ))


###change values from character to numeric
data[,c(1, 10, 12:28, 33:52, 57:72, 75:89, 92:107)]<-as.numeric(unlist(data[,c(1, 10, 12:28, 33:52, 57:72, 75:89, 92:107)]))


###recode self and actual partner  ratings into 0-10 scale instead of 1:11(excluding age and gender)
data[,c(33:34, 36:47, 57:58, 60:71, 75:88, 92:104, 106)] <- (data[,c(33:34, 36:47, 57:58, 60:71, 75:88, 92:104, 106)]-1)



###combine self ratings and actual partner ratings into composites
#1 composite for the 2 questions of each trait
#create data frame of just trait ratings 

ratings <- data[,c(33:34, 36:47, 57:58, 60:71, 75:88, 92:104, 106)]

##compute composite ratings 
#go into ratings and take first two columns and first row and calculate mean
#do this for all rows and pairs of columns in ratings
comps <- sapply(seq(1,56,2), function(x) rowMeans(ratings[,x:(x+1)]))

#take every other column name in ratings and put in vector
compNames <- colnames(ratings)[seq(1,56,2)]

#eliminate the 1 from each column name and replace with nothing (e.g. intell_1 to intell)
compNames<-gsub("1","",compNames)

#add compnames as column names in comps dataset
colnames(comps)<- compNames

#add comps data set to data
data<-cbind(data, comps)


###create group variable 
#single monogamous, partnered monogamous, single polyamorous, one partner polyamorous more than one partner polyamorous

data <- data %>%
  mutate(group = case_when(
    is.na(poly_identity) | is.na(rel_status) ~ "NA", #make NA if either is NA
    poly_identity == 2 ~ "NA", #make NA if "prefer not to say" for poly identity question
    poly_identity == 1 & rel_status == 1 ~ "single_poly",          
    poly_identity == 1 & num_partners == 1 ~ "one_poly",         
    poly_identity == 1 & num_partners > 1 ~ "multi_poly",          
    poly_identity == 0 & rel_status == 1 ~ "single_monog",         
    TRUE ~ "partnered_monog"                                    
  ))




#Remove people who didn't complete the budget allocation

nacheck <- apply(data[,14:27], 1, function(x) sum(is.na(x))>0)
data<- data[!nacheck,]


#Remove participants who don't identify as either a man or woman
#or selected "prefer not to say"

data<-data[data$gender<2,]


###save processed dataframe as a csv
date<-format(Sys.time(),format="%Y%m%d %H%M%S")

write.csv(data,paste0("Human Data/Processed Data/", "PolyPrefs3_ProcessedData",date,".csv"), row.names = FALSE)








