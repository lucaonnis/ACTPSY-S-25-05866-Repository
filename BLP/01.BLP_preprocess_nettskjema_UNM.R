# Task: Preprocess data from the BLP, Bilingual Language Profile (Birdsong, D., Gertken, L.M., & Amengual, 2012)
# Description: takes as input a .xlsx or .txt file downloaded from UiO's Nettskjema form (modified_BLP)
# Source link:
# Authors: David Thornton and Luca Onnis
# Date: 2023-2024
# Description: A script that calculates language entropy values from the BLP questionnaire
# BLP = Bilingual Language Profile - adapted from https://sites.la.utexas.edu/bilingual/
# An instrument for assessing language dominance through self-reports
# Note: This version measures experience with up to THREE languages.
# Takes as input a .xlsx or .txt file downloaded from UiO's Nettskjema form ("Modified BLP 2024 (Norsk / English)").

# Added 13.02.2024 by Luca ----
# install.packages("readxl")
#library("readxl")

# install.packages("janitor")
library(janitor)
library(tidyverse)

# Load the latest excel or txt file (downloaded from Nettskjema)
# location: https://nettskjema.no/user/form/406810
rm(testdata)
testdata <- read.csv("data-580477-2026-08-12-0904.csv",sep = ";")
View(testdata)
#testdata <- testdata %>% row_to_names(row_number = 1) %>% clean_names(row_number = 1)
#View(testdata)
names(testdata) #70
#testdata <- testdata[, -70] # remove fluke column
# Rename columns ----
colnames(testdata)[1] <- "nettskjema_id"
colnames(testdata)[2] <- "date_created"
colnames(testdata)[3] <- "subject"
colnames(testdata)[4] <- "age"
colnames(testdata)[5] <- "sex"
colnames(testdata)[6] <- "edu"
colnames(testdata)[7] <- "l1"
colnames(testdata)[8] <- "other_l1"
colnames(testdata)[9] <- "l2"
colnames(testdata)[10] <- "other_l2"
colnames(testdata)[11] <- "l3"
colnames(testdata)[12] <- "other_l3"
colnames(testdata)[13] <- "start_learn_l1"
colnames(testdata)[14] <- "start_learn_l2"
colnames(testdata)[15] <- "start_learn_l3"
colnames(testdata)[16] <- "comfort_l1"
colnames(testdata)[17] <- "comfort_l2"
colnames(testdata)[18] <- "comfort_l3"
colnames(testdata)[19] <- "classes_l1"
colnames(testdata)[20] <- "classes_l2"
colnames(testdata)[21] <- "classes_l3"
colnames(testdata)[22] <- "years_country_l1"
colnames(testdata)[23] <- "years_country_l2"
colnames(testdata)[24] <- "years_country_l3"
colnames(testdata)[25] <- "family_l1"
colnames(testdata)[26] <- "family_l2"
colnames(testdata)[27] <- "family_l3"
colnames(testdata)[28] <- "work_l1"
colnames(testdata)[29] <- "work_l2"
colnames(testdata)[30] <- "work_l3"
colnames(testdata)[31] <- "friends_l1"
colnames(testdata)[32] <- "friends_l2"
colnames(testdata)[33] <- "friends_l3"
colnames(testdata)[34] <- "use_family_l1"
colnames(testdata)[35] <- "use_family_l2"
colnames(testdata)[36] <- "use_family_l3"
colnames(testdata)[37] <- "use_work_l1"
colnames(testdata)[38] <-  "use_work_l2"
colnames(testdata)[39] <- "use_work_l3"
colnames(testdata)[40] <- "selftalk_l1"
colnames(testdata)[41] <- "selftalk_l2"
colnames(testdata)[42] <- "selftalk_l3"
colnames(testdata)[43] <- "count_l1"
colnames(testdata)[44] <- "count_l2"
colnames(testdata)[45] <- "count_l3"
colnames(testdata)[46] <- "speak_l1"
colnames(testdata)[47] <- "speak_l2"
colnames(testdata)[48] <- "speak_l3"
colnames(testdata)[49] <- "understand_l1"
colnames(testdata)[50] <- "understand_l2"
colnames(testdata)[51] <- "understand_l3"
colnames(testdata)[52] <- "read_l1"
colnames(testdata)[53] <- "read_l2"
colnames(testdata)[54] <- "read_l3"
colnames(testdata)[55] <- "write_l1"
colnames(testdata)[56] <- "write_l2"
colnames(testdata)[57] <- "write_l3"
colnames(testdata)[58] <- "feel_l1"
colnames(testdata)[59] <- "feel_l2"
colnames(testdata)[60] <- "feel_l3"
colnames(testdata)[61] <- "culture_l1"
colnames(testdata)[62] <- "culture_l2"
colnames(testdata)[63] <- "culture_l3"
colnames(testdata)[64] <- "use_native_l1"
colnames(testdata)[65] <- "use_native_l2"
colnames(testdata)[66] <- "use_native_l3"
colnames(testdata)[67] <- "others_perc_l1"
colnames(testdata)[68] <- "others_perc_l2"
colnames(testdata)[69] <- "others_perc_l3"
colnames(testdata)[70] <- "answer_time_ms"

colnames(testdata) # read column/variable names
# colnames(testdata) <- gsub("m.*\\.", "", colnames(testdata)) 

# How many subjects?
nrow(testdata) # 21 subjects

testdata$subject
str(testdata)
#testdata$subject <- as.character(testdata$subject)
#testdata$subject[testdata$subject == 963]
#testdata$subject[testdata$subject == '963'] <- '00963'
#testdata$subject[testdata$subject == '5686'] <- '05686'
#testdata$subject[testdata$subject == '8224'] <- '08224'

library(tidyr)
# remove Ss if L1 or L2 is not declared
#testdata <- testdata %>% drop_na(l1) # remove Ss if l1 not declared
#testdata <- testdata %>% drop_na(l2) # remove Ss if l2 not declared

rowhold = nrow(testdata)
rowhold

# RECODE RESPONSES ----
colnames(testdata)
# coding of classifiers - gender/sex
testdata$sex[testdata$sex == "1"] <- "male"
testdata$sex[testdata$sex == "2"] <- "female"
testdata$sex[testdata$sex == "3"] <- "non-binary" # (non-binary)
testdata$sex[testdata$sex == "4"] <- "other"
testdata$sex[testdata$sex == "5"] <- "non_answered" # (prefer not to answer)

# coding of classifiers - education
testdata$degree[testdata$edu == "1"] <- "no_high_school"
testdata$degree[testdata$edu == "2"] <- "high_school"
testdata$degree[testdata$edu == "3"] <- "some_college"
testdata$degree[testdata$edu == "4"] <- "college"
testdata$degree[testdata$edu == "5"] <- "some_graduate_school"
testdata$degree[testdata$edu == "6"] <- "masters"
testdata$degree[testdata$edu == "7"] <- "terminal_degree"


# coding of classifiers - L1 languages based on Malaysia ----
testdata$l1[testdata$l1 == "1"] <- "Malay" #  (Bahasa Malaysia / Bahasa Melayu)
testdata$l1[testdata$l1 == "2"] <- "English"
testdata$l1[testdata$l1 == "3"] <- "Mandarin"
testdata$l1[testdata$l1 == "4"] <- "Cantonese"
testdata$l1[testdata$l1 == "5"] <- "Hokkien"
testdata$l1[testdata$l1 == "6"] <- "Hakka"
testdata$l1[testdata$l1 == "7"] <- "Teochew"
testdata$l1[testdata$l1 == "8"] <- "Hainanese"
testdata$l1[testdata$l1 == "9"] <- "Tamil"
testdata$l1[testdata$l1 == "10"] <- "Iban (Sarawak)"
testdata$l1[testdata$l1 == "11"] <- "Bidayuh (Sarawak)"
testdata$l1[testdata$l1 == "12"] <- "Kadazan-Dusun (Sabah)"
testdata$l1[testdata$l1 == "13"] <- "Bajau (Sabah)"
testdata$l1[testdata$l1 == "14"] <- "Orang Asli languages"
testdata$l1[testdata$l1 == "15"] <- "Other"

for (i in 1: rowhold)
  {testdata$l1[i][testdata$l1[i] == "13"] <- testdata$other_l1[i]
}

# coding of classifiers - L2 languages ----
testdata$l2[testdata$l2 == "1"] <- "Malay" #  (Bahasa Malaysia / Bahasa Melayu)
testdata$l2[testdata$l2 == "2"] <- "English"
testdata$l2[testdata$l2 == "3"] <- "Mandarin"
testdata$l2[testdata$l2 == "4"] <- "Cantonese"
testdata$l2[testdata$l2 == "5"] <- "Hokkien"
testdata$l2[testdata$l2 == "6"] <- "Hakka"
testdata$l2[testdata$l2 == "7"] <- "Teochew"
testdata$l2[testdata$l2 == "8"] <- "Hainanese"
testdata$l2[testdata$l2 == "9"] <- "Tamil"
testdata$l2[testdata$l2 == "10"] <- "Iban (Sarawak)"
testdata$l2[testdata$l2 == "11"] <- "Bidayuh (Sarawak)"
testdata$l2[testdata$l2 == "12"] <- "Kadazan-Dusun (Sabah)"
testdata$l2[testdata$l2 == "13"] <- "Bajau (Sabah)"
testdata$l2[testdata$l2 == "14"] <- "Orang Asli languages"
testdata$l2[testdata$l2 == "15"] <- "Other"

for (i in 1: rowhold)
{testdata$l2[i][testdata$l2[i] == "13"] <- testdata$other_l2[i]
}

# coding of classifiers - L3 languages -----
# For more standardisation we can 
# use three-letter ISO language codes (Set 2)
# https://en.wikipedia.org/wiki/List_of_ISO_639_language_codes

testdata$l3[testdata$l3 == "2"] <- "Malay" #  (Bahasa Malaysia / Bahasa Melayu)
testdata$l3[testdata$l3 == "3"] <- "English"
testdata$l3[testdata$l3 == "4"] <- "Mandarin"
testdata$l3[testdata$l3 == "5"] <- "Cantonese"
testdata$l3[testdata$l3 == "6"] <- "Hokkien"
testdata$l3[testdata$l3 == "7"] <- "Hakka"
testdata$l3[testdata$l3 == "8"] <- "Teochew"
testdata$l3[testdata$l3 == "9"] <- "Hainanese"
testdata$l3[testdata$l3 == "10"] <- "Tamil"
testdata$l3[testdata$l3 == "11"] <- "Iban (Sarawak)"
testdata$l3[testdata$l3 == "12"] <- "Bidayuh (Sarawak)"
testdata$l3[testdata$l3 == "13"] <- "Kadazan-Dusun (Sabah)"
testdata$l3[testdata$l3 == "14"] <- "Bajau (Sabah)"
testdata$l3[testdata$l3 == "15"] <- "Orang Asli languages"
testdata$l3[testdata$l3 == "16"] <- "Other"



for (i in 1: rowhold)
{testdata$l3[i][testdata$l3[i] == "13"] <- testdata$other_l3[i]
}

# Ad-hoc recoding of specific L3 languages
testdata$l3[testdata$l3 == "Not Applicable"] <- NA
testdata$l3[testdata$other_l3 == "Not Applicable"] <- NA
#testdata$l3[testdata$other_l3 == "Finsk / Finnish"] <- "fin"
#testdata$l3[testdata$other_l3 == "Arabic (Algerian dialect)"] <- "ara"
#testdata$l3[testdata$other_l3 == "Tertiær: fransk, femte: italiensk"] <- "fre"
#testdata$l3[testdata$other_l3 == "Polsk"] <- "pol"
#testdata$l3[testdata$other_l3 == "Kirundi"] <- "run"

#View(testdata)
#Age start learning - flip values ----
testdata$years_known_l1 <- testdata$age - testdata$start_learn_l1
testdata$years_known_l2 <- testdata$age - testdata$start_learn_l2
testdata$years_known_l3 <- testdata$age - testdata$start_learn_l3

#Comfort Recode ----
testdata$comfort_l1 <- testdata$age - testdata$comfort_l1
testdata$comfort_l2 <- testdata$age - testdata$comfort_l2
testdata$comfort_l3 <- testdata$age - testdata$comfort_l3
str(testdata)

testdata <- testdata %>%
  mutate_if(is.numeric, ~replace_na(., 0))

write.csv(testdata, file = "testdata.csv", row.names = F)
glimpse(testdata)
levels(testdata$subject)
View(testdata)
