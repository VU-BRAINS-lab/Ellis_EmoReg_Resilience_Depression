#Download libraries
library(dplyr)

#Variables to include and their variable names/codings

# Age -> age

# Sex -> gender
# 1 = male
# 2 = female
# 4 = other

# Race -> race
#1=American Indian/Alaskan Native
#2=Asian
#3=Native Hawaiian or Other Pacific Islander
#4=Black or African American
#5=White
#6=More than One Race
#7=Other

# Ethnicity -> ethnicity
#1=Hispanic or Latino
#2=NOT Hispanic or Latino

# Education -> education 
#0=Currently enrolled in elementary school
#1=Currently enrolled in middle/junior high school
#2=Currently enrolled in high school
#3=Didn't finish high school
#4=Didn't finish high school, but completed a technical/ vocational program
#5=High school graduate or GED (General Education Diploma)
#6=Completed high school and a technical/vocational program
#7=Less than 2 years of college
#8=2 years of college or more/ including associate degree or equivalent
#9=College graduate (4 or 5 year program)
#10=Master's degree (or other post-graduate training)
#11=Doctoral degree (PhD, MD, EdD, DVM, DDS, JD, etc)

# Income -> household_income
#0=$31,000 or less (Lowest income)
#1=$31,001 - $42,000 (Lower-middle income)
#2=$42,001 - $126,000 (Middle-income)
#3=$126,001 - $188,000 (Upper-middle income)
#4=$188,001 or more (Higher-income)

#Read in data
survey <- read.csv("~/Desktop/Survey_cleaned.csv", header=TRUE) 

#See what variables you have
names(survey)

#Trim to just variables needed
EmoData <- survey[c(grep("record_id|^age$|^gender$|^race$|ethnicity|^education$|household_income|bdi_total|brs_raw_score|erq_cog_total|erq_expressive_total|ders_sf_strategies_total|ders_sf_nonacceptance_tot|ders_sf_impulse_total|ders_sf_goals_total|ders_sf_awareness_total|ders_sf_clarity_total",names(survey)))]

#Check that it worked
names(EmoData)

#Trim to those 12-27 years of age since there are few subjects over 27 in this data
EmoData<-EmoData[!(EmoData$age > 27),]

#Remove those missing data on any of the variables of interest
EmoData <- na.omit(EmoData)

#Final sample size
nrow(EmoData)

#Save final dataset
saveRDS(EmoData,"~/Desktop/EmoReg_BRS_BDI.rds")

#Calculating descriptives on continuous data (age)
mean(EmoData$age, na.rm=TRUE)
sd(EmoData$age, na.rm=TRUE)
range(EmoData$age, na.rm=TRUE)

#Calculating descriptives on continuous variables 
mean(EmoData$brs_raw_score)
sd(EmoData$brs_raw_score)

mean(EmoData$bdi_total)
sd(EmoData$bdi_total)

mean(EmoData$ders_sf_strategies_total)
sd(EmoData$ders_sf_strategies_total)

mean(EmoData$ders_sf_nonacceptance_tot)
sd(EmoData$ders_sf_nonacceptance_tot)

mean(EmoData$ders_sf_impulse_total)
sd(EmoData$ders_sf_impulse_total)

mean(EmoData$ders_sf_goals_total)
sd(EmoData$ders_sf_goals_total)

mean(EmoData$ders_sf_awareness_total)
sd(EmoData$ders_sf_awareness_total)

mean(EmoData$ders_sf_clarity_total)
sd(EmoData$ders_sf_clarity_total)

mean(EmoData$erq_cog_total)
sd(EmoData$erq_cog_total)

mean(EmoData$erq_expressive_total)
sd(EmoData$erq_expressive_total)

#Calculating descriptives on categorical variables
count(EmoData,gender) 
count(EmoData,race)
count(EmoData,ethnicity)
count(EmoData,education)
count(EmoData,household_income)

### To get percentages use this formula: Percentage = (sample size in one category / total sample size) * 100
#Gender: Female
(1010/nrow(EmoData)) * 100
#Gender: Male
(412/nrow(EmoData)) * 100
#Gender: Other
(1/nrow(EmoData)) * 100

#Race: White
(791/nrow(EmoData)) * 100 
#Race: American Indian/Native Alaskan
(6/nrow(EmoData)) * 100
#Race: Asian
(353/nrow(EmoData)) * 100
#Race: Native Hawaiian or Other Pacific Islander
(5/nrow(EmoData)) * 100
#Race: Black or African American
(162/nrow(EmoData)) * 100
#Race: More than one race
(90/nrow(EmoData)) * 100
#Race: Other
(16/nrow(EmoData)) * 100

#Ethnicity: Hispanic or Latino
(158/nrow(EmoData)) * 100
#Ethnicity: Not Hispanic or Latino
(1265/nrow(EmoData)) * 100

#Income: $31000 or less
(93/nrow(EmoData)) * 100
#Income: $31,001-$42,000
(108/nrow(EmoData)) * 100
#Income: $42,001-$126,000
(465/nrow(EmoData)) * 100
#Income: $126,001-$188,000
(279/nrow(EmoData)) * 100
#Income: $188,001 or more
(478/nrow(EmoData)) * 100

#Education: 1
(14/nrow(EmoData)) * 100
#Education 2
(38/nrow(EmoData)) * 100
#Education 4
(2/nrow(EmoData)) * 100
#Education 5
(185/nrow(EmoData)) * 100
#Education 6
(11/nrow(EmoData)) * 100
#Education 7
(681/nrow(EmoData)) * 100
#Education 8
(233/nrow(EmoData)) * 100
#Education 9
(227/nrow(EmoData)) * 100
#Education 10
(22/nrow(EmoData)) * 100
#Education 11
(9/nrow(EmoData)) * 100
