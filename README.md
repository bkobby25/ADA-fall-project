#ADA project
library(dplyr)

#Uploading the csv file which contains the head and neck SEER data on R
HNCADA <- read.csv('/Users/Kobby/Desktop/Kobby HNC-selected/HNC1_Kobby.txt', header = TRUE)

#I want too find out the various variables that are in the data set and how they are named in R
colnames(HNCADA)

#finding the frequency of the combined  summary stage 
table(HNCADA$Combined.Summary.Stage..2004..)

#finding the frequency of year of diagnosis variable
table(HNCADA$Year.of.diagnosis)

#cross tabs between the stage of diagnosis and year of diagnosis
table(HNCADA$Year.of.diagnosis, HNCADA$Summary.stage.2000..1998.2017.)

#cross tabs between stage of diagnosis and year of diagnosis using Combined summary stage
table(HNCADA$Year.of.diagnosis, HNCADA$Combined.Summary.Stage..2004..)

#frequency of first malignant primary indicator
table(HNCADA$First.malignant.primary.indicator)

#excluding observations which are not first primary malignant tumor and diagnosed before 2004
HNC_clean <- HNCADA %>%
  filter(
    First.malignant.primary.indicator == "Yes",
    Year.of.diagnosis >= 2004
  )
  

#recode year of diagnosis into categories
HNC_clean$period <- cut(
  HNC_clean$Year.of.diagnosis,
  breaks = c(2003, 2008, 2013, 2018, 2021),
  labels = c("2004-2008", "2009-2013", "2014-2018", "2019-2021"),
  right = TRUE
)

#check if recoding worked
table(HNC_clean$period)

colnames(HNC_clean)  

#frequency of the head and neck sites
table(HNC_clean$Site.recode.ICD.O.3.WHO.2008)

#recoding stage variable into 2 (early and late)
HNC_clean <- HNC_clean %>%
  mutate(
    stage_bin = case_when(
      combined_stage %in% c("Localized", "In situ") ~ "Early",
      combined_stage %in% c("Regional", "Distant") ~ "Late",
      TRUE ~ NA_character_
    )
  )

#checking to see if recoding was successful 
table(HNC_clean$Stage_binary, useNA = "ifany")

#making sure no observations were deleted
table(HNC_clean$Combined.Summary.Stage..2004..)

#renaming the variables
HNC_clean <- HNC_clean %>%
  rename(
    diagnosis_year = Year.of.diagnosis,
    primaryHNCtumor = First.malignant.primary.indicator,
    stage = Summary.stage.2000..1998.2017.,
    site = Site.recode.ICD.O.3.WHO.2008,
    combined_stage = Combined.Summary.Stage..2004..,
    marital_status = Marital.status.at.diagnosis,
    hh_income = Median.household.income.inflation.adj.to.2022,
    age = Age.recode.with.single.ages.and.90.,
    race = Race.and.origin.recode..NHW..NHB..NHAIAN..NHAPI..Hispanic.,
  )

#checking the column names to make sure they have been recoded corredctly 
colnames(HNC_clean)

#checking the race variable 
table(HNC_clean$race)

#recoding the race variable 
HNC_clean <- HNC_clean %>%
  mutate(
    race_cat = case_when(
      race == "Non-Hispanic White" ~ "Non-Hispanic White",
      race == "Non-Hispanic Black" ~ "Non-Hispanic Black",
      race == "Hispanic (All Races)" ~ "Hispanic",
      race %in% c("Non-Hispanic Asian or Pacific Islander",
                  "Non-Hispanic American Indian/Alaska Native",
                  "Non-Hispanic Unknown Race") ~ "Other",
      TRUE ~ NA_character_
    )
  )
  
#confirming the recoding
table(HNC_clean$race_cat)

#check married status varibale 
table(HNC_clean$marital_status)

#recode marital status variabe 
HNC_clean <- HNC_clean %>%
  mutate(
    married_bin = case_when(
      marital_status == "Married (including common law)" ~ "Married",
      TRUE ~ "Not married"
    )
  )

#checking the stage variable 
table(HNC_clean$stage_bin)


#recoding the stage where early is 0 and late is 1
HNC_clean <- HNC_clean %>%
  mutate(
    stage_bin_num = case_when(
      stage_bin == "Early" ~ 0,
      stage_bin == "Late"  ~ 1,
      TRUE ~ NA_real_
    )
  )

#confirming the code  
table(HNC_clean$stage_bin, useNA= "ifany")

#make stage a factor variale
HNC_clean$stage_bin_num <- factor(HNC_clean$stage_bin_num, levels = c(0,1), labels = c("Early", "Late"))

#check classification
table(HNC_clean$stage_bin_num, HNC_clean$stage_bin, useNA = "always")


#recoding the period of diagnosis wherre 0 = 2004-2008, 1 = 2009-2013, 
#2= 2014-2018, 3 = 2019-2021
table(HNC_clean$period)

HNC_clean <- HNC_clean %>%
  mutate(
    period_cat = case_when(
      period == "2004-2008" ~ 0,
      period == "2009-2013" ~ 1,
      period == "2014-2018" ~ 2,
      period == "2019-2021" ~ 3,
      TRUE ~ NA_real_
    )
  )

#checking the period categories variable created
table(HNC_clean$period_cat)

# visualize period of diagnosis by stage of diagnosis using a bar graph
      
library(dplyr)
library(ggplot2)


