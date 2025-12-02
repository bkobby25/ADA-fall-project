#ADA project

pacman::p_load(ggplot2, car, odds.n.ends, readr, dplyr, tidyr, odds.n.ends, 
blorr, lmtest, broom, tidyverse, jtools, table1, DiagrammeR, rsvg) 

#Uploading the csv file which contains the head and neck SEER data on R
HNCADA <- read.csv('/Users/Kobby/Desktop/Kobby HNC-selected/HNC1_Kobby.txt', header = TRUE)

#I want too find out the various variables that are in the data set and how they are named in R
colnames(HNCADA)

#Finding out how many observations are in the dataset
nrow(HNCADA)

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

table(HNCADA$Year.of.diagnosis)

#excluding observations which are not first primary malignant tumor and diagnosed before 2004
HNC_c <- HNCADA %>%
  filter(
    First.malignant.primary.indicator == "Yes",
   
     )

HNC_c2 <- HNC_c %>%
  filter(
     Year.of.diagnosis >= 2004,
      )
 
#check how many observations were excluded at each stage
exc1 <- nrow(HNCADA) - nrow(HNC_c)

exc2 <- nrow(HNC_c) - nrow(HNC_c2)

#combine the formulas
HNC_clean <- HNC_c2 %>%
  filter(    First.malignant.primary.indicator == "Yes",
     Year.of.diagnosis >= 2004,
      )

##create a figure 1

library(DiagrammeR)

fig1 <- grViz("
digraph flowchart{

  node [fontname = Helvetica, shape = rectangle, fontsize = 15]

  node1 [label = '@@1']
  node2 [label = '@@2']
  node3 [label = '@@3']

  node1 -> node2 -> node3
}

[1]: 'Persons diagnosed with HNC between 2000 - 2021 \\n n = 287,069'
[2]: 'Excluding 57,126 individuals who did not have \\n first primary malignant HNC tumor \\n n = 229,943'
[3]: 'Excluding 36,335 individuals diagnosed before 2004 \\n n = 193,608'
")

###export figure 1

fig1 %>%
  DiagrammeRsvg::export_svg() %>% 
  charToRaw() %>% 
  rsvg::rsvg_pdf("Fig1.pdf")
  
###



#recode year of diagnosis into categories
HNC_clean$period <- cut(
  HNC_clean$Year.of.diagnosis,
  breaks = c(2003, 2008, 2013, 2018, 2021),
  labels = c("2004-2008", "2009-2013", "2014-2018", "2019-2021"),
  right = TRUE
)

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


#check if recoding worked
table(HNC_clean$period)



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
table(HNC_clean$stage_bin, useNA = "ifany")

#making sure no observations were deleted
table(HNC_clean$combined_stage)


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
table(HNC_clean$stage_bin, useNA= "always")


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
table(HNC_clean$stage_bin_num, useNA= "ifany")

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

#checking for non-missing variables
sum(!is.na(HNC_clean$period_cat))


#for a complete case analysis 
names(HNC_clean)

colnames(HNC_clean)

model_vars <- c("stage_bin_num", "period_cat", "age", "Sex", "hh_income",
"race_cat")

#excluding missing values for any of the variables to be used
HNC_ex <- HNC_clean %>%
  filter(if_all(all_of(model_vars), ~ !is.na(.)))
  

#finding out how many observations are left for a complete case analysis after 
#the Unstaged/ Unknown stage was removed
nrow(HNC_ex)


#recode income variable 
HNC_ex <- HNC_ex %>%
  mutate(
    hh_income_cat = case_when(
      hh_income %in% c(
        "< $40,000",
        "$40,000 - $44,999",
        "$45,000 - $49,999",
        "$50,000 - $54,999",
        "$55,000 - $59,999"
      ) ~ "<=59k",

      hh_income %in% c(
        "$60,000 - $64,999",
        "$65,000 - $69,999",
        "$70,000 - $74,999",
        "$75,000 - $79,999"
      ) ~ "60-79k",

      hh_income %in% c(
        "$80,000 - $84,999",
        "$85,000 - $89,999",
        "$90,000 - $94,999",
        "$95,000 - $99,999"
      ) ~ "80-99k",

      hh_income %in% c(
        "$100,000 - $109,999",
        "$110,000 - $119,999",
        "$120,000+"
      ) ~ "100k+",

      TRUE ~ NA_character_
    )
  )

#checking the recoding for household income
table(HNC_ex$hh_income_cat)

# visualize period of diagnosis by stage of diagnosis using a bar graph
library(dplyr)
library(ggplot2)


  
  

#Making period a factor variable 
HNC_ex$period_cat <- factor(HNC_ex$period_cat,
                            levels = 0:3,
                            labels = c("2004-2008","2009-2013","2014-2018",
                            "2019-2021"))



#bar graph with period of diagnosis

ggplot(HNC_ex, aes(x = period_cat)) +
  geom_bar(fill = "#2C3E50") +
  labs(
    title = "Distribution of Period of Diagnosis",
    x = "Period of Diagnosis",
    y = "Number of Cases"
  ) +
  theme_minimal(base_size = 14)

                          
#Running a univariate model with stage as dependent variable and period 
#independent variable
model <- glm(stage_bin_num ~ period_cat, 
             data = HNC_ex, 
             family = binomial)

summary(model)
library(broom)
tidy(model, exponentiate = TRUE, conf.int = TRUE)

odds.n.ends(model)

#To check for the linearity assumption the dependent variable should be classified as 
#0 for early and 1 for late under a new variable stage_num
HNC_ex$stage_num <- ifelse(HNC_ex$stage_bin_num == "Late", 1, 0)

HNC_ex$stage_num <- ifelse(HNC_ex$stage_bin == "Late", 1,
                      ifelse(HNC_ex$stage_bin == "Early", 0, NA))
table(HNC_ex$stage_num, useNA = "ifany")

#convert age from character variable to numerical under a new variable 
HNC_ex$age_num <- as.numeric(gsub(" years", "", HNC_ex$age))

#convert all 90+ to numeric 90
HNC_ex$age_num <- gsub(" years", "", HNC_ex$age)        
HNC_ex$age_num <- gsub("\\+", "", HNC_ex$age_num)       
HNC_ex$age_num <- as.numeric(HNC_ex$age_num)            

#check if conversion worked
summary(HNC_ex$age_num)
table(is.na(HNC_ex$age_num))


#load car package
library(car)

#run a box tidwell for age 
#check the age variable

summary(HNC_ex$age_num)
sum(is.na(HNC_ex$age_num))



#create a term to test for linearity for age
HNC_ex <- HNC_ex %>%
  mutate(age_num.times.logage_num = age_num * log(age_num)) 

mod2 <- glm(stage_num ~ age_num + age_num.times.logage_num, data=HNC_ex, family="binomial") 

summary(mod2)


#age variable violates linearity assumption so we need to recode it into meaningful categories
HNC_ex <- HNC_ex %>%
  mutate(
    age_cat = case_when(
      age_num < 50 ~ "<50",         
      age_num >= 50 & age_num < 60 ~ "50-59",
      age_num >= 60 & age_num < 70 ~ "60-69",
      age_num >= 70 & age_num < 80 ~ "70-79",
      age_num >= 80 ~ "80+",
      TRUE ~ NA_character_
    )
  )


#Checking the income variable
table(HNC_ex$hh_income_cat)


#multivariable model with age categories, sex and income categories

HNC_ex$age_cat         <- factor(HNC_ex$age_cat,
                                 levels = c("<50","50-59","60-69","70-79","80+"),
                                 ordered = FALSE)

HNC_ex$hh_income_cat   <- factor(HNC_ex$hh_income_cat,
                                 levels = c("<=59k","60-79k","80-99k","100k+"))

HNC_ex$Sex             <- factor(HNC_ex$Sex)  

table(HNC_ex$age_cat)

#checking how many variables are missing 
colSums(is.na(HNC_ex[, c("stage_bin_num", "period_cat", "age_cat", "Sex", "hh_income_cat")]))


#make a table 1
label(HNC_ex$age_cat) <- "Age Categories (years)"
label(HNC_ex$Sex) <- "Sex"
label(HNC_ex$race_cat) <- "Race"
label(HNC_ex$stage_bin_num) <- "Stage at diagnosis"
label(HNC_ex$period_cat) <- "Period of diagnosis"
label(HNC_ex$hh_income_cat) <- "Adjusted Household Income"

table1(~ age_cat + Sex + race_cat  + stage_bin_num  + hh_income_cat + period_cat , HNC_ex)



#running a multivariable model
mod_full <- glm(
  stage_num ~ period_cat + age_cat + Sex + hh_income_cat,
  data = HNC_ex,
  family = binomial)

library(broom)
tidy(mod_full, exponentiate = TRUE, conf.int = TRUE)

odds.n.ends(mod_full)

str(HNC_ex$race_cat)

#recoding race variable 
#HNC_ex$race_cat <- case_when(
#HNC_ex$race == "Non-Hispanic White" ~ "NHW",
#HNC_ex$race == "Non-Hispanic Black" ~ "NHB",
# HNC_ex$race == "Hispanic (All races)" ~ "Hispanic",
#  TRUE ~ "Other")

table(HNC_ex$race_cat)

#recode race as factor variable 
HNC_ex$race_cat <- factor(HNC_ex$race_cat,
                          levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other"))



#model without interaction 
mod_noint <- glm(
  stage_num ~ period_cat + race_cat + age_cat + Sex + hh_income_cat,
  data   = HNC_ex,
  family = binomial)

mod_int <- glm(
  stage_num ~ period_cat * race_cat + age_cat + Sex + hh_income_cat,
  data   = HNC_ex,
  family = binomial)

#testing if the period * race interaction is signficant 
anova(mod_noint, mod_int, test = "LRT")

install.packages("emmeans")
library(emmeans)


#predicted probability for every period 
em_period_race <- emmeans(mod_int2, ~ period_cat | race_cat, type = "response")
em_period_race


#hosmer lemeshow test
blr_test_hosmer_lemeshow(mod_noint)

#checking multicollinearity
vif(mod_int2)

## 1) Cook's distance for each observation
cooks <- cooks.distance(mod_int2)

# Quick summary
summary(cooks)

#sample size used in the model
n <- length(cooks)     
n

#cutoff being set
cutoff_4n <- 4 / n
cutoff_4n

#observations above this cutoff
which(cooks > cutoff_4n)


## 3) Put diagnostics into a tidy data frame
mod_diag <- augment(mod_int2) %>%
  mutate(
    index  = dplyr::row_number(),
    cooksd = .cooksd
  )

# Check the top 10 most influential observations
mod_diag %>%
  arrange(desc(cooksd)) %>%
  slice(1:10)
  
# Proportion of observations above 4/n
mean(mod_diag$cooksd > cutoff_4n)

#conducting a sensitivity analysis reclassifying unknown / unstaged as Late
HNC_sens1 <- HNC_clean %>%
  mutate(
    stage_sens = case_when(
      combined_stage %in% c("Localized", "In situ") ~ "Early",
      combined_stage %in% c("Regional", "Distant") ~ "Late",
      TRUE ~ "Late"   
    )
  )
  
  
#convering to a numeric outcome 
HNC_sens1$stage_sens_num <- ifelse(HNC_sens1$stage_sens == "Late", 1, 0)

#creating the age_cat variable 
HNC_sens1$age_num <- as.numeric(gsub(" years", "", HNC_sens1$age))

HNC_sens1 <- HNC_ex %>%
  mutate(
    # reclassify unstaged / anything not clearly Early as Late
    stage_sens = case_when(
      combined_stage %in% c("Localized", "In situ") ~ "Early",
      combined_stage %in% c("Regional", "Distant") ~ "Late",
      TRUE ~ "Late"   # unknown / unstaged → Late
    ),
    stage_sens_num = ifelse(stage_sens == "Late", 1, 0)
  ) %>%
  filter(
    !is.na(stage_sens_num),
    !is.na(period_cat),
    !is.na(age_cat),
    !is.na(Sex),
    !is.na(hh_income_cat),
    !is.na(race_cat)
  )



#sensitivity model 
mod_sens1 <- glm(
  stage_sens_num ~ period_cat + age_cat + Sex + hh_income_cat + race_cat,
  data   = HNC_sens1,
  family = "binomial")

library(broom)
tidy(mod_sens1, exponentiate = TRUE, conf.int = TRUE)


#checking ROC and AUC
install.packages("pROC")
library(pROC)

#AUC for first model
pred_prob <- fitted(mod_noint)
roc_obj <- roc(response = mod_noint$y, predictor = pred_prob)
roc_obj
auc(roc_obj)


###AUC for second model 
pred_prob_int <- fitted(mod_int2)
roc_int <- roc(response = mod_int2$y, predictor = pred_prob_int)
auc(roc_int)


##graph predicted probabilities 
em_period_race <- emmeans(mod_int2, ~ period_cat | race_cat, type = "response")

# Convert to dataframe
em_df <- as.data.frame(em_period_race)
head(em_df)


##plot of predicted probabilities 
ggplot(em_df, aes(x = period_cat, y = prob, color = race_cat, group = race_cat)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = race_cat),
              alpha = 0.15, color = NA) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Adjusted Predicted Probability of Late-Stage Diagnosis",
    subtitle = "By Diagnosis Period and Race/Ethnicity",
    x = "Diagnosis Period",
    y = "Predicted Probability (Late Stage)",
    color = "Race/Ethnicity",
    fill = "Race/Ethnicity"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
