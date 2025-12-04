##Period of Diagnosis and Stage at Presentation in Head and Neck Cancers:A SEER-Based Cross-Sectional Study Examining Effect Modification by Race

#ADA Assignment

## Project Description
This repository contains the dataset and R code used to analyze temporal trends in stage at diagnosis among patients with head and neck cancer (HNC) using SEER 17 Registries data (2004–2021).
The purpose of this project is to evaluate whether later diagnostic periods are associated with higher odds of late-stage presentation and to assess whether this association differs across racial groups.
This analysis was completed as part of an applied epidemiology and data analysis assignment using R.

#Files Included
HNC1_Kobby.txt – Dataset used for the project
HNC_analysis.R – Main R script for data cleaning, modeling, and visualization
README.md – Project documentation (this file)

###What the Code Does
The analysis code:
-Loads and cleans SEER HNC data
-Recodes key variables (period of diagnosis, race, stage at diagnosis, income, sex, age category)
-Applies inclusion/exclusion criteria
-Creates descriptive statistics and summary tables
-Runs multivariable logistic regression models
-Tests for effect modification by race
-Checks model assumptions (linearity, multicollinearity, influence diagnostics, AUC)
-Produces plots and predicted probability figures for interpretation

##How to Run the Code
-Download or clone this repository.
-Open HNC_analysis.R in RStudio.
-Ensure that the necessary packages are installed (tidyverse, table1, car, ResourceSelection, pROC, etc.).
-Set your working directory to the project folder.
-Run the script from top to bottom.


Author
Name: Kwabena Boateng
Course: Advanced Data Analysis / Epidemiology
Date: December 2025