---
title: "Generate imputed data to use when creating IPW for genteic testing survey"
output: 
  html_document:
    toc: true
    keep_md: true
---





```r
library(tidyverse)
library(survey)
library(plyr)
library(dplyr)
library(factoextra)
library(labelled)
library(tableone)
library(memisc)
library(sjlabelled)
library(mi)
library(boot)
```

# Set up working directory and packages


```r
base_path <- "~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20221003_data_code/"
```


# Overview
This code is to impute variables which will then be used in generating IPW for the genetic testing survey. 
Only a subset of HCHS/SOL individuals who participated in visit 1, participated 
in the genetic testing survey. We cannot use visit 2 weights, because some of the
people who participated in the genetic testing survey, did not participate in visit 2.
Therefore, we estimate the probability of participating in the genetic testing survey 
as follows: 
1. Estimate the probability of participating in the annual follow up phone call (AFU)
of 2019-2020, given that as individual participated in HCHS/SOL visit 1. 
2. Estimate the probability of responding to the genetic testing survey, given
that an individual participated in the AFU. 

There is sporadic missingness, so we first impute missing data, based on visit 1 only. For individuals who have more updated covariates from the AFU or from visit 2, we will use these more updated data when estimating the probability of responding to the genetic testing survey, and for association analysis.



# Creating dataset for imputation
First, we need to identify people who participated in the AFU.

```r
# read the genetic testing survey data: 
survey_gen <- read.csv(paste0(base_path, "/Preprocessed_data/solfu_gte_20211122.csv"))

# remove rows with data from afu_year =2 
survey_gen <- survey_gen[-which(survey_gen$AFU_YEAR==2),]
head(survey_gen)
```

```
##       ID FORM VERS AFU_YEAR OCCURRENCE GTE1 GTE2 GTE3 GTE4 GTE5 GTE1A GTE1B GTE1C GTE2A GTE2B GTE2C GTE3A GTE3B
## 2   5678  GTS    1       11          1    0    1    0    0    9     S     S     2     8     S     1     S     S
## 4   6570  GTS    1       11          1   NA   NA   NA   NA   NA     S     S     S     S     S     S     S     S
## 8  19626  GTS    1       10          1   NA   NA   NA   NA   NA     S     S     S     S     S     S     S     S
## 10 26693         1       10          1   NA   NA   NA   NA   NA                                                
## 12 26874  GTS    1        9          1   NA   NA   NA   NA   NA     S     S     S     S     S     S     S     S
## 15 38653  GTS    1       11          1   NA   NA   NA   NA   NA     S     S     S     S     S     S     S     S
##    GTE3C GTE4A GTE4B GTE4C OTEA52 AGE ageafu
## 2      1     S     S     1         53     63
## 4      S     S     S     S         40     50
## 8      S     S     S     S         60     69
## 10                                 65     NA
## 12     S     S     S     S         29     38
## 15     S     S     S     S         53     64
```
In the survey data, there are 9716 rows, corresponding to  9408 individuals. 



```r
# read covariates: 
covariates <- read.csv(paste0(base_path,"Preprocessed_data/solfu_covariates_20221003.csv"))
head(covariates)
```

```
##      ID STRAT PSU_ID WEIGHT_FINAL_NORM_OVERALL CENTER AGE RACE US_BORN GENDER   CLINDATE US_NATIVE
## 1  5678    20  20383                 0.2240973      S  53    9       0      F 10/16/2008         0
## 2  6570    12  12143                 0.4421305      C  40    9       0      F 12/03/2008         0
## 3 11150    21  21540                 0.4777536      M  55    5       0      F 06/09/2011         0
## 4 11243    26  26366                 2.6408307      S  44    6       0      M 10/15/2010         0
## 5 19626    14  14587                 0.7607269      M  60    5       0      F 03/04/2010         0
## 6 26693    17  17010                 0.4979705      B  65    9       0      F 03/14/2009         0
##   MARITAL_STATUS INCOME_C5 EMPLOYED BKGRD1_C6 LANG_PREF N_HC BKGRD1_C7 EDUCATION_C3 PIEA3 PIEA21B PIEA21C
## 1              2         2        2         3         1    0         3            1     2       S       S
## 2              2         2        2         3         1    1         3            1     2       S       S
## 3              3         3        3         1         1    0         1            3     4       1       0
## 4              3         3        2         3         2    0         3            3     4       1       0
## 5              3        NA        2         2         1    1         2            1     5       S       S
## 6              3        NA        2         0         1    1         0            1     3       S       S
##   PIEA21D PIEA21E PIEA21F PIEA21G ECEA4 HCEA5 HCEB5 WEIGHT_NORM_OVERALL_V2 INCOME_C5_V2 AGE_V2 EDUCATION_C3_V2
## 1       S       S       S       S     S     4                    0.2299544            2     60               1
## 2       S       S       S       S     S     2                           NA           NA     NA              NA
## 3       0       0       0       0     S          13                     NA           NA     NA              NA
## 4       0       0       0       0     1           0                     NA           NA     NA              NA
## 5       S       S       S       S     Q     2                    0.5774276            1     66               1
## 6       S       S       S       S     Q     5                           NA           NA     NA              NA
##   N_HC_V2 EMPLOYED_V2 NATIVITY_SUBSCORE_MESA_V2 HCE1 HCE2a HCE2b HCE2c HCE2d HCE2e HCE2f HCE2g HCE2h HCE2i
## 1       1           2                         2    1     1     0     0     0     0     1     1     1     0
## 2      NA          NA                        NA                                                           
## 3      NA          NA                        NA                                                           
## 4      NA          NA                        NA                                                           
## 5       1           2                         0    1     1     0     1     0     1     1     1     0     0
## 6      NA          NA                        NA                                                           
##   HCE2i1 HCE2j HCE2k HCE9 HCE31
## 1      ~     S     S    1     1
## 2                              
## 3                              
## 4                              
## 5      ~     S     S    1     1
## 6
```

```r
# create a variable AFU_par that is equal to 1 if a person participated in the AFU:
covariates$AFU_par <- ifelse(is.element(covariates$ID, survey_gen$ID), 1, 0 )
```

Of 16415 HCHS/SOL participants, 9408 participated in the AFU, and 7007 did not. 


# Prepare variable "physician_visits" 
In the dataset, there are two forms of "HCE5" (how many times saw a physician in the last 12 months): "HCEA5" and "HCEB5" that record different participants. We will combine two of them and refill the missing in HCEA5 with the value of HCEB5 with the same ID.
Afterwards, we re-level the HCEA5 and sort them into 3 groups: 
(1)If HCEA5==0, then it is "No".
(2)If HCEA5 ==1 or 2, then it is "One or Two times". 
(3)If HCEA5>=3, it is "At least three times".

```r
covariates$HCEA5[covariates$HCEA5==""] <- NA
covariates$HCEB5[covariates$HCEB5==""] <- NA

covariates$HCEA5 <- ifelse(is.na(covariates$HCEA5)==TRUE,covariates$HCEB5,covariates$HCEA5)
names(covariates)[names(covariates)=="HCEB5"] <- "HCE5"

covariates$HCEA5 <- as.numeric(covariates$HCEA5)
```

```
## Warning: NAs introduced by coercion
```

```r
covariates$physician_visits <- ifelse(covariates$HCEA5>=3,2,covariates$HCEA5)
covariates$physician_visits <- ifelse(covariates$HCEA5==2,1,covariates$physician_visits)

# sum(is.na(covariates$HCE5))
```

# Prepare education variable
Following paper review, we are breaking up the education variable to be more granular and instead of "more than high school education" we will have "Associate, vocational, and bachelore degree", and "Masters, doctoral, or professional degree". 


```r
# create a new variable called "education 
covariates$Education_C4 <- covariates$EDUCATION_C3
inds_higher_edu <- which(covariates$EDUCATION_C3 == 3 & 
                                (covariates$PIEA21E == 1 | 
                                   covariates$PIEA21F == 1 | 
                                   covariates$PIEA21G == 1)) 
covariates$Education_C4[inds_higher_edu] <- 4
```




# Categorize variables and create factors
In this section, we factorize all variables for imputation and logistic regression and define levels for all variables to be used. We re-defined levels of two variables: "Employment_status" and "age". 


```r
covariates_1 <- covariates

covariates_1 <- covariates_1 %>%
  mutate(
     SEX=factor(GENDER,levels = c("F","M"),labels=c("Female","Male")),
    Education=factor(Education_C4,levels = c("1","2","3", "4"),labels=c("<12","12",">12", "Masters, doctoral, professional")),
    CENTER=factor(CENTER,levels = c("B","C","M","S"),labels=c("Bronx","Chicago", "Miami", "San Diego")),
    Income_level=factor(INCOME_C5,levels = c("1","2","3","4", "5"),labels = c("Less than $10,000","$10,001-$20,000","$20,001-$40,000","$40,001-$75,000","More than $75,000")),
    Current_Health_insurance=factor(N_HC,levels = c("0","1"),labels = c("No","Yes")),
    Background=factor(BKGRD1_C7,levels = c("0","1","2","3","4", "5","6"),labels = c("Domician","Central American","Cuban","Mexican","Puerto Rican","South American","More than one/Other heritage")),
    Language_pref = factor(LANG_PREF, levels = c("1", "2"), labels = c("Spanish", "English")),
    Marital_status=factor(MARITAL_STATUS,levels=c("1","2","3"),labels = c(
      "Single","Married or living with a partner","Separated,divorced,or widow(er)")),
    Employment_status=factor(EMPLOYED, levels=c("1","2","3","4"),labels = c("Retired/not currently employed","Not retired and not currently employed","Employed part-time(<=35 hours/week)","Employed full-time(>35 hours/week)")),
    US_BORN=factor(US_BORN, levels=c("0","1"),labels = c("No","Yes")),
    Physician_Visit=factor(physician_visits,levels = c("0","1","2"), labels = c("No","One or two times","At least three times"))
  )



covariates_1$Employment_status <- relabel(
  covariates_1$Employment_status,
  "Retired/not currently employed" ="Retired/not currently employed",
  "Not retired and not currently employed"="Retired/not currently employed"
)
```

```
## Warning in relabel.factor(covariates_1$Employment_status, `Retired/not currently employed` = "Retired/not
## currently employed", : Duplicate labels
```

```r
covariates_1$age_cat <- case_when(covariates_1$AGE <= 25 ~ '<25',
                              between(covariates_1$AGE, 26,40) ~ '26-40',
                              between(covariates_1$AGE, 41,60) ~ '41-60',
                            covariates_1$AGE >= 61 ~ '61<')

covariates_1$age_cat <- as.factor(covariates_1$age_cat)
saveRDS(covariates_1,paste0(base_path,"Processed_data/organized_visit1_covariates.RData"))
```

The levels of "Employment_status" is Retired/not currently employed, Employed part-time(<=35 hours/week), Employed full-time(>35 hours/week). The levels of "age_cat" are <25, 26-40, 41-60, 61<



# Performing Imputation
First, we extract the variables to be applied in the imputation and then create a data matrix. We repeat the process 5 times. 

```r
set.seed(800)
predictors <- c("age_cat",
                             "SEX",
                             "Education",
                             "CENTER",
                             "Income_level",
                             "Current_Health_insurance",
                             "Physician_Visit",
                             "Background", 
                             "Language_pref", 
                             "Marital_status",
                             "Employment_status", 
                             "US_BORN",
                             "STRAT")
  
imputedat <- covariates_1[,c("ID", predictors)]
cl <- parallel::makeCluster(5, setup_strategy = "sequential")
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

mdf <- missing_data.frame(imputedat)
mdf1 <- mi(mdf)
imputed_data <- as.data.frame(complete(mdf1,m=5))
saveRDS(imputed_data,paste0(base_path,"Processed_data/visit1_imputed_data.RData"))
```




