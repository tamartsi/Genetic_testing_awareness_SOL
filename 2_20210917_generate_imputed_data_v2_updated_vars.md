---
title: "Generate imputed data to use when creating IPW for genetic testing survey (part 2)"
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
 base_path <- "~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20210917_data_code/"
```


# Overview
This code is to impute variables which will then be used in generating IPW for the genetic testing survey. 
Only a subset of HCHS/SOL individuals who participated in visit 1, participated 
in the genetic testing survey. We cannot use visit 2 weights, because some of the
people who participated in the genetic testing survey, did not participate in visit 2.
Therefore, we estimate the probability of participating in the genetic testing survey 
as follows: 
1. Estimate the probability of participating in the annual follow up phone call (AFU)
of 2019-2020, given that individual participated in HCHS/SOL visit 1. 
2. Estimate the probability of responding to the genetic testing survey, given
that an individual participated in the AFU. 

There is sporadic missingness. We first imputed missing data, based on visit 1 only, to use when computing probabilities of participation in the AFU. In this report we are imputing missing data after updating various covariates based on data from visit 2 (or, in the case of marital status, from the 2nd AFU), for use when computing probabilities of participating in the genetic testing survey.



# Creating dataset for imputation
First, we need to identify people who participated in the AFU.

```r
# read the genetic testing survey data: 
survey_gen <- read.csv(paste0(base_path, "/Preprocessed_data/solfu_gte_20200730.csv"))

# remove rows with data from afu_year =2 
survey_gen <- survey_gen[-which(survey_gen$AFU_YEAR==2),]
head(survey_gen)
```

```
##       ID FORM VERS AFU_YEAR OCCURRENCE GTE1 GTE2 GTE3
## 2   5678  GTS    1       11          1    0    1    0
## 4   6570  GTS    1       11          1   NA   NA   NA
## 8  19626  GTS    1       10          1   NA   NA   NA
## 10 26693         1       10          1   NA   NA   NA
## 12 26874  GTS    1        9          1   NA   NA   NA
## 15 38653  GTS    1       11          1   NA   NA   NA
##    GTE4 GTE5 GTE1A GTE1B GTE1C GTE2A GTE2B GTE2C GTE3A
## 2     0    9     S     S     2     8     S     1     S
## 4    NA   NA     S     S     S     S     S     S     S
## 8    NA   NA     S     S     S     S     S     S     S
## 10   NA   NA                                          
## 12   NA   NA     S     S     S     S     S     S     S
## 15   NA   NA     S     S     S     S     S     S     S
##    GTE3B GTE3C GTE4A GTE4B GTE4C OTEA52 AGE ageafu
## 2      S     1     S     S     1         53     63
## 4      S     S     S     S     S         40     50
## 8      S     S     S     S     S         60     69
## 10                                       65     NA
## 12     S     S     S     S     S         29     38
## 15     S     S     S     S     S         53     64
```
In the survey data, there are 9716 rows, corresponding to  9408 individuals. 



```r
# read covariates: 
covariates <- read.csv(paste0(base_path,"Preprocessed_data/solfu_covariates_20210917.csv"))
head(covariates)
```

```
##      ID STRAT PSU_ID WEIGHT_FINAL_NORM_OVERALL CENTER
## 1  5678    20  20383                 0.2240973      S
## 2  6570    12  12143                 0.4421305      C
## 3 11150    21  21540                 0.4777536      M
## 4 11243    26  26366                 2.6408307      S
## 5 19626    14  14587                 0.7607269      M
## 6 26693    17  17010                 0.4979705      B
##   AGE RACE US_BORN GENDER   CLINDATE MARITAL_STATUS
## 1  53    9       0      F 10/16/2008              2
## 2  40    9       0      F 12/03/2008              2
## 3  55    5       0      F 06/09/2011              3
## 4  44    6       0      M 10/15/2010              3
## 5  60    5       0      F 03/04/2010              3
## 6  65    9       0      F 03/14/2009              3
##   INCOME_C5 EMPLOYED BKGRD1_C6 LANG_PREF N_HC BKGRD1_C7
## 1         2        2         3         1    0         3
## 2         2        2         3         1    1         3
## 3         3        3         1         1    0         1
## 4         3        2         3         2    0         3
## 5        NA        2         2         1    1         2
## 6        NA        2         0         1    1         0
##   EDUCATION_C3 PIEA3 HCEA5 HCEB5 WEIGHT_NORM_OVERALL_V2
## 1            1     2     4                    0.2299544
## 2            1     2     2                           NA
## 3            3     4          13                     NA
## 4            3     4           0                     NA
## 5            1     5     2                    0.5774276
## 6            1     3     5                           NA
##   INCOME_C5_V2 AGE_V2 EDUCATION_C3_V2 N_HC_V2
## 1            2     60               1       1
## 2           NA     NA              NA      NA
## 3           NA     NA              NA      NA
## 4           NA     NA              NA      NA
## 5            1     66               1       1
## 6           NA     NA              NA      NA
##   EMPLOYED_V2 NATIVITY_SUBSCORE_MESA_V2 HCE1 HCE2a
## 1           2                         2    1     1
## 2          NA                        NA           
## 3          NA                        NA           
## 4          NA                        NA           
## 5           2                         0    1     1
## 6          NA                        NA           
##   HCE2b HCE2c HCE2d HCE2e HCE2f HCE2g HCE2h HCE2i
## 1     0     0     0     0     1     1     1     0
## 2                                                
## 3                                                
## 4                                                
## 5     0     1     0     1     1     1     0     0
## 6                                                
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


## Prepare variable "physician_visits" 
In the dataset, there are two forms of "HCE5" (how many times saw a physician in the last 12 month): "HCEA5" and "HCEB5" that records different participants. We will combine two of them and refill the missing in HCEA5 with the value of HCEB5 with the same ID.
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

## Update marital status variable
Marital status was available from the baseline visit and year 2 AFU. So when possible, we use the information from year 2 AFU, because it is more recent:


```r
survey_marital <- read.csv(paste0(base_path, "/Preprocessed_data/solfu_gte_20200730.csv"))
# keep rows with data from afu_year =2 
survey_marital <- survey_marital[which(survey_marital$AFU_YEAR==2),]
# re-level these to be the same as the collapsed marital_status variable:
# separated, divorced, or widower should become 3
survey_marital$OTEA52[which(is.element(survey_marital$OTEA52, c("1","2","3")))] <- 3
# single should become 1
survey_marital$OTEA52[which(survey_marital$OTEA52 == "4")] <- 1 
# married or living with a partner should become 2
survey_marital$OTEA52[which(is.element(survey_marital$OTEA52, c("0","5")) )] <- 2 
survey_marital$OTEA52[which(survey_marital$OTEA52 == "S")] <- NA
survey_marital$OTEA52[which(survey_marital$OTEA52 == "")] <- NA

covariates <-merge(survey_marital[,c("ID", "OTEA52")],covariates, by="ID", all=TRUE)
inds_updated <- which(!is.na(covariates$OTEA52))
covariates$MARITAL_STATUS[inds_updated] <- covariates$OTEA52[inds_updated]

covariates$OTEA52 <- NULL
```


## Update variables to have visit 2 data when available 
Additional variables are available from visit 2. So when possible, we use the information from visit 2, because it is more recent:


```r
# Health insurance:
inds_update <- which(!is.na(covariates$N_HC_V2))
covariates$N_HC[inds_update] <- covariates$N_HC_V2[inds_update]

# Education:
inds_update <- which(!is.na(covariates$EDUCATION_C3_V2))
covariates$EDUCATION_C3[inds_update] <- covariates$EDUCATION_C3_V2[inds_update]

# Household income:
inds_update <- which(!is.na(covariates$INCOME_C5_V2))
covariates$INCOME_C5[inds_update] <- covariates$INCOME_C5_V2[inds_update]

# Employment status:
inds_update <- which(!is.na(covariates$EMPLOYED_V2))
covariates$EMPLOYED[inds_update] <- covariates$EMPLOYED_V2[inds_update]

# Age: use age at the survey when possible
# first, reduce the data to that from the AFU

covariates_AFU <- covariates[which(covariates$AFU_par == 1),]
# in the AFU...
inds_na_ageafu <- which(is.na(survey_gen$ageafu)) 
length(inds_na_ageafu)
```

```
## [1] 856
```

```r
survey_gen$ageafu[inds_na_ageafu] <- survey_gen$AGE[inds_na_ageafu] + survey_gen$AFU_YEAR[inds_na_ageafu]

# there are still some missing values... 
inds_na_ageafu <- which(is.na(survey_gen$ageafu))
length(inds_na_ageafu)
```

```
## [1] 232
```

```r
survey_gen$ageafu[inds_na_ageafu] <- survey_gen$AGE[inds_na_ageafu] + median(survey_gen$AFU_YEAR, na.rm = TRUE)

covariates_AFU <- merge(covariates_AFU, survey_gen[-which(duplicated(survey_gen$ID)),c("ID", "ageafu")])
```


# Categorize variables and create factors
In this section, we factorize all variables for imputation and logistic regression and define levels for all variables to be used. We re-defined levels of three variables: "Race","Employment_status" and "age". 


```r
covariates_1 <- covariates_AFU

covariates_1 <- covariates_1 %>%
  mutate(
     SEX=factor(GENDER,levels = c("F","M"),labels=c("Female","Male")),
    Education=factor(EDUCATION_C3,levels = c("1","2","3"),labels=c("<12","12",">12")),
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
## Warning in
## relabel.factor(covariates_1$Employment_status, `Retired/
## not currently employed` = "Retired/not currently
## employed", : Duplicate labels
```

```r
covariates_1$age_cat <- case_when(covariates_1$ageafu <= 40 ~ '<40',
                              between(covariates_1$ageafu, 41,60) ~ '41-60',
                            covariates_1$ageafu >= 61 ~ '61<')

covariates_1$age_cat <- as.factor(covariates_1$age_cat)
saveRDS(covariates_1,paste0(base_path,"Processed_data/20210918_organized_covariates_updated_from_vis2.RData"))
```



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
saveRDS(imputed_data,paste0(base_path,"Processed_data/20210918_imputed_data_updated_from_vis2.RData"))
```

# Package versions

```r
sessionInfo()
```

```
## R version 4.0.3 (2020-10-10)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Catalina 10.15.7
## 
## Matrix products: default
## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats4    grid      stats     graphics  grDevices
## [6] utils     datasets  methods   base     
## 
## other attached packages:
##  [1] rmarkdown_2.7    boot_1.3-27      mi_1.0          
##  [4] sjlabelled_1.1.7 memisc_0.99.27.3 MASS_7.3-53.1   
##  [7] lattice_0.20-41  tableone_0.12.0  labelled_2.8.0  
## [10] factoextra_1.0.7 plyr_1.8.6       survey_4.0      
## [13] survival_3.2-9   Matrix_1.3-2     forcats_0.5.1   
## [16] stringr_1.4.0    dplyr_1.0.5      purrr_0.3.4     
## [19] readr_1.4.0      tidyr_1.1.3      tibble_3.1.0    
## [22] ggplot2_3.3.3    tidyverse_1.3.0 
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-152        fs_1.5.0           
##  [3] lubridate_1.7.10    insight_0.13.1     
##  [5] RColorBrewer_1.1-2  httr_1.4.2         
##  [7] repr_1.1.3          bslib_0.2.4        
##  [9] tools_4.0.3         backports_1.2.1    
## [11] utf8_1.2.1          R6_2.5.0           
## [13] rpart_4.1-15        Hmisc_4.5-0        
## [15] DBI_1.1.1           colorspace_2.0-0   
## [17] nnet_7.3-15         withr_2.4.1        
## [19] gridExtra_2.3       tidyselect_1.1.0   
## [21] curl_4.3            compiler_4.0.3     
## [23] cli_3.0.1           rvest_1.0.0        
## [25] htmlTable_2.1.0     xml2_1.3.2         
## [27] sass_0.3.1          checkmate_2.0.0    
## [29] scales_1.1.1        digest_0.6.27      
## [31] minqa_1.2.4         foreign_0.8-81     
## [33] rio_0.5.26          base64enc_0.1-3    
## [35] jpeg_0.1-8.1        pkgconfig_2.0.3    
## [37] htmltools_0.5.1.1   lme4_1.1-26        
## [39] dbplyr_2.1.0        htmlwidgets_1.5.3  
## [41] rlang_0.4.10        readxl_1.3.1       
## [43] rstudioapi_0.13     jquerylib_0.1.3    
## [45] generics_0.1.0      jsonlite_1.7.2     
## [47] zip_2.1.1           car_3.0-10         
## [49] magrittr_2.0.1      Formula_1.2-4      
## [51] Rcpp_1.0.6          munsell_0.5.0      
## [53] fansi_0.4.2         abind_1.4-5        
## [55] lifecycle_1.0.0     stringi_1.5.3      
## [57] yaml_2.2.1          carData_3.0-4      
## [59] parallel_4.0.3      ggrepel_0.9.1      
## [61] crayon_1.4.1        haven_2.3.1        
## [63] splines_4.0.3       hms_1.0.0          
## [65] knitr_1.31          pillar_1.5.1       
## [67] reprex_1.0.0        glue_1.4.2         
## [69] evaluate_0.14       mitools_2.4        
## [71] latticeExtra_0.6-29 data.table_1.14.0  
## [73] modelr_0.1.8        nloptr_1.2.2.2     
## [75] vctrs_0.3.6         png_0.1-7          
## [77] cellranger_1.1.0    gtable_0.3.0       
## [79] assertthat_0.2.1    xfun_0.22          
## [81] openxlsx_4.2.3      broom_0.7.8        
## [83] coda_0.19-4         arm_1.11-2         
## [85] cluster_2.1.1       statmod_1.4.35     
## [87] ellipsis_0.3.1
```



