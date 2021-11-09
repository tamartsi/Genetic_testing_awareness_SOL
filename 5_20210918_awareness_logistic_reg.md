---
title: "Logistic regression of genetic testing awareness"
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
library(memisc)
library(jtools)
library(Publish)
library(gtsummary)
library(flextable)
```

# Setting up working directory

```r
base_path <- "~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20210917_data_code/"
data_file <- paste0(base_path, "Processed_data/20210918_gte_data_set_with_covariates_and_IPW.RData")
output_dir <- paste0(base_path, "Results")
```

# Loading the data and weight the data with IPW (trim the weights at 30)
We need to define the composite genetic testing awareness variable (we did not keep it in the data frame generated in earlier reports)

```r
data_gte <- readRDS(data_file)
nrow(data_gte)
```

```
## [1] 5769
```

```r
data_gte$aware <- NA
data_gte$aware[which(data_gte$GTE1 == 1 | 
                         data_gte$GTE2 == 1 | 
                         data_gte$GTE3 == 1 | 
                         data_gte$GTE4 == 1)] <- 1
data_gte$aware[which(is.na(data_gte$aware) & 
                         (data_gte$GTE1 == 0 | 
                         data_gte$GTE2 == 0 | 
                         data_gte$GTE3 == 0 | 
                         data_gte$GTE4 == 0))] <- 0

table(data_gte$aware)
```

```
## 
##    0    1 
## 2878 2891
```

```r
survey_gte <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~IPW , data=data_gte)
survey_trim <- trimWeights(survey_gte,upper = 30)
```

# Perform logistic regression of composite measure of genetic testing awareness



```r
# we just use the doctor visit variable from visit 1, and not the personal doctor from visit 2. 
model_aware <- svyglm(aware~AGE+SEX+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=survey_trim,na.action=na.omit,family=quasibinomial())
summary(model_aware)
```

```
## 
## Call:
## svyglm(formula = aware ~ AGE + SEX + Education + CENTER + Income_level + 
##     Current_Health_insurance + Physician_Visit + Background + 
##     Language_pref + Marital_status + Employment_status + US_BORN, 
##     design = survey_trim, family = quasibinomial(), na.action = na.omit)
## 
## Survey design:
## trimWeights(survey_gte, upper = 30)
## 
## Coefficients:
##                                                       Estimate
## (Intercept)                                          -0.285430
## AGE41-60                                              0.031437
## AGE61<                                               -0.038407
## SEXMale                                              -0.122330
## Education12                                          -0.039810
## Education>12                                          0.456838
## CENTERChicago                                        -0.592674
## CENTERMiami                                           0.627429
## CENTERSan Diego                                       0.185653
## Income_level$10,001-$20,000                           0.136688
## Income_level$20,001-$40,000                           0.209390
## Income_level$40,001-$75,000                           0.566174
## Income_levelMore than $75,000                         0.938254
## Current_Health_insuranceYes                          -0.003315
## Physician_VisitOne or two times                      -0.017968
## Physician_VisitAt least three times                  -0.001424
## BackgroundCentral American                            0.044657
## BackgroundCuban                                       0.148962
## BackgroundMexican                                    -0.037562
## BackgroundPuerto Rican                               -0.013623
## BackgroundSouth American                             -0.077935
## BackgroundMore than one/Other heritage                0.370374
## Language_prefEnglish                                  0.166374
## Marital_statusMarried or living with a partner       -0.147941
## Marital_statusSeparated,divorced,or widow(er)        -0.347138
## Employment_statusEmployed part-time(<=35 hours/week)  0.065638
## Employment_statusEmployed full-time(>35 hours/week)  -0.032447
## US_BORNYes                                            0.118225
##                                                      Std. Error
## (Intercept)                                            0.266736
## AGE41-60                                               0.110302
## AGE61<                                                 0.152948
## SEXMale                                                0.094063
## Education12                                            0.117411
## Education>12                                           0.109314
## CENTERChicago                                          0.165419
## CENTERMiami                                            0.185708
## CENTERSan Diego                                        0.206552
## Income_level$10,001-$20,000                            0.155812
## Income_level$20,001-$40,000                            0.159089
## Income_level$40,001-$75,000                            0.179456
## Income_levelMore than $75,000                          0.232657
## Current_Health_insuranceYes                            0.113299
## Physician_VisitOne or two times                        0.122312
## Physician_VisitAt least three times                    0.132344
## BackgroundCentral American                             0.242863
## BackgroundCuban                                        0.245113
## BackgroundMexican                                      0.229450
## BackgroundPuerto Rican                                 0.191909
## BackgroundSouth American                               0.235893
## BackgroundMore than one/Other heritage                 0.308961
## Language_prefEnglish                                   0.133928
## Marital_statusMarried or living with a partner         0.111645
## Marital_statusSeparated,divorced,or widow(er)          0.146399
## Employment_statusEmployed part-time(<=35 hours/week)   0.125815
## Employment_statusEmployed full-time(>35 hours/week)    0.115688
## US_BORNYes                                             0.145338
##                                                      t value
## (Intercept)                                           -1.070
## AGE41-60                                               0.285
## AGE61<                                                -0.251
## SEXMale                                               -1.301
## Education12                                           -0.339
## Education>12                                           4.179
## CENTERChicago                                         -3.583
## CENTERMiami                                            3.379
## CENTERSan Diego                                        0.899
## Income_level$10,001-$20,000                            0.877
## Income_level$20,001-$40,000                            1.316
## Income_level$40,001-$75,000                            3.155
## Income_levelMore than $75,000                          4.033
## Current_Health_insuranceYes                           -0.029
## Physician_VisitOne or two times                       -0.147
## Physician_VisitAt least three times                   -0.011
## BackgroundCentral American                             0.184
## BackgroundCuban                                        0.608
## BackgroundMexican                                     -0.164
## BackgroundPuerto Rican                                -0.071
## BackgroundSouth American                              -0.330
## BackgroundMore than one/Other heritage                 1.199
## Language_prefEnglish                                   1.242
## Marital_statusMarried or living with a partner        -1.325
## Marital_statusSeparated,divorced,or widow(er)         -2.371
## Employment_statusEmployed part-time(<=35 hours/week)   0.522
## Employment_statusEmployed full-time(>35 hours/week)   -0.280
## US_BORNYes                                             0.813
##                                                      Pr(>|t|)
## (Intercept)                                           0.28505
## AGE41-60                                              0.77575
## AGE61<                                                0.80182
## SEXMale                                               0.19397
## Education12                                           0.73469
## Education>12                                         3.40e-05
## CENTERChicago                                         0.00037
## CENTERMiami                                           0.00078
## CENTERSan Diego                                       0.36914
## Income_level$10,001-$20,000                           0.38072
## Income_level$20,001-$40,000                           0.18865
## Income_level$40,001-$75,000                           0.00169
## Income_levelMore than $75,000                        6.28e-05
## Current_Health_insuranceYes                           0.97667
## Physician_VisitOne or two times                       0.88326
## Physician_VisitAt least three times                   0.99142
## BackgroundCentral American                            0.85418
## BackgroundCuban                                       0.54362
## BackgroundMexican                                     0.87002
## BackgroundPuerto Rican                                0.94343
## BackgroundSouth American                              0.74124
## BackgroundMore than one/Other heritage                0.23113
## Language_prefEnglish                                  0.21466
## Marital_statusMarried or living with a partner        0.18568
## Marital_statusSeparated,divorced,or widow(er)         0.01807
## Employment_statusEmployed part-time(<=35 hours/week)  0.60209
## Employment_statusEmployed full-time(>35 hours/week)   0.77922
## US_BORNYes                                            0.41631
##                                                         
## (Intercept)                                             
## AGE41-60                                                
## AGE61<                                                  
## SEXMale                                                 
## Education12                                             
## Education>12                                         ***
## CENTERChicago                                        ***
## CENTERMiami                                          ***
## CENTERSan Diego                                         
## Income_level$10,001-$20,000                             
## Income_level$20,001-$40,000                             
## Income_level$40,001-$75,000                          ** 
## Income_levelMore than $75,000                        ***
## Current_Health_insuranceYes                             
## Physician_VisitOne or two times                         
## Physician_VisitAt least three times                     
## BackgroundCentral American                              
## BackgroundCuban                                         
## BackgroundMexican                                       
## BackgroundPuerto Rican                                  
## BackgroundSouth American                                
## BackgroundMore than one/Other heritage                  
## Language_prefEnglish                                    
## Marital_statusMarried or living with a partner          
## Marital_statusSeparated,divorced,or widow(er)        *  
## Employment_statusEmployed part-time(<=35 hours/week)    
## Employment_statusEmployed full-time(>35 hours/week)     
## US_BORNYes                                              
## ---
## Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*'
##   0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9877822)
## 
## Number of Fisher Scoring iterations: 4
```

Summarize in a nice table

```r
tbl_aware <- tbl_regression(model_aware, exponentiate = TRUE)
tbl_aware %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(output_dir, "/20210918_composite_awarenes_glm.docx"))
```

# Perform separate analysis of each of the awareness questions
We need a supplemental table reporting results for each awareness question separately (rather than the composite awareness variable)

```r
# Look at the individual responses:
table(data_gte$GTE1)
```

```
## 
##    0    1    9 
## 3672 2089    1
```

```r
table(data_gte$GTE2)
```

```
## 
##    0    1    9 
## 3329 2059   24
```

```r
table(data_gte$GTE3)
```

```
## 
##    0    1    9 
## 4494  874   28
```

```r
table(data_gte$GTE4)
```

```
## 
##    0    1    9 
## 4479  885   31
```

```r
# change some "refuse to respond" (values of 9) to NAs. Note that as people respond to more questions, more people refuse to respond. 
# then we need to recreate the survey object.
data_gte$GTE1[which(data_gte$GTE1 == 9)] <- NA
data_gte$GTE2[which(data_gte$GTE2 == 9)] <- NA
data_gte$GTE3[which(data_gte$GTE3 == 9)] <- NA
data_gte$GTE4[which(data_gte$GTE4 == 9)] <- NA

survey_gte <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~IPW , data=data_gte)
survey_trim <- trimWeights(survey_gte,upper = 30)

model_gte1 <- svyglm(GTE1~AGE+SEX+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=survey_trim,na.action=na.omit,family=quasibinomial())

model_gte2 <- svyglm(GTE2~AGE+SEX+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=survey_trim,na.action=na.omit,family=quasibinomial())

model_gte3 <- svyglm(GTE3~AGE+SEX+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=survey_trim,na.action=na.omit,family=quasibinomial())

model_gte4 <- svyglm(GTE4~AGE+SEX+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=survey_trim,na.action=na.omit,family=quasibinomial())
```

## Summarize the regressions in a table

```r
tbl_gte1 <- tbl_regression(model_gte1, exponentiate = TRUE)
tbl_gte2 <- tbl_regression(model_gte2, exponentiate = TRUE)
tbl_gte3 <- tbl_regression(model_gte3, exponentiate = TRUE)
tbl_gte4 <- tbl_regression(model_gte4, exponentiate = TRUE)

tbl_merge_awareness <-
  tbl_merge(
    tbls = list(tbl_gte1, tbl_gte2, tbl_gte3, tbl_gte4),
    tab_spanner = c("**Disease risk**", 
                    "**Risk to children**", 
                    "**Personalized treatment**",
                    "**Drug efficacy**")
  )

ftbl_merge_awarenss <- as_flex_table(tbl_merge_awareness)

# function I found online here
# https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar
FitFlextableToPage <- function(ft, pgwidth = 7){
  ft_out <- ft %>% autofit()
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

ftbl_merge_awarenss_w <- FitFlextableToPage(ftbl_merge_awarenss)
flextable::save_as_docx(ftbl_merge_awarenss_w, path = paste0(output_dir, "/20210918_separate_awarenes_glm.docx"))
```


# Perform another analysis stratified by center. 
We want to see if significant associations in the pooled analysis of all individuals are driven by individuals from a specific site. 


```r
subset_site <- subset(survey_trim, CENTER == "Miami")
model_miami <- svyglm(aware~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasibinomial())

subset_site <- subset(survey_trim, CENTER == "Bronx")
model_bronx <- svyglm(aware~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasibinomial())

subset_site <- subset(survey_trim, CENTER == "San Diego")
model_sandiego <- svyglm(aware~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasibinomial())

subset_site <- subset(survey_trim, CENTER == "Chicago")
model_chicago <- svyglm(aware~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasibinomial())
```

## Summarize the center-specific regressions in a table

```r
tbl_miami <- tbl_regression(model_miami, exponentiate = TRUE)
tbl_chicago <- tbl_regression(model_chicago, exponentiate = TRUE)
tbl_bronx <- tbl_regression(model_bronx, exponentiate = TRUE)
tbl_sandiego <- tbl_regression(model_sandiego, exponentiate = TRUE)

tbl_merge_center <-
  tbl_merge(
    tbls = list(tbl_bronx, tbl_chicago, tbl_miami, tbl_sandiego),
    tab_spanner = c("**Bronx**", 
                    "**Chicago**", 
                    "**Miami**",
                    "**San Diego**")
  )

ftbl_merge_center <- as_flex_table(tbl_merge_center)


ftbl_merge_center_w <- FitFlextableToPage(ftbl_merge_center)
flextable::save_as_docx(ftbl_merge_center_w, path = paste0(output_dir, "/20210918_awarenes_glm_by_center.docx"))
```

# Because San Diego have mostly Mexicans, redo San Diego analysis without background


```r
subset_site <- subset(survey_trim, CENTER == "San Diego")
model_sandiego <- svyglm(aware~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasibinomial())

tbl_sandiego <- tbl_regression(model_sandiego, exponentiate = TRUE)

tbl_sandiego %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(output_dir, "/20211012_awarenes_glm_by_san_diego_no_background.docx"))
```


# Perform another analysis stratified by gender. 
Another secondary analysis to allow for checking whether some results differ between women and men. 


```r
subset_sex <- subset(survey_trim, SEX == "Female")
model_female <- svyglm(aware~AGE+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_sex,na.action=na.omit,family=quasibinomial())

subset_sex <- subset(survey_trim, SEX == "Male")
model_male <- svyglm(aware~AGE+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_sex,na.action=na.omit,family=quasibinomial())


tbl_female <- tbl_regression(model_female, exponentiate = TRUE)
tbl_male <- tbl_regression(model_male, exponentiate = TRUE)

tbl_merge_sex <-
  tbl_merge(
    tbls = list(tbl_female, tbl_male),
    tab_spanner = c("**Women**", 
                    "**Men**")
  )

ftbl_merge_sex <- as_flex_table(tbl_merge_sex)


ftbl_merge_sex_w <- FitFlextableToPage(ftbl_merge_sex)
flextable::save_as_docx(ftbl_merge_sex_w, path = paste0(output_dir, "/20210918_awarenes_glm_by_gender.docx"))
```



# version:

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
## [1] grid      stats     graphics 
## [4] grDevices utils     datasets 
## [7] methods   base     
## 
## other attached packages:
##  [1] flextable_0.6.6   
##  [2] gtsummary_1.4.2   
##  [3] Publish_2020.12.23
##  [4] prodlim_2019.11.13
##  [5] jtools_2.1.3      
##  [6] memisc_0.99.27.3  
##  [7] MASS_7.3-53.1     
##  [8] lattice_0.20-41   
##  [9] labelled_2.8.0    
## [10] factoextra_1.0.7  
## [11] plyr_1.8.6        
## [12] survey_4.0        
## [13] survival_3.2-9    
## [14] Matrix_1.3-2      
## [15] forcats_0.5.1     
## [16] stringr_1.4.0     
## [17] dplyr_1.0.5       
## [18] purrr_0.3.4       
## [19] readr_1.4.0       
## [20] tidyr_1.1.3       
## [21] tibble_3.1.0      
## [22] ggplot2_3.3.3     
## [23] tidyverse_1.3.0   
## [24] rmarkdown_2.7     
## 
## loaded via a namespace (and not attached):
##  [1] fs_1.5.0           
##  [2] lubridate_1.7.10   
##  [3] httr_1.4.2         
##  [4] repr_1.1.3         
##  [5] tools_4.0.3        
##  [6] backports_1.2.1    
##  [7] bslib_0.2.4        
##  [8] utf8_1.2.1         
##  [9] R6_2.5.0           
## [10] DBI_1.1.1          
## [11] colorspace_2.0-0   
## [12] withr_2.4.1        
## [13] tidyselect_1.1.0   
## [14] curl_4.3           
## [15] compiler_4.0.3     
## [16] cli_3.0.1          
## [17] rvest_1.0.0        
## [18] gt_0.3.0           
## [19] xml2_1.3.2         
## [20] officer_0.3.18     
## [21] sass_0.3.1         
## [22] scales_1.1.1       
## [23] systemfonts_1.0.1  
## [24] digest_0.6.27      
## [25] foreign_0.8-81     
## [26] rio_0.5.26         
## [27] base64enc_0.1-3    
## [28] pkgconfig_2.0.3    
## [29] htmltools_0.5.1.1  
## [30] dbplyr_2.1.0       
## [31] rlang_0.4.10       
## [32] readxl_1.3.1       
## [33] rstudioapi_0.13    
## [34] jquerylib_0.1.3    
## [35] generics_0.1.0     
## [36] jsonlite_1.7.2     
## [37] zip_2.1.1          
## [38] car_3.0-10         
## [39] magrittr_2.0.1     
## [40] Rcpp_1.0.6         
## [41] munsell_0.5.0      
## [42] fansi_0.4.2        
## [43] abind_1.4-5        
## [44] gdtools_0.2.3      
## [45] lifecycle_1.0.0    
## [46] stringi_1.5.3      
## [47] yaml_2.2.1         
## [48] carData_3.0-4      
## [49] ggrepel_0.9.1      
## [50] crayon_1.4.1       
## [51] haven_2.3.1        
## [52] splines_4.0.3      
## [53] pander_0.6.3       
## [54] hms_1.0.0          
## [55] knitr_1.31         
## [56] pillar_1.5.1       
## [57] uuid_0.1-4         
## [58] reprex_1.0.0       
## [59] glue_1.4.2         
## [60] evaluate_0.14      
## [61] mitools_2.4        
## [62] broom.helpers_1.3.0
## [63] data.table_1.14.0  
## [64] modelr_0.1.8       
## [65] vctrs_0.3.6        
## [66] cellranger_1.1.0   
## [67] gtable_0.3.0       
## [68] assertthat_0.2.1   
## [69] xfun_0.22          
## [70] openxlsx_4.2.3     
## [71] broom_0.7.8        
## [72] lava_1.6.9         
## [73] ellipsis_0.3.1
```
