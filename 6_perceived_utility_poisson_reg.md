---
title: "Poisson regression of perceived utility of genetic tests"
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
library(jtools)
library(Publish)
library(gtsummary)
library(flextable)
```



# Setting up working directory

```r
base_path <- "~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20221003_data_code/"
data_file <- paste0(base_path, "Processed_data/gte_data_set_with_covariates_and_IPW.RData")
output_dir <- paste0(base_path, "Results")
```

# Load the data and weight it with IPW (trim the weight at 30)


```r
data_gte <- readRDS(data_file)
nrow(data_gte)
```

```
## [1] 5769
```

```r
hist(data_gte$GTE5)
```

![](6_20221003_perceived_utility_poisson_reg_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
survey_gte <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~IPW , data=data_gte)
survey_trim <- trimWeights(survey_gte,upper = 30)
```
There are `{r sum(is.na(data_gte$GTE5))}` missing values in the response to the perceived utility question. 


# Perform possion regression of percieved utility.
The responses to the perceived utility question are roughly log-normal. We use Poisson regression, modeling multiplicative effects of the covariates. 


```r
model_utility <- svyglm(GTE5~AGE+SEX+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=survey_trim,na.action=na.omit,family=quasipoisson())
summary(model_utility)
```

```
## 
## Call:
## svyglm(formula = GTE5 ~ AGE + SEX + Education + CENTER + Income_level + 
##     Current_Health_insurance + Physician_Visit + Background + 
##     Language_pref + Marital_status + Employment_status + US_BORN, 
##     design = survey_trim, family = quasipoisson(), na.action = na.omit)
## 
## Survey design:
## trimWeights(survey_gte, upper = 30)
## 
## Coefficients:
##                                                        Estimate Std. Error
## (Intercept)                                           2.0832475  0.0311870
## AGE41-60                                              0.0280713  0.0145198
## AGE61<                                               -0.0157993  0.0168193
## SEXMale                                              -0.0214247  0.0112436
## Education12                                           0.0099932  0.0163084
## Education>12                                          0.0300590  0.0139872
## EducationMasters, doctoral, professional             -0.0322031  0.0276770
## CENTERChicago                                         0.0078975  0.0260469
## CENTERMiami                                           0.0630286  0.0240421
## CENTERSan Diego                                       0.0268053  0.0314600
## Income_level$10,001-$20,000                           0.0096904  0.0245838
## Income_level$20,001-$40,000                           0.0080611  0.0232217
## Income_level$40,001-$75,000                           0.0298049  0.0238492
## Income_levelMore than $75,000                        -0.0165311  0.0283236
## Current_Health_insuranceYes                          -0.0003669  0.0127064
## Physician_VisitOne or two times                       0.0042179  0.0143229
## Physician_VisitAt least three times                   0.0054982  0.0135040
## BackgroundCentral American                            0.0011622  0.0251298
## BackgroundCuban                                       0.0273752  0.0283085
## BackgroundMexican                                    -0.0091241  0.0342770
## BackgroundPuerto Rican                               -0.0040969  0.0255719
## BackgroundSouth American                             -0.0092539  0.0279022
## BackgroundMore than one/Other heritage                0.0008282  0.0335094
## Language_prefEnglish                                 -0.0533399  0.0176735
## Marital_statusMarried or living with a partner        0.0155936  0.0156683
## Marital_statusSeparated,divorced,or widow(er)         0.0218446  0.0163046
## Employment_statusEmployed part-time(<=35 hours/week)  0.0236825  0.0133287
## Employment_statusEmployed full-time(>35 hours/week)  -0.0016855  0.0130673
## US_BORNYes                                           -0.0449002  0.0163542
##                                                      t value Pr(>|t|)    
## (Intercept)                                           66.799  < 2e-16 ***
## AGE41-60                                               1.933  0.05371 .  
## AGE61<                                                -0.939  0.34796    
## SEXMale                                               -1.906  0.05724 .  
## Education12                                            0.613  0.54029    
## Education>12                                           2.149  0.03207 *  
## EducationMasters, doctoral, professional              -1.164  0.24512    
## CENTERChicago                                          0.303  0.76185    
## CENTERMiami                                            2.622  0.00900 ** 
## CENTERSan Diego                                        0.852  0.39456    
## Income_level$10,001-$20,000                            0.394  0.69360    
## Income_level$20,001-$40,000                            0.347  0.72862    
## Income_level$40,001-$75,000                            1.250  0.21194    
## Income_levelMore than $75,000                         -0.584  0.55970    
## Current_Health_insuranceYes                           -0.029  0.97698    
## Physician_VisitOne or two times                        0.294  0.76850    
## Physician_VisitAt least three times                    0.407  0.68405    
## BackgroundCentral American                             0.046  0.96313    
## BackgroundCuban                                        0.967  0.33396    
## BackgroundMexican                                     -0.266  0.79020    
## BackgroundPuerto Rican                                -0.160  0.87278    
## BackgroundSouth American                              -0.332  0.74028    
## BackgroundMore than one/Other heritage                 0.025  0.98029    
## Language_prefEnglish                                  -3.018  0.00266 ** 
## Marital_statusMarried or living with a partner         0.995  0.32007    
## Marital_statusSeparated,divorced,or widow(er)          1.340  0.18087    
## Employment_statusEmployed part-time(<=35 hours/week)   1.777  0.07616 .  
## Employment_statusEmployed full-time(>35 hours/week)   -0.129  0.89742    
## US_BORNYes                                            -2.745  0.00624 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 0.4746891)
## 
## Number of Fisher Scoring iterations: 4
```

Summarize in a nice table

```r
tbl_utility <- tbl_regression(model_utility, exponentiate = TRUE)
tbl_utility %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(output_dir, "/utility_poisson_reg.docx"))
```

# Secondary analysis: stratify by center
This analysis will allow others, if interested, to check whether some results seem to be driven by a specific center (though not to test this hypothesis).


```r
subset_site <- subset(survey_trim, CENTER == "Miami")
model_miami <- svyglm(GTE5~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasipoisson())

subset_site <- subset(survey_trim, CENTER == "Bronx")
model_bronx <- svyglm(GTE5~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasipoisson())

subset_site <- subset(survey_trim, CENTER == "San Diego")
model_sandiego <- svyglm(GTE5~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasipoisson())

subset_site <- subset(survey_trim, CENTER == "Chicago")
model_chicago <- svyglm(GTE5~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasipoisson())

# merge results and prepare a combined table:

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
flextable::save_as_docx(ftbl_merge_center_w, path = paste0(output_dir, "/perceived utility_reg_by_center.docx"))
```


# Secondary analysis: stratify by center
This analysis will allow others, if interested, to check whether some results seem to be driven by a specific center (though not to test this hypothesis).


```r
subset_sex <- subset(survey_trim, SEX == "Female")
model_female <- svyglm(GTE5~AGE+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_sex,na.action=na.omit,family=quasipoisson())

subset_sex <- subset(survey_trim, SEX == "Male")
model_male <- svyglm(GTE5~AGE+Education+CENTER+Income_level+Current_Health_insurance+Physician_Visit+Background+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_sex,na.action=na.omit,family=quasipoisson())


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
flextable::save_as_docx(ftbl_merge_sex_w, path = paste0(output_dir, "/perceived_utility_reg_by_gender.docx"))
```


```r
sessionInfo()
```

```
## R version 4.0.3 (2020-10-10)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS  12.5.1
## 
## Matrix products: default
## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats4    grid      stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] flextable_0.6.6    gtsummary_1.4.2    Publish_2020.12.23
##  [4] prodlim_2019.11.13 jtools_2.1.3       boot_1.3-27       
##  [7] mi_1.0             sjlabelled_1.1.7   memisc_0.99.27.3  
## [10] MASS_7.3-53.1      lattice_0.20-41    tableone_0.12.0   
## [13] labelled_2.8.0     factoextra_1.0.7   plyr_1.8.6        
## [16] survey_4.0         survival_3.2-9     Matrix_1.3-2      
## [19] forcats_0.5.1      stringr_1.4.0      dplyr_1.0.5       
## [22] purrr_0.3.4        readr_1.4.0        tidyr_1.1.3       
## [25] tibble_3.1.7       ggplot2_3.3.3      tidyverse_1.3.0   
## [28] rmarkdown_2.7     
## 
## loaded via a namespace (and not attached):
##  [1] minqa_1.2.4         colorspace_2.0-0    ellipsis_0.3.2     
##  [4] rio_0.5.26          htmlTable_2.1.0     base64enc_0.1-3    
##  [7] fs_1.5.0            rstudioapi_0.13     ggrepel_0.9.1      
## [10] fansi_0.4.2         lubridate_1.7.10    xml2_1.3.2         
## [13] splines_4.0.3       knitr_1.31          Formula_1.2-4      
## [16] jsonlite_1.7.2      nloptr_1.2.2.2      gt_0.3.0           
## [19] broom_0.7.8         cluster_2.1.1       dbplyr_2.1.0       
## [22] png_0.1-7           compiler_4.0.3      httr_1.4.2         
## [25] backports_1.2.1     assertthat_0.2.1    cli_3.3.0          
## [28] htmltools_0.5.1.1   tools_4.0.3         coda_0.19-4        
## [31] gtable_0.3.0        glue_1.6.2          Rcpp_1.0.6         
## [34] carData_3.0-4       cellranger_1.1.0    jquerylib_0.1.3    
## [37] vctrs_0.4.1         nlme_3.1-152        broom.helpers_1.3.0
## [40] insight_0.17.1      xfun_0.22           openxlsx_4.2.3     
## [43] lme4_1.1-26         rvest_1.0.0         lifecycle_1.0.0    
## [46] statmod_1.4.35      zoo_1.8-9           scales_1.1.1       
## [49] hms_1.0.0           RColorBrewer_1.1-2  yaml_2.2.1         
## [52] curl_4.3            gridExtra_2.3       pander_0.6.3       
## [55] gdtools_0.2.3       sass_0.3.1          rpart_4.1-15       
## [58] latticeExtra_0.6-29 stringi_1.5.3       highr_0.8          
## [61] checkmate_2.0.0     zip_2.1.1           lava_1.6.9         
## [64] repr_1.1.3          commonmark_1.7      rlang_1.0.3        
## [67] pkgconfig_2.0.3     systemfonts_1.0.1   arm_1.11-2         
## [70] evaluate_0.14       htmlwidgets_1.5.3   tidyselect_1.1.0   
## [73] magrittr_2.0.1      R6_2.5.0            generics_0.1.2     
## [76] Hmisc_4.5-0         DBI_1.1.1           pillar_1.7.0       
## [79] haven_2.3.1         foreign_0.8-81      withr_2.4.1        
## [82] abind_1.4-5         nnet_7.3-15         modelr_0.1.8       
## [85] crayon_1.4.1        car_3.0-10          uuid_0.1-4         
## [88] utf8_1.2.1          officer_0.3.18      jpeg_0.1-8.1       
## [91] readxl_1.3.1        data.table_1.14.0   reprex_1.0.0       
## [94] digest_0.6.27       munsell_0.5.0       bslib_0.2.4        
## [97] mitools_2.4
```

