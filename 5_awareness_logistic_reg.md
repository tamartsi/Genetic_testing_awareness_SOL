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
base_path <- "~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20221003_data_code/"
data_file <- paste0(base_path, "Processed_data/gte_data_set_with_covariates_and_IPW.RData")
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
##                                                        Estimate Std. Error
## (Intercept)                                          -0.2896047  0.2642099
## AGE41-60                                              0.0203085  0.1098519
## AGE61<                                               -0.0518027  0.1542131
## SEXMale                                              -0.1261216  0.0953553
## Education12                                           0.0372692  0.1133895
## Education>12                                          0.5032829  0.1096835
## EducationMasters, doctoral, professional              0.3907866  0.2672481
## CENTERChicago                                        -0.6116092  0.1651677
## CENTERMiami                                           0.5902916  0.1871278
## CENTERSan Diego                                       0.1591271  0.2068057
## Income_level$10,001-$20,000                           0.1309091  0.1569857
## Income_level$20,001-$40,000                           0.2137208  0.1599437
## Income_level$40,001-$75,000                           0.5703733  0.1806903
## Income_levelMore than $75,000                         0.9278268  0.2331238
## Current_Health_insuranceYes                           0.0157933  0.1142263
## Physician_VisitOne or two times                      -0.0243046  0.1213557
## Physician_VisitAt least three times                   0.0002594  0.1315060
## BackgroundCentral American                            0.0720330  0.2431862
## BackgroundCuban                                       0.1894237  0.2497116
## BackgroundMexican                                    -0.0152074  0.2296898
## BackgroundPuerto Rican                               -0.0199260  0.1918726
## BackgroundSouth American                             -0.0524929  0.2367541
## BackgroundMore than one/Other heritage                0.3863521  0.3141148
## Language_prefEnglish                                  0.1653354  0.1348006
## Marital_statusMarried or living with a partner       -0.1700963  0.1113848
## Marital_statusSeparated,divorced,or widow(er)        -0.3665790  0.1473698
## Employment_statusEmployed part-time(<=35 hours/week)  0.0776663  0.1265971
## Employment_statusEmployed full-time(>35 hours/week)  -0.0345757  0.1159329
## US_BORNYes                                            0.1314126  0.1455371
##                                                      t value Pr(>|t|)    
## (Intercept)                                           -1.096 0.273503    
## AGE41-60                                               0.185 0.853398    
## AGE61<                                                -0.336 0.737061    
## SEXMale                                               -1.323 0.186497    
## Education12                                            0.329 0.742519    
## Education>12                                           4.589 5.52e-06 ***
## EducationMasters, doctoral, professional               1.462 0.144236    
## CENTERChicago                                         -3.703 0.000234 ***
## CENTERMiami                                            3.154 0.001695 ** 
## CENTERSan Diego                                        0.769 0.441952    
## Income_level$10,001-$20,000                            0.834 0.404701    
## Income_level$20,001-$40,000                            1.336 0.182023    
## Income_level$40,001-$75,000                            3.157 0.001683 ** 
## Income_levelMore than $75,000                          3.980 7.81e-05 ***
## Current_Health_insuranceYes                            0.138 0.890083    
## Physician_VisitOne or two times                       -0.200 0.841338    
## Physician_VisitAt least three times                    0.002 0.998427    
## BackgroundCentral American                             0.296 0.767184    
## BackgroundCuban                                        0.759 0.448432    
## BackgroundMexican                                     -0.066 0.947236    
## BackgroundPuerto Rican                                -0.104 0.917326    
## BackgroundSouth American                              -0.222 0.824614    
## BackgroundMore than one/Other heritage                 1.230 0.219229    
## Language_prefEnglish                                   1.227 0.220524    
## Marital_statusMarried or living with a partner        -1.527 0.127305    
## Marital_statusSeparated,divorced,or widow(er)         -2.487 0.013158 *  
## Employment_statusEmployed part-time(<=35 hours/week)   0.613 0.539802    
## Employment_statusEmployed full-time(>35 hours/week)   -0.298 0.765633    
## US_BORNYes                                             0.903 0.366945    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9890217)
## 
## Number of Fisher Scoring iterations: 4
```

Summarize in a nice table

```r
tbl_aware <- tbl_regression(model_aware, exponentiate = TRUE)
tbl_aware %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(output_dir, "/composite_awarenes_glm.docx"))
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
flextable::save_as_docx(ftbl_merge_awarenss_w, path = paste0(output_dir, "/separate_awarenes_glm.docx"))
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
flextable::save_as_docx(ftbl_merge_center_w, path = paste0(output_dir, "/awarenes_glm_by_center.docx"))
```

# Because San Diego have mostly Mexicans, redo San Diego analysis without background


```r
subset_site <- subset(survey_trim, CENTER == "San Diego")
model_sandiego <- svyglm(aware~AGE+SEX+Education+Income_level+Current_Health_insurance+Physician_Visit+Language_pref+Marital_status+Employment_status+US_BORN,design=subset_site,na.action=na.omit,family=quasibinomial())

tbl_sandiego <- tbl_regression(model_sandiego, exponentiate = TRUE)

tbl_sandiego %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = paste0(output_dir, "/awarenes_glm_by_san_diego_no_background.docx"))
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
flextable::save_as_docx(ftbl_merge_sex_w, path = paste0(output_dir, "/awarenes_glm_by_gender.docx"))
```



# version:

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
