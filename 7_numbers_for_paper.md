---
title: "Using code to extract numbers for the manuscript"
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
```


# Setting up working directory

```r
base_path <- "~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20221003_data_code/"
data_file <- paste0(base_path, "Processed_data/gte_data_set_with_covariates_and_IPW.RData")
output_dir <- paste0(base_path, "Results")
visit1_covariates_file <- paste0(base_path, "/Processed_data/organized_visit1_covariates.RData")
```



# read the data and format variables as necessary


```r
data_gte <- readRDS(data_file)

# combined awareness variable:

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

# change "refuse to respond" to the individual awareness items to NAs:
data_gte$GTE1[which(data_gte$GTE1 == 9)] <- NA
data_gte$GTE2[which(data_gte$GTE2 == 9)] <- NA
data_gte$GTE3[which(data_gte$GTE3 == 9)] <- NA
data_gte$GTE4[which(data_gte$GTE4 == 9)] <- NA

# creating composite "offered" and "received" responses:
# we assume that all non-responses indicate no offer of test.
# this is reasonable due to skip pattern.
data_gte$offered <- 0
data_gte$offered[which(data_gte$GTE1A == "1" | 
                         data_gte$GTE2A == "1" | 
                         data_gte$GTE3A == "1" | 
                         data_gte$GTE4A == "1")] <- 1


data_gte$received <- 0
data_gte$received[which(data_gte$GTE1B == "1" | 
                         data_gte$GTE2B == "1" | 
                         data_gte$GTE3B == "1" | 
                         data_gte$GTE4B == "1")] <- 1


survey_gte <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~IPW , data=data_gte)
survey_trim <- trimWeights(survey_gte,upper = 30)
```

# compute participation rate in the survey based on the total HCHS/SOL population

```r
covariates_orig <- readRDS(visit1_covariates_file)
covariates_orig$gte_par <- 0
covariates_orig$gte_par[which(covariates_orig$ID %in% data_gte$ID)] <- 1
survey_all <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~WEIGHT_FINAL_NORM_OVERALL , data=covariates_orig)
round(as.matrix(svyciprop(~I(gte_par==1),survey_all,method="lo"))*100,1)
```

```
##                 [,1]
## I(gte_par == 1) 32.6
```

# Text for paper with numbers based on IPW survey estimates: results paragraph 2.
Overall, of 5769 survey respondents,2891 (weighted percentage 55.3%) of respondents reported awareness of at least one type of genetic test. Respondents were most likely to report awareness of genetic tests to determine risks of getting disease (2089 individuals, weighted percentage 40.3%) and the likelihood of passing disease to children (2059 individuals, weighted percentage 42.1%), and least likely to know about genetic tests about how diseases should be treated (874 individuals, weighted percentage 16.6%) or determining drug response (885 individuals, weighted percentage 16.4%). 

Few respondents reported ever being offered or receiving the types of genetic tests described in the survey (Figure 1). Only 362 individuals (weighted percentage 6.5%) of respondents reported being offered any of the four types of tests described, and 190 individuals (weighted percentage 3.3%) of respondents reported receiving at least one of the tests described. 


# Text for paper with numbers based on IPW survey estimates: usefulness rating.
Based on this:


```r
svymean(~GTE5, design = survey_trim, na.rm = TRUE)
```

```
##        mean     SE
## GTE5 8.4135 0.0517
```

Overall, participants rated the usefulness of genetic testing for managing a person’s health 8.4 (SE 0.05) on average in a 0-11 scale.  








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

