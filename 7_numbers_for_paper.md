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
base_path <- "~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20210917_data_code/"
data_file <- paste0(base_path, "Processed_data/20210918_gte_data_set_with_covariates_and_IPW.RData")
output_dir <- paste0(base_path, "Results")
visit1_covariates_file <- paste0(base_path, "/Processed_data/20210917_organized_visit1_covariates.RData")
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
Overall, 55.2% of respondents reported awareness of at least one type of genetic test. Respondents were most likely to report awareness of genetic tests to determine risks of getting disease (40.3%) and the likelihood of passing disease to children (42%), and least likely to know about genetic tests about how diseases should be treated (16.6%) or determining drug response (16.3%). Few respondents reported ever being offered or receiving the types of genetic tests described in the survey (Figure 1). Only 6.5% of respondents reported being offered any of the four types of tests described, and 3.3% of respondents reported receiving at least one of the tests described. 


# Text for paper with numbers based on IPW survey estimates: usefulness rating.
Based on this:


```r
svymean(~GTE5, design = survey_trim, na.rm = TRUE)
```

```
##        mean     SE
## GTE5 8.4168 0.0513
```

Overall, participants rated the usefulness of genetic testing for managing a person’s health 8.4 (SE 0.05) on average in a 0-11 scale.  








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
## [1] grid      stats     graphics  grDevices
## [5] utils     datasets  methods   base     
## 
## other attached packages:
##  [1] rmarkdown_2.7      Publish_2020.12.23
##  [3] prodlim_2019.11.13 jtools_2.1.3      
##  [5] memisc_0.99.27.3   MASS_7.3-53.1     
##  [7] lattice_0.20-41    tableone_0.12.0   
##  [9] labelled_2.8.0     factoextra_1.0.7  
## [11] plyr_1.8.6         survey_4.0        
## [13] survival_3.2-9     Matrix_1.3-2      
## [15] forcats_0.5.1      stringr_1.4.0     
## [17] dplyr_1.0.5        purrr_0.3.4       
## [19] readr_1.4.0        tidyr_1.1.3       
## [21] tibble_3.1.0       ggplot2_3.3.3     
## [23] tidyverse_1.3.0   
## 
## loaded via a namespace (and not attached):
##  [1] fs_1.5.0          lubridate_1.7.10 
##  [3] httr_1.4.2        repr_1.1.3       
##  [5] tools_4.0.3       backports_1.2.1  
##  [7] bslib_0.2.4       utf8_1.2.1       
##  [9] R6_2.5.0          DBI_1.1.1        
## [11] colorspace_2.0-0  withr_2.4.1      
## [13] tidyselect_1.1.0  curl_4.3         
## [15] compiler_4.0.3    cli_3.0.1        
## [17] rvest_1.0.0       flextable_0.6.6  
## [19] xml2_1.3.2        officer_0.3.18   
## [21] sass_0.3.1        scales_1.1.1     
## [23] systemfonts_1.0.1 digest_0.6.27    
## [25] foreign_0.8-81    rio_0.5.26       
## [27] base64enc_0.1-3   pkgconfig_2.0.3  
## [29] htmltools_0.5.1.1 dbplyr_2.1.0     
## [31] rlang_0.4.10      readxl_1.3.1     
## [33] rstudioapi_0.13   jquerylib_0.1.3  
## [35] generics_0.1.0    jsonlite_1.7.2   
## [37] zip_2.1.1         car_3.0-10       
## [39] magrittr_2.0.1    Rcpp_1.0.6       
## [41] munsell_0.5.0     fansi_0.4.2      
## [43] abind_1.4-5       gdtools_0.2.3    
## [45] lifecycle_1.0.0   stringi_1.5.3    
## [47] yaml_2.2.1        carData_3.0-4    
## [49] ggrepel_0.9.1     crayon_1.4.1     
## [51] haven_2.3.1       splines_4.0.3    
## [53] pander_0.6.3      hms_1.0.0        
## [55] knitr_1.31        pillar_1.5.1     
## [57] uuid_0.1-4        reprex_1.0.0     
## [59] glue_1.4.2        evaluate_0.14    
## [61] mitools_2.4       data.table_1.14.0
## [63] modelr_0.1.8      vctrs_0.3.6      
## [65] cellranger_1.1.0  gtable_0.3.0     
## [67] assertthat_0.2.1  xfun_0.22        
## [69] openxlsx_4.2.3    broom_0.7.8      
## [71] lava_1.6.9        ellipsis_0.3.1
```

