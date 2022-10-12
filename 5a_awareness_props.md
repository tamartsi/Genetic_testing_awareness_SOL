---
title: "Computing proportions of individuals aware of genetic tests"
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

# Compute survey-weighted proportions of awareness of the composite outcome across strata
For all factors variables used in table 2, compute the weighted proportions of 
being aware of any genetic test.



```r
vars <- c("AGE", 
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
          "US_BORN")

res_list <- vector(mode = "list", length = length(vars))

for (i in 1:length(vars)){
  var <- vars[i]
  cur_prop <- svyby(~aware, as.formula(paste0("~", var)),survey_trim, svyciprop)
  cur_est <- round(cur_prop$aware,2)
  cur_se <- cur_prop[["se.as.numeric(aware)"]]
  cur_res <- data.frame(var = var, 
                        stratum = cur_prop[[var]], 
                        prop_aware = round(cur_est,2),
                        CI_95 = paste0(round(cur_est-1.96*cur_se,2), ",", round(cur_est+1.96*cur_se,2)))
  res_list[[i]] <- cur_res
}

res <- do.call(rbind, res_list)
```

Save in a nice table (and as a csv file):

```r
tbl_prop_aware <- flextable(res)
flextable::save_as_docx(tbl_prop_aware, path = paste0(output_dir, "/proportion_aware_across_var_levels.docx"))

write.csv(res, file = paste0(output_dir, "/proportion_aware_across_var_levels.csv"))
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
##  [1] dummies_1.5.6      naniar_0.6.1       UpSetR_1.4.0      
##  [4] flextable_0.6.6    gtsummary_1.4.2    Publish_2020.12.23
##  [7] prodlim_2019.11.13 jtools_2.1.3       boot_1.3-27       
## [10] mi_1.0             sjlabelled_1.1.7   memisc_0.99.27.3  
## [13] MASS_7.3-53.1      lattice_0.20-41    tableone_0.12.0   
## [16] labelled_2.8.0     factoextra_1.0.7   plyr_1.8.6        
## [19] survey_4.0         survival_3.2-9     Matrix_1.3-2      
## [22] forcats_0.5.1      stringr_1.4.0      dplyr_1.0.5       
## [25] purrr_0.3.4        readr_1.4.0        tidyr_1.1.3       
## [28] tibble_3.1.7       ggplot2_3.3.3      tidyverse_1.3.0   
## [31] rmarkdown_2.7     
## 
## loaded via a namespace (and not attached):
##   [1] minqa_1.2.4         colorspace_2.0-0    class_7.3-18       
##   [4] ellipsis_0.3.2      rio_0.5.26          visdat_0.5.3       
##   [7] htmlTable_2.1.0     base64enc_0.1-3     fs_1.5.0           
##  [10] proxy_0.4-25        rstudioapi_0.13     farver_2.1.0       
##  [13] ggrepel_0.9.1       fansi_0.4.2         lubridate_1.7.10   
##  [16] xml2_1.3.2          splines_4.0.3       knitr_1.31         
##  [19] Formula_1.2-4       jsonlite_1.7.2      nloptr_1.2.2.2     
##  [22] gt_0.3.0            broom_0.7.8         cluster_2.1.1      
##  [25] dbplyr_2.1.0        png_0.1-7           compiler_4.0.3     
##  [28] httr_1.4.2          backports_1.2.1     assertthat_0.2.1   
##  [31] cli_3.3.0           htmltools_0.5.1.1   tools_4.0.3        
##  [34] coda_0.19-4         gtable_0.3.0        glue_1.6.2         
##  [37] Rcpp_1.0.6          carData_3.0-4       cellranger_1.1.0   
##  [40] jquerylib_0.1.3     vctrs_0.4.1         nlme_3.1-152       
##  [43] broom.helpers_1.3.0 insight_0.17.1      xfun_0.22          
##  [46] openxlsx_4.2.3      lme4_1.1-26         rvest_1.0.0        
##  [49] lifecycle_1.0.0     statmod_1.4.35      zoo_1.8-9          
##  [52] scales_1.1.1        hms_1.0.0           RColorBrewer_1.1-2 
##  [55] yaml_2.2.1          curl_4.3            gridExtra_2.3      
##  [58] pander_0.6.3        gdtools_0.2.3       sass_0.3.1         
##  [61] rpart_4.1-15        latticeExtra_0.6-29 stringi_1.5.3      
##  [64] highr_0.8           e1071_1.7-5         checkmate_2.0.0    
##  [67] zip_2.1.1           lava_1.6.9          repr_1.1.3         
##  [70] commonmark_1.7      rlang_1.0.3         pkgconfig_2.0.3    
##  [73] systemfonts_1.0.1   arm_1.11-2          evaluate_0.14      
##  [76] labeling_0.4.2      htmlwidgets_1.5.3   tidyselect_1.1.0   
##  [79] magrittr_2.0.1      R6_2.5.0            generics_0.1.2     
##  [82] Hmisc_4.5-0         DBI_1.1.1           pillar_1.7.0       
##  [85] haven_2.3.1         foreign_0.8-81      withr_2.4.1        
##  [88] abind_1.4-5         nnet_7.3-15         modelr_0.1.8       
##  [91] crayon_1.4.1        car_3.0-10          uuid_0.1-4         
##  [94] utf8_1.2.1          officer_0.3.18      jpeg_0.1-8.1       
##  [97] readxl_1.3.1        data.table_1.14.0   reprex_1.0.0       
## [100] digest_0.6.27       munsell_0.5.0       bslib_0.2.4        
## [103] mitools_2.4
```
