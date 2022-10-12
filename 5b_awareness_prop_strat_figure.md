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
library(ggpubr)
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
One of the reviewers has a nice suggestion to make a figure displaying proportions of individuals aware of genetic tests, stratified by education, income, and study site (center).
We here generate the data for that (also generated as part of script 5a) and make a figure (though we will re-create a figure for the paper to match the style of the other paper figure).



```r
vars <- c("Education", 
          "CENTER", 
          "Income_level")

res_list <- vector(mode = "list", length = length(vars))

for (i in 1:length(vars)){
  var <- vars[i]
  cur_prop <- svyby(~aware, as.formula(paste0("~", var)),survey_trim, svyciprop)
  cur_est <- round(cur_prop$aware,2)
  cur_se <- cur_prop[["se.as.numeric(aware)"]]
  cur_res <- data.frame(var = var, 
                        stratum = cur_prop[[var]], 
                        prop_aware = cur_est,
                        SE = cur_se,
                        CI_low = cur_est-1.96*cur_se,
                        CI_high = cur_est + 1.96*cur_se)
  res_list[[i]] <- cur_res
}

res <- do.call(rbind, res_list)
```

Save in a file:

```r
write.csv(res, file = paste0(output_dir, "/proportion_aware_education_income_center_for_figure.csv"))
```

Figure -- by variables

```r
p_edu <- ggplot(res[which(res$var == "Education"),], aes(x=stratum, y=prop_aware, fill=stratum)) + 
      geom_bar(stat="identity", position=position_dodge(0.9))+ 
      geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2, position=position_dodge(.9)) +
      scale_fill_manual(values=c("#fec44f", "#2b8cbe","#a6bddb", "#2ca25f")) +
      theme_minimal() + xlab("") + ylab("Proportion aware") + 
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 60, vjust=1, hjust =1)) + coord_flip()

p_center <- ggplot(res[which(res$var == "CENTER"),], aes(x=stratum, y=prop_aware, fill=stratum)) + 
      geom_bar(stat="identity", position=position_dodge(0.9))+ 
      geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2, position=position_dodge(.9)) +
      scale_fill_manual(values=c("#fec44f", "#2b8cbe","#a6bddb", "#2ca25f")) +
      theme_minimal() + xlab("") + ylab("Proportion aware") + 
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 60, vjust=1, hjust =1)) + coord_flip()

p_income <- ggplot(res[which(res$var == "Income_level"),], aes(x=stratum, y=prop_aware, fill=stratum)) + 
      geom_bar(stat="identity", position=position_dodge(0.9))+ 
      geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2, position=position_dodge(.9)) +
      scale_fill_manual(values=c("#fec44f", "#2b8cbe","#a6bddb", "#2ca25f", "#99d8c9")) +
      theme_minimal() + xlab("") + ylab("Proportion aware") + 
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 60, vjust=1, hjust =1)) + coord_flip()


fig <- ggarrange(p_edu, p_center, p_income, ncol = 1, align = "v")

ggexport(fig, filename = file.path(output_dir, "awareness_edu_enter_income.pdf"))
```

```
## file saved to ~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20221003_data_code/Results/awareness_edu_enter_income.pdf
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
##  [1] ggpubr_0.4.0       dummies_1.5.6      naniar_0.6.1      
##  [4] UpSetR_1.4.0       flextable_0.6.6    gtsummary_1.4.2   
##  [7] Publish_2020.12.23 prodlim_2019.11.13 jtools_2.1.3      
## [10] boot_1.3-27        mi_1.0             sjlabelled_1.1.7  
## [13] memisc_0.99.27.3   MASS_7.3-53.1      lattice_0.20-41   
## [16] tableone_0.12.0    labelled_2.8.0     factoextra_1.0.7  
## [19] plyr_1.8.6         survey_4.0         survival_3.2-9    
## [22] Matrix_1.3-2       forcats_0.5.1      stringr_1.4.0     
## [25] dplyr_1.0.5        purrr_0.3.4        readr_1.4.0       
## [28] tidyr_1.1.3        tibble_3.1.7       ggplot2_3.3.3     
## [31] tidyverse_1.3.0    rmarkdown_2.7     
## 
## loaded via a namespace (and not attached):
##   [1] readxl_1.3.1        uuid_0.1-4          backports_1.2.1    
##   [4] Hmisc_4.5-0         systemfonts_1.0.1   repr_1.1.3         
##   [7] splines_4.0.3       digest_0.6.27       htmltools_0.5.1.1  
##  [10] fansi_0.4.2         magrittr_2.0.1      checkmate_2.0.0    
##  [13] cluster_2.1.1       openxlsx_4.2.3      modelr_0.1.8       
##  [16] officer_0.3.18      jpeg_0.1-8.1        colorspace_2.0-0   
##  [19] rvest_1.0.0         ggrepel_0.9.1       mitools_2.4        
##  [22] haven_2.3.1         xfun_0.22           crayon_1.4.1       
##  [25] jsonlite_1.7.2      lme4_1.1-26         zoo_1.8-9          
##  [28] glue_1.6.2          gtable_0.3.0        car_3.0-10         
##  [31] abind_1.4-5         scales_1.1.1        DBI_1.1.1          
##  [34] rstatix_0.7.0       Rcpp_1.0.6          htmlTable_2.1.0    
##  [37] foreign_0.8-81      proxy_0.4-25        Formula_1.2-4      
##  [40] lava_1.6.9          htmlwidgets_1.5.3   httr_1.4.2         
##  [43] RColorBrewer_1.1-2  ellipsis_0.3.2      pkgconfig_2.0.3    
##  [46] farver_2.1.0        nnet_7.3-15         sass_0.3.1         
##  [49] dbplyr_2.1.0        utf8_1.2.1          tidyselect_1.1.0   
##  [52] labeling_0.4.2      rlang_1.0.3         munsell_0.5.0      
##  [55] cellranger_1.1.0    tools_4.0.3         cli_3.3.0          
##  [58] generics_0.1.2      broom_0.7.8         evaluate_0.14      
##  [61] arm_1.11-2          yaml_2.2.1          knitr_1.31         
##  [64] fs_1.5.0            zip_2.1.1           pander_0.6.3       
##  [67] visdat_0.5.3        nlme_3.1-152        xml2_1.3.2         
##  [70] compiler_4.0.3      rstudioapi_0.13     curl_4.3           
##  [73] png_0.1-7           e1071_1.7-5         ggsignif_0.6.1     
##  [76] gt_0.3.0            reprex_1.0.0        statmod_1.4.35     
##  [79] broom.helpers_1.3.0 bslib_0.2.4         stringi_1.5.3      
##  [82] highr_0.8           gdtools_0.2.3       commonmark_1.7     
##  [85] nloptr_1.2.2.2      vctrs_0.4.1         pillar_1.7.0       
##  [88] lifecycle_1.0.0     jquerylib_0.1.3     cowplot_1.1.1      
##  [91] data.table_1.14.0   insight_0.17.1      R6_2.5.0           
##  [94] latticeExtra_0.6-29 gridExtra_2.3       rio_0.5.26         
##  [97] assertthat_0.2.1    withr_2.4.1         hms_1.0.0          
## [100] rpart_4.1-15        coda_0.19-4         class_7.3-18       
## [103] minqa_1.2.4         carData_3.0-4       lubridate_1.7.10   
## [106] base64enc_0.1-3
```
