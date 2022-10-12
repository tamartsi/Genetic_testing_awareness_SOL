---
title: "Usings contrasts to provide effect estimates and CIs for effects we may want to report"
output:
  html_document:
    toc: yes
    keep_md: true
  pdf_document:
    toc: yes
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
library(dummies)
```
# Setting up working directory

```r
base_path <- "~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20221003_data_code/"
data_file <- paste0(base_path, "Processed_data/gte_data_set_with_covariates_and_IPW.RData")
```


# Computing effect estimates for significant differences
For every effect estimate that had p-value<0.05, we here compute an estimate (making sure we are comparing "extreme" factors) and 95% confidence interval. We also compute (weighted) proportions. This is useful because we can use this to put results in the text. 

# Load the data and prepare the model


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
data_gte$GTE1[which(data_gte$GTE1 == 9)] <- NA
data_gte$GTE2[which(data_gte$GTE2 == 9)] <- NA
data_gte$GTE3[which(data_gte$GTE3 == 9)] <- NA
data_gte$GTE4[which(data_gte$GTE4 == 9)] <- NA
```



# Prreparing dummy variables of interest
Creating dummy variables in preparation for computing contrast

```r
data_gte$income_10000 <- ifelse(data_gte$Income_level=="Less than $10,000",1,0)
data_gte$income_20000 <- ifelse(data_gte$Income_level=="$10,001-$20,000",1,0)
data_gte$income_40000 <- ifelse(data_gte$Income_level=="$20,001-$40,000",1,0)
data_gte$income_75000 <- ifelse(data_gte$Income_level=="$40,001-$75,000",1,0)
data_gte$income_75000_high <- ifelse(data_gte$Income_level=="More than $75,000",1,0)

data_gte$Pref_english <- ifelse(data_gte$Language_pref == "English", 1, 0)
data_gte$Pref_spanish <- ifelse(data_gte$Language_pref == "Spanish", 1, 0)

data_gte$age_40 <- ifelse(data_gte$AGE=="<40",1,0)
data_gte$age_60 <- ifelse(data_gte$AGE=="41-60",1,0)
data_gte$age_60_high <- ifelse(data_gte$AGE=="61<",1,0)

data_gte$us_born_yes <- ifelse(data_gte$US_BORN=="Yes",1,0)
data_gte$us_born_no <- ifelse(data_gte$US_BORN=="No",1,0)

data_gte$center_bronx <- ifelse(data_gte$CENTER=="Bronx",1,0)
data_gte$center_miami <- ifelse(data_gte$CENTER=="Miami",1,0)
data_gte$center_chicago <- ifelse(data_gte$CENTER=="Chicago",1,0)
data_gte$center_sandiego <- ifelse(data_gte$CENTER=="San Diego",1,0)

# marital status
data_gte$Marital_status_single <- ifelse(data_gte$Marital_status=="Single",1,0)
data_gte$Marital_status_partner <- ifelse(data_gte$Marital_status=="Married or living with a partner",1,0)
data_gte$Marital_status_separate <- ifelse(data_gte$Marital_status=="Separated,divorced,or widow(er)",1,0)



# education level
data_gte$edu_less_12 <- ifelse(data_gte$Education=="<12",1,0)
data_gte$edu_12 <- ifelse(data_gte$Education=="12",1,0)
data_gte$edu_more_12 <- ifelse(data_gte$Education==">12",1,0)
data_gte$edu_more_12_doc <- ifelse(data_gte$Education=="Masters, doctoral, professional",1,0)

survey_gte <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~IPW , data=data_gte)
survey_trim <- trimWeights(survey_gte,upper = 30)
```

# Create a function of computing p_value from chi-square test

```r
contrast_OR_pval <- function(contrast, model){
  
  contrast <- as.data.frame(contrast)[1,]
	t_stat <- as.numeric(abs(contrast[1]/contrast[2]))
	OR <- as.numeric(exp(contrast[1]))
	df <- model$df.residual
	Z <- qt(0.975,df = df, ncp=0, lower.tail = TRUE)
	pval <- (1-pt(t_stat, df =df, lower.tail = TRUE))*2
	CI <- c(as.numeric(contrast[1] - Z*contrast[2]), as.numeric(contrast[1] + Z*contrast[2]))
	return(list(OR= round(OR,2), pval = pval, CI=  round(exp(CI),2)))
}
```

## Education comparison

```r
model_aware_edu <- svyglm(aware~AGE+SEX+edu_less_12+edu_12+ edu_more_12 + 
                                Income_level+ 
                                CENTER  + 
                               Current_Health_insurance+
                               Physician_Visit+Background+Language_pref+
                               Marital_status+Employment_status+US_BORN,
                             design=survey_trim,na.action=na.omit,family=quasibinomial())

# compare 12 years of education to less then 12:
contrast <- svycontrast(model_aware_edu, list(diff=c(edu_12=1, edu_less_12=-1)))
contrast_OR_pval(contrast,model_aware_edu)
```

```
## $OR
## [1] 1.04
## 
## $pval
## [1] 0.7425194
## 
## $CI
## [1] 0.83 1.30
```

```r
# compare more than 12 years (but not MA, doctoral, or professional degree) of education to less then:
contrast <- svycontrast(model_aware_edu, list(diff=c(edu_more_12=1, edu_less_12=-1)))
contrast_OR_pval(contrast,model_aware_edu)
```

```
## $OR
## [1] 1.65
## 
## $pval
## [1] 5.524459e-06
## 
## $CI
## [1] 1.33 2.05
```

```r
model_aware_edu <- svyglm(aware~AGE+SEX+edu_less_12 + edu_12+ edu_more_12_doc + 
                                Income_level+ 
                                CENTER  + 
                               Current_Health_insurance+
                               Physician_Visit+Background+Language_pref+
                               Marital_status+Employment_status+US_BORN,
                             design=survey_trim,na.action=na.omit,family=quasibinomial())

# compare MA/doctoral/professional degree to less then 12:
contrast <- svycontrast(model_aware_edu, list(diff=c(edu_more_12_doc=1, edu_less_12=-1)))
contrast_OR_pval(contrast,model_aware_edu)
```

```
## $OR
## [1] 1.48
## 
## $pval
## [1] 0.1442355
## 
## $CI
## [1] 0.87 2.50
```

```r
# also compute proportion of awareness by education groups:
svyby(~aware, ~Education,survey_trim, svyciprop)
```

```
##                                                       Education     aware
## <12                                                         <12 0.4493532
## 12                                                           12 0.5120249
## >12                                                         >12 0.6620532
## Masters, doctoral, professional Masters, doctoral, professional 0.6510064
##                                 se.as.numeric(aware)
## <12                                       0.01799076
## 12                                        0.02147811
## >12                                       0.01619749
## Masters, doctoral, professional           0.05480584
```


## Miami and Chicago comparison

```r
model_aware_center <- svyglm(aware~AGE+SEX+Education+
                                Income_level+ 
                                center_chicago +
                               center_sandiego + center_miami + 
                               Current_Health_insurance+
                               Physician_Visit+Background+Language_pref+
                               Marital_status+Employment_status+US_BORN,
                             design=survey_trim,na.action=na.omit,family=quasibinomial())
contrast <- svycontrast(model_aware_center, list(diff=c(center_miami=1, center_chicago=-1)))
contrast_OR_pval(contrast,model_aware_center)
```

```
## $OR
## [1] 3.33
## 
## $pval
## [1] 4.176215e-12
## 
## $CI
## [1] 2.38 4.64
```

```r
# just reverse the order...
contrast <- svycontrast(model_aware_center, list(diff=c(center_chicago=-1, center_miami=1)))
contrast_OR_pval(contrast,model_aware_center)
```

```
## $OR
## [1] 3.33
## 
## $pval
## [1] 4.176215e-12
## 
## $CI
## [1] 2.38 4.64
```

```r
# compute proportions:
svyby(~aware, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER     aware se.as.numeric(aware)
## Bronx         Bronx 0.5171574           0.02058225
## Chicago     Chicago 0.3620596           0.01608420
## Miami         Miami 0.6723255           0.01924982
## San Diego San Diego 0.5838067           0.02250196
```

Number of complete cases used in the analysis: 5534.

## Marital status

```r
model_drug_marital <- svyglm(aware~AGE+SEX+Education+
                                Income_level+ 
                                CENTER + Language_pref  + 
                               Marital_status_separate + Marital_status_single + 
                               Current_Health_insurance+
                               Physician_Visit+Background+
                               Marital_status+Employment_status+US_BORN,
                            design=survey_trim,na.action=na.omit,family=quasibinomial())

contrast <- svycontrast(model_drug_marital, list(diff=c(Marital_status_separate=1, Marital_status_single=-1)))
contrast_OR_pval(contrast,model_drug_marital)
```

```
## $OR
## [1] 0.69
## 
## $pval
## [1] 0.01315782
## 
## $CI
## [1] 0.52 0.93
```

```r
# compute proportions:
svyby(~aware, ~Marital_status,survey_trim, svyciprop)
```

```
##                                                    Marital_status     aware
## Single                                                     Single 0.5953204
## Married or living with a partner Married or living with a partner 0.5477546
## Separated,divorced,or widow(er)   Separated,divorced,or widow(er) 0.4917325
##                                  se.as.numeric(aware)
## Single                                     0.01971144
## Married or living with a partner           0.01540975
## Separated,divorced,or widow(er)            0.02347220
```

## Income comparison

```r
model_aware_income <- svyglm(aware~AGE+SEX+Education+
                                income_10000 + income_75000_high + 
                               income_75000 + income_20000 + 
                                CENTER + 
                               Current_Health_insurance+
                               Physician_Visit+Background+Language_pref+
                               Marital_status+Employment_status+US_BORN,
                             design=survey_trim,na.action=na.omit,family=quasibinomial())
contrast <- svycontrast(model_aware_income, list(diff=c(income_75000_high=1, income_10000=-1)))
contrast_OR_pval(contrast,model_aware_income)
```

```
## $OR
## [1] 2.53
## 
## $pval
## [1] 7.807561e-05
## 
## $CI
## [1] 1.6 4.0
```

```r
# compute proportions:
svyby(~aware, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level     aware se.as.numeric(aware)
## Less than $10,000 Less than $10,000 0.4626093           0.03035271
## $10,001-$20,000     $10,001-$20,000 0.5051506           0.02203670
## $20,001-$40,000     $20,001-$40,000 0.5251300           0.01797876
## $40,001-$75,000     $40,001-$75,000 0.6248518           0.02431757
## More than $75,000 More than $75,000 0.7324657           0.03627677
```


# Contrasts based on specific genetic tests


## Drug efficacy: separated, divorced or widdower, versus married or living with a partner.


```r
model_drug_marital <- svyglm(GTE4~AGE+SEX+Education+
                                Income_level+ 
                                CENTER + Language_pref  + 
                               Marital_status_separate + Marital_status_partner + 
                               Current_Health_insurance+
                               Physician_Visit+Background+
                               Marital_status+Employment_status+US_BORN,
                            design=survey_trim,na.action=na.omit,family=quasibinomial())

contrast <- svycontrast(model_drug_marital, list(diff=c(Marital_status_separate=1, Marital_status_partner=-1)))
contrast_OR_pval(contrast,model_drug_marital)
```

```
## $OR
## [1] 0.66
## 
## $pval
## [1] 0.007634282
## 
## $CI
## [1] 0.48 0.89
```

```r
# compute proportions:
svyby(~GTE4, ~Marital_status,survey_trim, svyciprop)
```

```
##                                                    Marital_status      GTE4
## Single                                                     Single 0.1695308
## Married or living with a partner Married or living with a partner 0.1748229
## Separated,divorced,or widow(er)   Separated,divorced,or widow(er) 0.1162043
##                                  se.as.numeric(GTE4)
## Single                                           NaN
## Married or living with a partner                 NaN
## Separated,divorced,or widow(er)                  NaN
```

# separate versus single


```r
model_drug_marital <- svyglm(GTE4~AGE+SEX+Education+
                                Income_level+ 
                                CENTER + Language_pref  + 
                               Marital_status_separate + Marital_status_single + 
                               Current_Health_insurance+
                               Physician_Visit+Background+
                               Employment_status+US_BORN,
                            design=survey_trim,na.action=na.omit,family=quasibinomial())

contrast <- svycontrast(model_drug_marital, list(diff=c(Marital_status_separate=1, Marital_status_single=-1)))
contrast_OR_pval(contrast,model_drug_marital)
```

```
## $OR
## [1] 0.67
## 
## $pval
## [1] 0.0294514
## 
## $CI
## [1] 0.47 0.96
```

```r
# compute proportions:
svyby(~GTE4, ~Marital_status,survey_trim, svyciprop)
```

```
##                                                    Marital_status      GTE4
## Single                                                     Single 0.1695308
## Married or living with a partner Married or living with a partner 0.1748229
## Separated,divorced,or widow(er)   Separated,divorced,or widow(er) 0.1162043
##                                  se.as.numeric(GTE4)
## Single                                           NaN
## Married or living with a partner                 NaN
## Separated,divorced,or widow(er)                  NaN
```
## Compute proportions of population groups with awareness - disease risk


```r
# by sex
svyby(~GTE1, ~SEX,survey_trim, svyciprop)
```

```
##           SEX      GTE1 se.as.numeric(GTE1)
## Female Female 0.4114878                 NaN
## Male     Male 0.3942077                 NaN
```

```r
# by education
svyby(~GTE1, ~Education,survey_trim, svyciprop)
```

```
##                                                       Education      GTE1
## <12                                                         <12 0.2754395
## 12                                                           12 0.3796857
## >12                                                         >12 0.5141672
## Masters, doctoral, professional Masters, doctoral, professional 0.5816374
##                                 se.as.numeric(GTE1)
## <12                                             NaN
## 12                                              NaN
## >12                                             NaN
## Masters, doctoral, professional          0.05494958
```

```r
# by center
svyby(~GTE1, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER      GTE1 se.as.numeric(GTE1)
## Bronx         Bronx 0.3319264                 NaN
## Chicago     Chicago 0.2536024                 NaN
## Miami         Miami 0.5717308          0.02048405
## San Diego San Diego 0.3984586          0.02595676
```

```r
# by income
svyby(~GTE1, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level      GTE1 se.as.numeric(GTE1)
## Less than $10,000 Less than $10,000 0.3196356          0.02874497
## $10,001-$20,000     $10,001-$20,000 0.3537221                 NaN
## $20,001-$40,000     $20,001-$40,000 0.3641384                 NaN
## $40,001-$75,000     $40,001-$75,000 0.4731183                 NaN
## More than $75,000 More than $75,000 0.6552409                 NaN
```

```r
# by background
svyby(~GTE1, ~Background,survey_trim, svyciprop)
```

```
##                                                Background      GTE1
## Domician                                         Domician 0.3695419
## Central American                         Central American 0.3936569
## Cuban                                               Cuban 0.5810436
## Mexican                                           Mexican 0.3323356
## Puerto Rican                                 Puerto Rican 0.3445223
## South American                             South American 0.4470613
## More than one/Other heritage More than one/Other heritage 0.5507496
##                              se.as.numeric(GTE1)
## Domician                                     NaN
## Central American                      0.02730431
## Cuban                                 0.02716616
## Mexican                                      NaN
## Puerto Rican                                 NaN
## South American                        0.03611199
## More than one/Other heritage          0.05982984
```
## Compute proportions of population groups with awareness - risk to children


```r
# by sex
svyby(~GTE2, ~SEX,survey_trim, svyciprop)
```

```
##           SEX      GTE2 se.as.numeric(GTE2)
## Female Female 0.4158040                 NaN
## Male     Male 0.4260088                 NaN
```

```r
# by education
svyby(~GTE2, ~Education,survey_trim, svyciprop)
```

```
##                                                       Education      GTE2
## <12                                                         <12 0.3076711
## 12                                                           12 0.3937687
## >12                                                         >12 0.5212062
## Masters, doctoral, professional Masters, doctoral, professional 0.5486920
##                                 se.as.numeric(GTE2)
## <12                                             NaN
## 12                                              NaN
## >12                                             NaN
## Masters, doctoral, professional                 NaN
```

```r
# by center
svyby(~GTE2, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER      GTE2 se.as.numeric(GTE2)
## Bronx         Bronx 0.3707240                 NaN
## Chicago     Chicago 0.3063946                 NaN
## Miami         Miami 0.5101788                 NaN
## San Diego San Diego 0.4404479                 NaN
```

```r
# by income
svyby(~GTE2, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level      GTE2 se.as.numeric(GTE2)
## Less than $10,000 Less than $10,000 0.3314048                 NaN
## $10,001-$20,000     $10,001-$20,000 0.3660768                 NaN
## $20,001-$40,000     $20,001-$40,000 0.4013490                 NaN
## $40,001-$75,000     $40,001-$75,000 0.5064143                 NaN
## More than $75,000 More than $75,000 0.5891571                 NaN
```

```r
# by background
svyby(~GTE2, ~Background,survey_trim, svyciprop)
```

```
##                                                Background      GTE2
## Domician                                         Domician 0.3933999
## Central American                         Central American 0.4440080
## Cuban                                               Cuban 0.5204741
## Mexican                                           Mexican 0.3908148
## Puerto Rican                                 Puerto Rican 0.3403444
## South American                             South American 0.4099970
## More than one/Other heritage More than one/Other heritage 0.6081888
##                              se.as.numeric(GTE2)
## Domician                                     NaN
## Central American                             NaN
## Cuban                                        NaN
## Mexican                                      NaN
## Puerto Rican                                 NaN
## South American                               NaN
## More than one/Other heritage                 NaN
```



## Compute proportions of population groups with awareness - personalized treatment


```r
# by sex
svyby(~GTE3, ~SEX,survey_trim, svyciprop)
```

```
##           SEX      GTE3 se.as.numeric(GTE3)
## Female Female 0.1589712                 NaN
## Male     Male 0.1745237                 NaN
```

```r
# by education
svyby(~GTE3, ~Education,survey_trim, svyciprop)
```

```
##                                                       Education      GTE3
## <12                                                         <12 0.1240232
## 12                                                           12 0.1497668
## >12                                                         >12 0.2001312
## Masters, doctoral, professional Masters, doctoral, professional 0.2864162
##                                 se.as.numeric(GTE3)
## <12                                             NaN
## 12                                              NaN
## >12                                             NaN
## Masters, doctoral, professional                 NaN
```

```r
# by center
svyby(~GTE3, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER      GTE3 se.as.numeric(GTE3)
## Bronx         Bronx 0.1748524                 NaN
## Chicago     Chicago 0.1494542                 NaN
## Miami         Miami 0.1626774                 NaN
## San Diego San Diego 0.1693284                 NaN
```

```r
# by income
svyby(~GTE3, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level      GTE3 se.as.numeric(GTE3)
## Less than $10,000 Less than $10,000 0.1589200                 NaN
## $10,001-$20,000     $10,001-$20,000 0.1511674                 NaN
## $20,001-$40,000     $20,001-$40,000 0.1428343                 NaN
## $40,001-$75,000     $40,001-$75,000 0.1830376                 NaN
## More than $75,000 More than $75,000 0.2710199                 NaN
```

```r
# by background
svyby(~GTE3, ~Background,survey_trim, svyciprop)
```

```
##                                                Background      GTE3
## Domician                                         Domician 0.1683898
## Central American                         Central American 0.1326006
## Cuban                                               Cuban 0.1677292
## Mexican                                           Mexican 0.1596939
## Puerto Rican                                 Puerto Rican 0.1837431
## South American                             South American 0.1206308
## More than one/Other heritage More than one/Other heritage 0.2558933
##                              se.as.numeric(GTE3)
## Domician                                     NaN
## Central American                             NaN
## Cuban                                        NaN
## Mexican                                      NaN
## Puerto Rican                                 NaN
## South American                               NaN
## More than one/Other heritage                 NaN
```

```r
# by age
svyby(~GTE3, ~AGE,survey_trim, svyciprop)
```

```
##         AGE      GTE3 se.as.numeric(GTE3)
## <40     <40 0.1429304                 NaN
## 41-60 41-60 0.1813112                 NaN
## 61<     61< 0.1691693                 NaN
```

```r
# by employment status
svyby(~GTE3, ~Employment_status,survey_trim, svyciprop)
```

```
##                                                       Employment_status
## Retired/not currently employed           Retired/not currently employed
## Employed part-time(<=35 hours/week) Employed part-time(<=35 hours/week)
## Employed full-time(>35 hours/week)   Employed full-time(>35 hours/week)
##                                          GTE3 se.as.numeric(GTE3)
## Retired/not currently employed      0.1745706                 NaN
## Employed part-time(<=35 hours/week) 0.1700972                 NaN
## Employed full-time(>35 hours/week)  0.1576054                 NaN
```




## Compute proportions of population groups with awareness - drug efficacy


```r
# by sex
svyby(~GTE4, ~SEX,survey_trim, svyciprop)
```

```
##           SEX      GTE4 se.as.numeric(GTE4)
## Female Female 0.1601912                 NaN
## Male     Male 0.1674058                 NaN
```

```r
# by education
svyby(~GTE4, ~Education,survey_trim, svyciprop)
```

```
##                                                       Education      GTE4
## <12                                                         <12 0.1179092
## 12                                                           12 0.1716749
## >12                                                         >12 0.1813488
## Masters, doctoral, professional Masters, doctoral, professional 0.2914983
##                                 se.as.numeric(GTE4)
## <12                                             NaN
## 12                                              NaN
## >12                                             NaN
## Masters, doctoral, professional                 NaN
```

```r
# by center
svyby(~GTE4, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER      GTE4 se.as.numeric(GTE4)
## Bronx         Bronx 0.1510880                 NaN
## Chicago     Chicago 0.1520174                 NaN
## Miami         Miami 0.1467026                 NaN
## San Diego San Diego 0.1996745                 NaN
```

```r
# by income
svyby(~GTE4, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level      GTE4 se.as.numeric(GTE4)
## Less than $10,000 Less than $10,000 0.1445090                 NaN
## $10,001-$20,000     $10,001-$20,000 0.1531427                 NaN
## $20,001-$40,000     $20,001-$40,000 0.1422149                 NaN
## $40,001-$75,000     $40,001-$75,000 0.1782495                 NaN
## More than $75,000 More than $75,000 0.2540401                 NaN
```

```r
# by background
svyby(~GTE4, ~Background,survey_trim, svyciprop)
```

```
##                                                Background      GTE4
## Domician                                         Domician 0.1384365
## Central American                         Central American 0.1382747
## Cuban                                               Cuban 0.1497800
## Mexican                                           Mexican 0.1800041
## Puerto Rican                                 Puerto Rican 0.1651517
## South American                             South American 0.1154172
## More than one/Other heritage More than one/Other heritage 0.2339030
##                              se.as.numeric(GTE4)
## Domician                                     NaN
## Central American                             NaN
## Cuban                                        NaN
## Mexican                                      NaN
## Puerto Rican                                 NaN
## South American                               NaN
## More than one/Other heritage                 NaN
```

```r
# by age
svyby(~GTE4, ~AGE,survey_trim, svyciprop)
```

```
##         AGE      GTE4 se.as.numeric(GTE4)
## <40     <40 0.1625295                 NaN
## 41-60 41-60 0.1724178                 NaN
## 61<     61< 0.1505052                 NaN
```

```r
# by employment status
svyby(~GTE4, ~Employment_status,survey_trim, svyciprop)
```

```
##                                                       Employment_status
## Retired/not currently employed           Retired/not currently employed
## Employed part-time(<=35 hours/week) Employed part-time(<=35 hours/week)
## Employed full-time(>35 hours/week)   Employed full-time(>35 hours/week)
##                                          GTE4 se.as.numeric(GTE4)
## Retired/not currently employed      0.1672361                 NaN
## Employed part-time(<=35 hours/week) 0.1592523                 NaN
## Employed full-time(>35 hours/week)  0.1597806                 NaN
```

```r
# by language preference
svyby(~GTE4, ~Language_pref,survey_trim, svyciprop)
```

```
##         Language_pref      GTE4 se.as.numeric(GTE4)
## Spanish       Spanish 0.1461173                 NaN
## English       English 0.2144807                 NaN
```



