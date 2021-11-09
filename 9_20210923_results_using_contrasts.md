---
title: "Usings contrasts to provide effect estimates and CIs effects we may want to report"
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
base_path <- "~/Dropbox (Partners HealthCare)/SOL_misc_genetics/survey_gen_test_utilization/20210917_data_code/"
data_file <- paste0(base_path, "Processed_data/20210918_gte_data_set_with_covariates_and_IPW.RData")
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
model_aware_edu <- svyglm(aware~AGE+SEX+edu_less_12+edu_more_12 + 
                                Income_level+ 
                                CENTER  + 
                               Current_Health_insurance+
                               Physician_Visit+Background+Language_pref+
                               Marital_status+Employment_status+US_BORN,
                             design=survey_trim,na.action=na.omit,family=quasibinomial())
contrast <- svycontrast(model_aware_edu, list(diff=c(edu_more_12=1, edu_less_12=-1)))
contrast_OR_pval(contrast,model_aware_edu)
```

```
## $OR
## [1] 1.58
## 
## $pval
## [1] 3.398031e-05
## 
## $CI
## [1] 1.27 1.96
```

```r
# also compute proportion of awareness by education groups:
svyby(~aware, ~Education,survey_trim, svyciprop)
```

```
##     Education     aware se.as.numeric(aware)
## <12       <12 0.4517998           0.01859569
## 12         12 0.4944836           0.02273657
## >12       >12 0.6532583           0.01546592
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
## [1] 3.39
## 
## $pval
## [1] 1.31184e-12
## 
## $CI
## [1] 2.44 4.71
```

```r
# just reverse the order...
contrast <- svycontrast(model_aware_center, list(diff=c(center_chicago=-1, center_miami=1)))
contrast_OR_pval(contrast,model_aware_center)
```

```
## $OR
## [1] 3.39
## 
## $pval
## [1] 1.31184e-12
## 
## $CI
## [1] 2.44 4.71
```

```r
# compute proportions:
svyby(~aware, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER     aware se.as.numeric(aware)
## Bronx         Bronx 0.5167739           0.02061809
## Chicago     Chicago 0.3615750           0.01609662
## Miami         Miami 0.6715606           0.01942201
## San Diego San Diego 0.5836513           0.02246811
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
## [1] 0.71
## 
## $pval
## [1] 0.01807148
## 
## $CI
## [1] 0.53 0.94
```

```r
# compute proportions:
svyby(~aware, ~Marital_status,survey_trim, svyciprop)
```

```
##                                                    Marital_status
## Single                                                     Single
## Married or living with a partner Married or living with a partner
## Separated,divorced,or widow(er)   Separated,divorced,or widow(er)
##                                      aware
## Single                           0.5947946
## Married or living with a partner 0.5476079
## Separated,divorced,or widow(er)  0.4910023
##                                  se.as.numeric(aware)
## Single                                     0.01966641
## Married or living with a partner           0.01540733
## Separated,divorced,or widow(er)            0.02352735
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
## [1] 2.56
## 
## $pval
## [1] 6.282515e-05
## 
## $CI
## [1] 1.62 4.04
```

```r
# compute proportions:
svyby(~aware, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level     aware
## Less than $10,000 Less than $10,000 0.4633876
## $10,001-$20,000     $10,001-$20,000 0.5042506
## $20,001-$40,000     $20,001-$40,000 0.5242943
## $40,001-$75,000     $40,001-$75,000 0.6238851
## More than $75,000 More than $75,000 0.7332169
##                   se.as.numeric(aware)
## Less than $10,000           0.03016985
## $10,001-$20,000             0.02196707
## $20,001-$40,000             0.01799378
## $40,001-$75,000             0.02432153
## More than $75,000           0.03632199
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
## [1] 0.007215276
## 
## $CI
## [1] 0.48 0.89
```

```r
# compute proportions:
svyby(~GTE4, ~Marital_status,survey_trim, svyciprop)
```

```
##                                                    Marital_status
## Single                                                     Single
## Married or living with a partner Married or living with a partner
## Separated,divorced,or widow(er)   Separated,divorced,or widow(er)
##                                       GTE4
## Single                           0.1689212
## Married or living with a partner 0.1747190
## Separated,divorced,or widow(er)  0.1160176
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
## [1] 0.69
## 
## $pval
## [1] 0.03974951
## 
## $CI
## [1] 0.48 0.98
```

```r
# compute proportions:
svyby(~GTE4, ~Marital_status,survey_trim, svyciprop)
```

```
##                                                    Marital_status
## Single                                                     Single
## Married or living with a partner Married or living with a partner
## Separated,divorced,or widow(er)   Separated,divorced,or widow(er)
##                                       GTE4
## Single                           0.1689212
## Married or living with a partner 0.1747190
## Separated,divorced,or widow(er)  0.1160176
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
## Female Female 0.4104277                 NaN
## Male     Male 0.3946399                 NaN
```

```r
# by education
svyby(~GTE1, ~Education,survey_trim, svyciprop)
```

```
##     Education      GTE1 se.as.numeric(GTE1)
## <12       <12 0.2763596                 NaN
## 12         12 0.3576416                 NaN
## >12       >12 0.5136970                 NaN
```

```r
# by center
svyby(~GTE1, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER      GTE1 se.as.numeric(GTE1)
## Bronx         Bronx 0.3313129                 NaN
## Chicago     Chicago 0.2531364                 NaN
## Miami         Miami 0.5713755          0.02051327
## San Diego San Diego 0.3981191          0.02597002
```

```r
# by income
svyby(~GTE1, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level      GTE1
## Less than $10,000 Less than $10,000 0.3194358
## $10,001-$20,000     $10,001-$20,000 0.3532064
## $20,001-$40,000     $20,001-$40,000 0.3634241
## $40,001-$75,000     $40,001-$75,000 0.4720822
## More than $75,000 More than $75,000 0.6566448
##                   se.as.numeric(GTE1)
## Less than $10,000          0.02848772
## $10,001-$20,000                   NaN
## $20,001-$40,000                   NaN
## $40,001-$75,000                   NaN
## More than $75,000                 NaN
```

```r
# by background
svyby(~GTE1, ~Background,survey_trim, svyciprop)
```

```
##                                                Background
## Domician                                         Domician
## Central American                         Central American
## Cuban                                               Cuban
## Mexican                                           Mexican
## Puerto Rican                                 Puerto Rican
## South American                             South American
## More than one/Other heritage More than one/Other heritage
##                                   GTE1
## Domician                     0.3699023
## Central American             0.3938251
## Cuban                        0.5809529
## Mexican                      0.3315306
## Puerto Rican                 0.3450332
## South American               0.4467998
## More than one/Other heritage 0.5478994
##                              se.as.numeric(GTE1)
## Domician                                     NaN
## Central American                      0.02736747
## Cuban                                 0.02710892
## Mexican                                      NaN
## Puerto Rican                                 NaN
## South American                        0.03620383
## More than one/Other heritage          0.05999058
```
## Compute proportions of population groups with awareness - risk to children


```r
# by sex
svyby(~GTE2, ~SEX,survey_trim, svyciprop)
```

```
##           SEX      GTE2 se.as.numeric(GTE2)
## Female Female 0.4151316                 NaN
## Male     Male 0.4260229                 NaN
```

```r
# by education
svyby(~GTE2, ~Education,survey_trim, svyciprop)
```

```
##     Education      GTE2 se.as.numeric(GTE2)
## <12       <12 0.3095591                 NaN
## 12         12 0.3786128                 NaN
## >12       >12 0.5151993                 NaN
```

```r
# by center
svyby(~GTE2, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER      GTE2 se.as.numeric(GTE2)
## Bronx         Bronx 0.3692942                 NaN
## Chicago     Chicago 0.3058093                 NaN
## Miami         Miami 0.5108059                 NaN
## San Diego San Diego 0.4402363                 NaN
```

```r
# by income
svyby(~GTE2, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level      GTE2
## Less than $10,000 Less than $10,000 0.3310985
## $10,001-$20,000     $10,001-$20,000 0.3651009
## $20,001-$40,000     $20,001-$40,000 0.4009723
## $40,001-$75,000     $40,001-$75,000 0.5049705
## More than $75,000 More than $75,000 0.5903363
##                   se.as.numeric(GTE2)
## Less than $10,000                 NaN
## $10,001-$20,000                   NaN
## $20,001-$40,000                   NaN
## $40,001-$75,000                   NaN
## More than $75,000                 NaN
```

```r
# by background
svyby(~GTE2, ~Background,survey_trim, svyciprop)
```

```
##                                                Background
## Domician                                         Domician
## Central American                         Central American
## Cuban                                               Cuban
## Mexican                                           Mexican
## Puerto Rican                                 Puerto Rican
## South American                             South American
## More than one/Other heritage More than one/Other heritage
##                                   GTE2
## Domician                     0.3939627
## Central American             0.4449519
## Cuban                        0.5202595
## Mexican                      0.3906060
## Puerto Rican                 0.3387491
## South American               0.4097775
## More than one/Other heritage 0.6065422
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
## Female Female 0.1589242                 NaN
## Male     Male 0.1745029                 NaN
```

```r
# by education
svyby(~GTE3, ~Education,survey_trim, svyciprop)
```

```
##     Education      GTE3 se.as.numeric(GTE3)
## <12       <12 0.1275266                 NaN
## 12         12 0.1426448                 NaN
## >12       >12 0.2044373                 NaN
```

```r
# by center
svyby(~GTE3, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER      GTE3 se.as.numeric(GTE3)
## Bronx         Bronx 0.1747750                 NaN
## Chicago     Chicago 0.1492756                 NaN
## Miami         Miami 0.1627011                 NaN
## San Diego San Diego 0.1693442                 NaN
```

```r
# by income
svyby(~GTE3, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level      GTE3
## Less than $10,000 Less than $10,000 0.1579380
## $10,001-$20,000     $10,001-$20,000 0.1513706
## $20,001-$40,000     $20,001-$40,000 0.1426838
## $40,001-$75,000     $40,001-$75,000 0.1827152
## More than $75,000 More than $75,000 0.2724649
##                   se.as.numeric(GTE3)
## Less than $10,000                 NaN
## $10,001-$20,000                   NaN
## $20,001-$40,000                   NaN
## $40,001-$75,000                   NaN
## More than $75,000                 NaN
```

```r
# by background
svyby(~GTE3, ~Background,survey_trim, svyciprop)
```

```
##                                                Background
## Domician                                         Domician
## Central American                         Central American
## Cuban                                               Cuban
## Mexican                                           Mexican
## Puerto Rican                                 Puerto Rican
## South American                             South American
## More than one/Other heritage More than one/Other heritage
##                                   GTE3
## Domician                     0.1691558
## Central American             0.1316158
## Cuban                        0.1685843
## Mexican                      0.1594740
## Puerto Rican                 0.1829260
## South American               0.1203919
## More than one/Other heritage 0.2544136
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
## <40     <40 0.1427786                 NaN
## 41-60 41-60 0.1810295                 NaN
## 61<     61< 0.1694944                 NaN
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
##                                          GTE3
## Retired/not currently employed      0.1746178
## Employed part-time(<=35 hours/week) 0.1694084
## Employed full-time(>35 hours/week)  0.1579179
##                                     se.as.numeric(GTE3)
## Retired/not currently employed                      NaN
## Employed part-time(<=35 hours/week)                 NaN
## Employed full-time(>35 hours/week)                  NaN
```




## Compute proportions of population groups with awareness - drug efficacy


```r
# by sex
svyby(~GTE4, ~SEX,survey_trim, svyciprop)
```

```
##           SEX      GTE4 se.as.numeric(GTE4)
## Female Female 0.1599809                 NaN
## Male     Male 0.1672915                 NaN
```

```r
# by education
svyby(~GTE4, ~Education,survey_trim, svyciprop)
```

```
##     Education      GTE4 se.as.numeric(GTE4)
## <12       <12 0.1192162                 NaN
## 12         12 0.1578648                 NaN
## >12       >12 0.1945386                 NaN
```

```r
# by center
svyby(~GTE4, ~CENTER,survey_trim, svyciprop)
```

```
##              CENTER      GTE4 se.as.numeric(GTE4)
## Bronx         Bronx 0.1507655                 NaN
## Chicago     Chicago 0.1516679                 NaN
## Miami         Miami 0.1463985                 NaN
## San Diego San Diego 0.1999016                 NaN
```

```r
# by income
svyby(~GTE4, ~Income_level,survey_trim, svyciprop)
```

```
##                        Income_level      GTE4
## Less than $10,000 Less than $10,000 0.1430074
## $10,001-$20,000     $10,001-$20,000 0.1536764
## $20,001-$40,000     $20,001-$40,000 0.1420059
## $40,001-$75,000     $40,001-$75,000 0.1771211
## More than $75,000 More than $75,000 0.2556627
##                   se.as.numeric(GTE4)
## Less than $10,000                 NaN
## $10,001-$20,000                   NaN
## $20,001-$40,000                   NaN
## $40,001-$75,000                   NaN
## More than $75,000                 NaN
```

```r
# by background
svyby(~GTE4, ~Background,survey_trim, svyciprop)
```

```
##                                                Background
## Domician                                         Domician
## Central American                         Central American
## Cuban                                               Cuban
## Mexican                                           Mexican
## Puerto Rican                                 Puerto Rican
## South American                             South American
## More than one/Other heritage More than one/Other heritage
##                                   GTE4
## Domician                     0.1384473
## Central American             0.1376041
## Cuban                        0.1499682
## Mexican                      0.1798135
## Puerto Rican                 0.1644401
## South American               0.1148411
## More than one/Other heritage 0.2326564
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
## <40     <40 0.1620918                 NaN
## 41-60 41-60 0.1721683                 NaN
## 61<     61< 0.1507700                 NaN
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
##                                          GTE4
## Retired/not currently employed      0.1671314
## Employed part-time(<=35 hours/week) 0.1592132
## Employed full-time(>35 hours/week)  0.1593468
##                                     se.as.numeric(GTE4)
## Retired/not currently employed                      NaN
## Employed part-time(<=35 hours/week)                 NaN
## Employed full-time(>35 hours/week)                  NaN
```

```r
# by language preference
svyby(~GTE4, ~Language_pref,survey_trim, svyciprop)
```

```
##         Language_pref      GTE4 se.as.numeric(GTE4)
## Spanish       Spanish 0.1457044                 NaN
## English       English 0.2152430                 NaN
```



