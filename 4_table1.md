---
title: "Generating Table 1"
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
```

# Loading the data and weight the data with IPW (trim the weights at 30)

```r
data_gte <- readRDS(data_file)
nrow(data_gte)
```

```
## [1] 5769
```

```r
survey_gte <- svydesign(id=~PSU_ID, strata=~STRAT, weights=~IPW , data=data_gte)
survey_trim <- trimWeights(survey_gte,upper = 30)
```


# Generate the table without stratification

```r
tbl1_var_1<-c("AGE","SEX","Education","Income_level","Current_Health_insurance","Physician_Visit", "Background", "Language_pref", "Marital_status","Employment_status", "US_BORN")
tb1_noweight<- print(CreateTableOne(vars = tbl1_var_1,data =data_gte ),missing=TRUE,varLabels = TRUE,digits =3,pDigits=3,showAllLevels=TRUE)
```

```
##                               
##                                level                              
##   n                                                               
##   AGE (%)                      <40                                
##                                41-60                              
##                                61<                                
##   SEX (%)                      Female                             
##                                Male                               
##   Education (%)                <12                                
##                                12                                 
##                                >12                                
##                                Masters, doctoral, professional    
##   Income_level (%)             Less than $10,000                  
##                                $10,001-$20,000                    
##                                $20,001-$40,000                    
##                                $40,001-$75,000                    
##                                More than $75,000                  
##   Current_Health_insurance (%) No                                 
##                                Yes                                
##   Physician_Visit (%)          No                                 
##                                One or two times                   
##                                At least three times               
##   Background (%)               Domician                           
##                                Central American                   
##                                Cuban                              
##                                Mexican                            
##                                Puerto Rican                       
##                                South American                     
##                                More than one/Other heritage       
##   Language_pref (%)            Spanish                            
##                                English                            
##   Marital_status (%)           Single                             
##                                Married or living with a partner   
##                                Separated,divorced,or widow(er)    
##   Employment_status (%)        Retired/not currently employed     
##                                Employed part-time(<=35 hours/week)
##                                Employed full-time(>35 hours/week) 
##   US_BORN (%)                  No                                 
##                                Yes                                
##                               
##                                Overall      Missing
##   n                            5769                
##   AGE (%)                      1000 (17.3)  0.0    
##                                2674 (46.4)         
##                                2095 (36.3)         
##   SEX (%)                      3701 (64.2)  0.0    
##                                2068 (35.8)         
##   Education (%)                2004 (34.9)  0.4    
##                                1458 (25.4)         
##                                2078 (36.2)         
##                                 206 ( 3.6)         
##   Income_level (%)              613 (10.9)  2.5    
##                                1532 (27.2)         
##                                1999 (35.5)         
##                                1039 (18.5)         
##                                 441 ( 7.8)         
##   Current_Health_insurance (%) 1686 (29.3)  0.3    
##                                4068 (70.7)         
##   Physician_Visit (%)          1521 (26.7)  1.3    
##                                1820 (32.0)         
##                                2352 (41.3)         
##   Background (%)                413 ( 7.2)  0.3    
##                                 575 (10.0)         
##                                 593 (10.3)         
##                                2753 (47.9)         
##                                 820 (14.3)         
##                                 424 ( 7.4)         
##                                 172 ( 3.0)         
##   Language_pref (%)            4731 (82.0)  0.0    
##                                1038 (18.0)         
##   Marital_status (%)           1234 (21.4)  0.1    
##                                3288 (57.1)         
##                                1240 (21.5)         
##   Employment_status (%)        2362 (41.1)  0.3    
##                                1318 (22.9)         
##                                2071 (36.0)         
##   US_BORN (%)                  4835 (84.0)  0.3    
##                                 918 (16.0)
```

```r
tbl1_weighted<- print(svyCreateTableOne(vars = tbl1_var_1,  data =survey_trim), missing=TRUE, varLabels = TRUE,digits =3,pDigits=3, showAllLevels=TRUE)
```

```
##                               
##                                level                              
##   n                                                               
##   AGE (%)                      <40                                
##                                41-60                              
##                                61<                                
##   SEX (%)                      Female                             
##                                Male                               
##   Education (%)                <12                                
##                                12                                 
##                                >12                                
##                                Masters, doctoral, professional    
##   Income_level (%)             Less than $10,000                  
##                                $10,001-$20,000                    
##                                $20,001-$40,000                    
##                                $40,001-$75,000                    
##                                More than $75,000                  
##   Current_Health_insurance (%) No                                 
##                                Yes                                
##   Physician_Visit (%)          No                                 
##                                One or two times                   
##                                At least three times               
##   Background (%)               Domician                           
##                                Central American                   
##                                Cuban                              
##                                Mexican                            
##                                Puerto Rican                       
##                                South American                     
##                                More than one/Other heritage       
##   Language_pref (%)            Spanish                            
##                                English                            
##   Marital_status (%)           Single                             
##                                Married or living with a partner   
##                                Separated,divorced,or widow(er)    
##   Employment_status (%)        Retired/not currently employed     
##                                Employed part-time(<=35 hours/week)
##                                Employed full-time(>35 hours/week) 
##   US_BORN (%)                  No                                 
##                                Yes                                
##                               
##                                Overall         Missing
##   n                            16342.7                
##   AGE (%)                       5037.7 (30.8)  0.0    
##                                 7027.6 (43.0)         
##                                 4277.5 (26.2)         
##   SEX (%)                       8554.2 (52.3)  0.0    
##                                 7788.5 (47.7)         
##   Education (%)                 5149.0 (31.6)  0.4    
##                                 4468.1 (27.4)         
##                                 6102.2 (37.5)         
##                                  570.6 ( 3.5)         
##   Income_level (%)              1734.3 (10.9)  2.5    
##                                 4308.9 (27.2)         
##                                 5343.7 (33.7)         
##                                 2958.1 (18.7)         
##                                 1512.6 ( 9.5)         
##   Current_Health_insurance (%)  5128.6 (31.5)  0.3    
##                                11139.1 (68.5)         
##   Physician_Visit (%)           5145.2 (32.1)  1.3    
##                                 4794.2 (29.9)         
##                                 6101.4 (38.0)         
##   Background (%)                1601.1 ( 9.8)  0.3    
##                                 1119.5 ( 6.9)         
##                                 3144.5 (19.3)         
##                                 6247.8 (38.4)         
##                                 2752.4 (16.9)         
##                                  828.4 ( 5.1)         
##                                  597.1 ( 3.7)         
##   Language_pref (%)            12185.9 (74.6)  0.0    
##                                 4156.8 (25.4)         
##   Marital_status (%)            4958.4 (30.4)  0.1    
##                                 8516.7 (52.2)         
##                                 2847.6 (17.4)         
##   Employment_status (%)         6407.3 (39.4)  0.3    
##                                 3443.6 (21.2)         
##                                 6406.3 (39.4)         
##   US_BORN (%)                  12551.8 (77.0)  0.3    
##                                 3744.4 (23.0)
```
Combine the count values (from the unweighted table) and percentage (from the weighted table) together


```r
tbl1_w <- tbl1_weighted 
tbl1 <- tb1_noweight

tbl1_comb <- tbl1
# ignore the "Miami" typo...
col_ind_to_update <- which(colnames(tbl1_w) %in% c("Overall"))

# update tbl1_comb with the percentages from the weighted table
for (i in col_ind_to_update){
  counts <- sapply(tbl1[,i], function(x){
                  strsplit(x, split = "(", fixed = TRUE)[[1]][1]
                 })
  percnt <- sapply(tbl1_w[,i], function(x){
                paste0("(",  strsplit(x, split = "(", fixed = TRUE)[[1]][2])
              })
  tbl1_comb[,i] <- paste0(counts, percnt)
}

  
write.csv(tbl1_comb,paste0(output_dir,"/tbl1_unstratified.csv"))
```

# Generate a table stratified by study center
The table follows the same pattern as the unstratified table.


```r
tb1_noweight_center <- print(CreateTableOne(vars = tbl1_var_1,strata = "CENTER",data = data_gte),missing=TRUE,varLabels = TRUE,digits =3,pDigits=3,showAllLevels=TRUE)
```

```
##                               Stratified by CENTER
##                                level                              
##   n                                                               
##   AGE (%)                      <40                                
##                                41-60                              
##                                61<                                
##   SEX (%)                      Female                             
##                                Male                               
##   Education (%)                <12                                
##                                12                                 
##                                >12                                
##                                Masters, doctoral, professional    
##   Income_level (%)             Less than $10,000                  
##                                $10,001-$20,000                    
##                                $20,001-$40,000                    
##                                $40,001-$75,000                    
##                                More than $75,000                  
##   Current_Health_insurance (%) No                                 
##                                Yes                                
##   Physician_Visit (%)          No                                 
##                                One or two times                   
##                                At least three times               
##   Background (%)               Domician                           
##                                Central American                   
##                                Cuban                              
##                                Mexican                            
##                                Puerto Rican                       
##                                South American                     
##                                More than one/Other heritage       
##   Language_pref (%)            Spanish                            
##                                English                            
##   Marital_status (%)           Single                             
##                                Married or living with a partner   
##                                Separated,divorced,or widow(er)    
##   Employment_status (%)        Retired/not currently employed     
##                                Employed part-time(<=35 hours/week)
##                                Employed full-time(>35 hours/week) 
##   US_BORN (%)                  No                                 
##                                Yes                                
##                               Stratified by CENTER
##                                Bronx        Chicago     
##   n                            1030         1950        
##   AGE (%)                       198 (19.2)   377 (19.3) 
##                                 442 (42.9)   916 (47.0) 
##                                 390 (37.9)   657 (33.7) 
##   SEX (%)                       679 (65.9)  1163 (59.6) 
##                                 351 (34.1)   787 (40.4) 
##   Education (%)                 381 (37.2)   894 (46.0) 
##                                 258 (25.2)   494 (25.4) 
##                                 345 (33.7)   516 (26.6) 
##                                  41 ( 4.0)    38 ( 2.0) 
##   Income_level (%)              188 (19.0)   150 ( 7.9) 
##                                 316 (31.9)   497 (26.1) 
##                                 261 (26.3)   757 (39.8) 
##                                 146 (14.7)   369 (19.4) 
##                                  81 ( 8.2)   130 ( 6.8) 
##   Current_Health_insurance (%)  121 (11.9)   738 (37.8) 
##                                 899 (88.1)  1212 (62.2) 
##   Physician_Visit (%)           133 (13.5)   531 (27.3) 
##                                 309 (31.3)   635 (32.7) 
##                                 544 (55.2)   778 (40.0) 
##   Background (%)                380 (37.1)    15 ( 0.8) 
##                                  56 ( 5.5)   226 (11.6) 
##                                   8 ( 0.8)    17 ( 0.9) 
##                                  46 ( 4.5)  1093 (56.1) 
##                                 442 (43.2)   342 (17.6) 
##                                  44 ( 4.3)   201 (10.3) 
##                                  48 ( 4.7)    53 ( 2.7) 
##   Language_pref (%)             679 (65.9)  1676 (85.9) 
##                                 351 (34.1)   274 (14.1) 
##   Marital_status (%)            363 (35.3)   336 (17.2) 
##                                 421 (41.0)  1265 (64.9) 
##                                 244 (23.7)   349 (17.9) 
##   Employment_status (%)         507 (50.0)   714 (36.6) 
##                                 210 (20.7)   443 (22.7) 
##                                 298 (29.4)   793 (40.7) 
##   US_BORN (%)                   786 (76.7)  1684 (86.5) 
##                                 239 (23.3)   263 (13.5) 
##                               Stratified by CENTER
##                                Miami        San Diego   
##   n                            1065         1724        
##   AGE (%)                       137 (12.9)   288 (16.7) 
##                                 508 (47.7)   808 (46.9) 
##                                 420 (39.4)   628 (36.4) 
##   SEX (%)                       713 (66.9)  1146 (66.5) 
##                                 352 (33.1)   578 (33.5) 
##   Education (%)                 203 (19.1)   526 (30.7) 
##                                 282 (26.5)   424 (24.7) 
##                                 521 (49.0)   696 (40.6) 
##                                  57 ( 5.4)    70 ( 4.1) 
##   Income_level (%)              135 (13.0)   140 ( 8.3) 
##                                 334 (32.3)   385 (22.7) 
##                                 360 (34.8)   621 (36.7) 
##                                 150 (14.5)   374 (22.1) 
##                                  56 ( 5.4)   174 (10.3) 
##   Current_Health_insurance (%)  395 (37.1)   432 (25.1) 
##                                 669 (62.9)  1288 (74.9) 
##   Physician_Visit (%)           441 (42.0)   416 (24.3) 
##                                 276 (26.3)   600 (35.0) 
##                                 332 (31.6)   698 (40.7) 
##   Background (%)                 17 ( 1.6)     1 ( 0.1) 
##                                 271 (25.5)    22 ( 1.3) 
##                                 565 (53.2)     3 ( 0.2) 
##                                   8 ( 0.8)  1606 (93.6) 
##                                  19 ( 1.8)    17 ( 1.0) 
##                                 157 (14.8)    22 ( 1.3) 
##                                  26 ( 2.4)    45 ( 2.6) 
##   Language_pref (%)            1030 (96.7)  1346 (78.1) 
##                                  35 ( 3.3)   378 (21.9) 
##   Marital_status (%)            234 (22.0)   301 (17.5) 
##                                 532 (50.0)  1070 (62.2) 
##                                 298 (28.0)   349 (20.3) 
##   Employment_status (%)         427 (40.1)   714 (41.5) 
##                                 251 (23.6)   414 (24.1) 
##                                 387 (36.3)   593 (34.5) 
##   US_BORN (%)                  1017 (95.7)  1348 (78.5) 
##                                  46 ( 4.3)   370 (21.5) 
##                               Stratified by CENTER
##                                p      test Missing
##   n                                               
##   AGE (%)                      <0.001      0.0    
##                                                   
##                                                   
##   SEX (%)                      <0.001      0.0    
##                                                   
##   Education (%)                <0.001      0.4    
##                                                   
##                                                   
##                                                   
##   Income_level (%)             <0.001      2.5    
##                                                   
##                                                   
##                                                   
##                                                   
##   Current_Health_insurance (%) <0.001      0.3    
##                                                   
##   Physician_Visit (%)          <0.001      1.3    
##                                                   
##                                                   
##   Background (%)               <0.001      0.3    
##                                                   
##                                                   
##                                                   
##                                                   
##                                                   
##                                                   
##   Language_pref (%)            <0.001      0.0    
##                                                   
##   Marital_status (%)           <0.001      0.1    
##                                                   
##                                                   
##   Employment_status (%)        <0.001      0.3    
##                                                   
##                                                   
##   US_BORN (%)                  <0.001      0.3    
## 
```

```r
tbl1_weighted_center<- print(svyCreateTableOne(vars = tbl1_var_1, strata = "CENTER", data = survey_trim), missing=TRUE, varLabels = TRUE,digits =3,pDigits=3,showAllLevels=TRUE)
```

```
##                               Stratified by CENTER
##                                level                              
##   n                                                               
##   AGE (%)                      <40                                
##                                41-60                              
##                                61<                                
##   SEX (%)                      Female                             
##                                Male                               
##   Education (%)                <12                                
##                                12                                 
##                                >12                                
##                                Masters, doctoral, professional    
##   Income_level (%)             Less than $10,000                  
##                                $10,001-$20,000                    
##                                $20,001-$40,000                    
##                                $40,001-$75,000                    
##                                More than $75,000                  
##   Current_Health_insurance (%) No                                 
##                                Yes                                
##   Physician_Visit (%)          No                                 
##                                One or two times                   
##                                At least three times               
##   Background (%)               Domician                           
##                                Central American                   
##                                Cuban                              
##                                Mexican                            
##                                Puerto Rican                       
##                                South American                     
##                                More than one/Other heritage       
##   Language_pref (%)            Spanish                            
##                                English                            
##   Marital_status (%)           Single                             
##                                Married or living with a partner   
##                                Separated,divorced,or widow(er)    
##   Employment_status (%)        Retired/not currently employed     
##                                Employed part-time(<=35 hours/week)
##                                Employed full-time(>35 hours/week) 
##   US_BORN (%)                  No                                 
##                                Yes                                
##                               Stratified by CENTER
##                                Bronx          Chicago       
##   n                            4809.4         2629.4        
##   AGE (%)                      1621.9 (33.7)   988.6 (37.6) 
##                                1941.2 (40.4)  1122.1 (42.7) 
##                                1246.3 (25.9)   518.6 (19.7) 
##   SEX (%)                      2611.8 (54.3)  1265.5 (48.1) 
##                                2197.7 (45.7)  1363.9 (51.9) 
##   Education (%)                1905.3 (39.8)  1126.3 (42.9) 
##                                1227.9 (25.6)   788.3 (30.0) 
##                                1523.5 (31.8)   659.2 (25.1) 
##                                 133.5 ( 2.8)    50.7 ( 1.9) 
##   Income_level (%)              767.5 (16.6)   174.8 ( 6.8) 
##                                1538.3 (33.4)   619.5 (24.1) 
##                                1296.1 (28.1)   992.1 (38.5) 
##                                 689.2 (14.9)   573.5 (22.3) 
##                                 319.1 ( 6.9)   213.6 ( 8.3) 
##   Current_Health_insurance (%)  823.5 (17.3)  1153.2 (43.9) 
##                                3922.7 (82.7)  1476.2 (56.1) 
##   Physician_Visit (%)           920.2 (20.0)   873.4 (33.4) 
##                                1488.0 (32.4)   819.3 (31.3) 
##                                2190.2 (47.6)   921.3 (35.2) 
##   Background (%)               1523.8 (31.8)    13.5 ( 0.5) 
##                                 205.4 ( 4.3)   191.8 ( 7.3) 
##                                  59.7 ( 1.2)    39.4 ( 1.5) 
##                                 491.0 (10.3)  1633.7 (62.2) 
##                                2094.5 (43.8)   486.2 (18.5) 
##                                 184.2 ( 3.8)   183.1 ( 7.0) 
##                                 228.6 ( 4.8)    80.3 ( 3.1) 
##   Language_pref (%)            2821.5 (58.7)  2096.9 (79.7) 
##                                1988.0 (41.3)   532.5 (20.3) 
##   Marital_status (%)           2050.7 (42.7)   659.4 (25.1) 
##                                1934.4 (40.3)  1622.6 (61.7) 
##                                 812.1 (16.9)   347.4 (13.2) 
##   Employment_status (%)        2204.9 (46.6)   796.8 (30.3) 
##                                 907.6 (19.2)   598.6 (22.8) 
##                                1616.1 (34.2)  1234.0 (46.9) 
##   US_BORN (%)                  3341.3 (69.8)  2040.8 (77.7) 
##                                1449.0 (30.2)   587.4 (22.3) 
##                               Stratified by CENTER
##                                Miami          San Diego     
##   n                            4485.5         4418.4        
##   AGE (%)                       972.1 (21.7)  1455.0 (32.9) 
##                                2038.2 (45.4)  1926.1 (43.6) 
##                                1475.2 (32.9)  1037.3 (23.5) 
##   SEX (%)                      2376.2 (53.0)  2300.7 (52.1) 
##                                2109.3 (47.0)  2117.7 (47.9) 
##   Education (%)                 922.6 (20.6)  1194.8 (27.1) 
##                                1270.7 (28.4)  1181.1 (26.8) 
##                                2061.6 (46.1)  1857.9 (42.2) 
##                                 215.4 ( 4.8)   171.0 ( 3.9) 
##   Income_level (%)              492.9 (11.4)   299.1 ( 6.9) 
##                                1301.4 (30.1)   849.7 (19.6) 
##                                1560.7 (36.0)  1494.9 (34.4) 
##                                 676.8 (15.6)  1018.6 (23.4) 
##                                 298.3 ( 6.9)   681.5 (15.7) 
##   Current_Health_insurance (%) 1722.9 (38.5)  1429.0 (32.4) 
##                                2756.4 (61.5)  2983.8 (67.6) 
##   Physician_Visit (%)          1976.4 (44.6)  1375.1 (31.3) 
##                                1009.3 (22.8)  1477.6 (33.6) 
##                                1446.6 (32.6)  1543.3 (35.1) 
##   Background (%)                 57.7 ( 1.3)     6.1 ( 0.1) 
##                                 685.5 (15.3)    36.8 ( 0.8) 
##                                3025.9 (67.7)    19.4 ( 0.4) 
##                                  40.6 ( 0.9)  4082.5 (92.7) 
##                                 104.9 ( 2.3)    66.8 ( 1.5) 
##                                 425.4 ( 9.5)    35.7 ( 0.8) 
##                                 130.3 ( 2.9)   157.9 ( 3.6) 
##   Language_pref (%)            4226.9 (94.2)  3040.7 (68.8) 
##                                 258.6 ( 5.8)  1377.7 (31.2) 
##   Marital_status (%)           1195.8 (26.7)  1052.6 (23.8) 
##                                2225.8 (49.7)  2734.0 (61.9) 
##                                1060.9 (23.7)   627.1 (14.2) 
##   Employment_status (%)        1821.8 (40.6)  1583.8 (35.9) 
##                                 933.4 (20.8)  1003.9 (22.7) 
##                                1730.2 (38.6)  1825.9 (41.4) 
##   US_BORN (%)                  4114.7 (92.0)  3055.0 (69.3) 
##                                 355.6 ( 8.0)  1352.4 (30.7) 
##                               Stratified by CENTER
##                                p      test Missing
##   n                                               
##   AGE (%)                      <0.001      0.0    
##                                                   
##                                                   
##   SEX (%)                       0.264      0.0    
##                                                   
##   Education (%)                <0.001      0.4    
##                                                   
##                                                   
##                                                   
##   Income_level (%)             <0.001      2.5    
##                                                   
##                                                   
##                                                   
##                                                   
##   Current_Health_insurance (%) <0.001      0.3    
##                                                   
##   Physician_Visit (%)          <0.001      1.3    
##                                                   
##                                                   
##   Background (%)               <0.001      0.3    
##                                                   
##                                                   
##                                                   
##                                                   
##                                                   
##                                                   
##   Language_pref (%)            <0.001      0.0    
##                                                   
##   Marital_status (%)           <0.001      0.1    
##                                                   
##                                                   
##   Employment_status (%)        <0.001      0.3    
##                                                   
##                                                   
##   US_BORN (%)                  <0.001      0.3    
## 
```

Try the same thing without turning the table into a data frame, to have nicer row names:

```r
tbl1_wc <- tbl1_weighted_center[,-which(colnames(tb1_noweight_center) %in% c("p", "test"))]   
tbl1_c <- tb1_noweight_center[,-which(colnames(tb1_noweight_center) %in% c("p", "test"))]

tbl1_comb <- tbl1_c
# ignore the "Miami" typo...
col_inds_to_update <- which(colnames(tbl1_wc) %in% c("Bronx", "Chicago", "Miami", "San Diego"))

# update tbl1_comb with the percentages from the weighted table
for (i in col_inds_to_update){
  counts <- sapply(tbl1_c[,i], function(x){
                  strsplit(x, split = "(", fixed = TRUE)[[1]][1]
                 })
  percnt <- sapply(tbl1_wc[,i], function(x){
                paste0("(",  strsplit(x, split = "(", fixed = TRUE)[[1]][2])
              })
  tbl1_comb[,i] <- paste0(counts, percnt)
}

  
write.csv(tbl1_comb,paste0(output_dir,"/tbl1_stratified.csv"))
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
## [1] stats4    grid      stats     graphics  grDevices utils    
## [7] datasets  methods   base     
## 
## other attached packages:
##  [1] Publish_2020.12.23 prodlim_2019.11.13 jtools_2.1.3      
##  [4] boot_1.3-27        mi_1.0             sjlabelled_1.1.7  
##  [7] memisc_0.99.27.3   MASS_7.3-53.1      lattice_0.20-41   
## [10] tableone_0.12.0    labelled_2.8.0     factoextra_1.0.7  
## [13] plyr_1.8.6         survey_4.0         survival_3.2-9    
## [16] Matrix_1.3-2       forcats_0.5.1      stringr_1.4.0     
## [19] dplyr_1.0.5        purrr_0.3.4        readr_1.4.0       
## [22] tidyr_1.1.3        tibble_3.1.7       ggplot2_3.3.3     
## [25] tidyverse_1.3.0    rmarkdown_2.7     
## 
## loaded via a namespace (and not attached):
##  [1] minqa_1.2.4         colorspace_2.0-0   
##  [3] ellipsis_0.3.2      rio_0.5.26         
##  [5] flextable_0.6.6     htmlTable_2.1.0    
##  [7] base64enc_0.1-3     fs_1.5.0           
##  [9] rstudioapi_0.13     ggrepel_0.9.1      
## [11] fansi_0.4.2         lubridate_1.7.10   
## [13] xml2_1.3.2          splines_4.0.3      
## [15] knitr_1.31          Formula_1.2-4      
## [17] jsonlite_1.7.2      nloptr_1.2.2.2     
## [19] broom_0.7.8         cluster_2.1.1      
## [21] dbplyr_2.1.0        png_0.1-7          
## [23] compiler_4.0.3      httr_1.4.2         
## [25] backports_1.2.1     assertthat_0.2.1   
## [27] cli_3.3.0           htmltools_0.5.1.1  
## [29] tools_4.0.3         coda_0.19-4        
## [31] gtable_0.3.0        glue_1.6.2         
## [33] Rcpp_1.0.6          carData_3.0-4      
## [35] cellranger_1.1.0    jquerylib_0.1.3    
## [37] vctrs_0.4.1         nlme_3.1-152       
## [39] insight_0.17.1      xfun_0.22          
## [41] openxlsx_4.2.3      lme4_1.1-26        
## [43] rvest_1.0.0         lifecycle_1.0.0    
## [45] statmod_1.4.35      zoo_1.8-9          
## [47] scales_1.1.1        hms_1.0.0          
## [49] RColorBrewer_1.1-2  yaml_2.2.1         
## [51] curl_4.3            gridExtra_2.3      
## [53] pander_0.6.3        gdtools_0.2.3      
## [55] sass_0.3.1          rpart_4.1-15       
## [57] latticeExtra_0.6-29 stringi_1.5.3      
## [59] highr_0.8           checkmate_2.0.0    
## [61] zip_2.1.1           lava_1.6.9         
## [63] repr_1.1.3          rlang_1.0.3        
## [65] pkgconfig_2.0.3     systemfonts_1.0.1  
## [67] arm_1.11-2          evaluate_0.14      
## [69] htmlwidgets_1.5.3   tidyselect_1.1.0   
## [71] magrittr_2.0.1      R6_2.5.0           
## [73] generics_0.1.2      Hmisc_4.5-0        
## [75] DBI_1.1.1           pillar_1.7.0       
## [77] haven_2.3.1         foreign_0.8-81     
## [79] withr_2.4.1         abind_1.4-5        
## [81] nnet_7.3-15         modelr_0.1.8       
## [83] crayon_1.4.1        car_3.0-10         
## [85] uuid_0.1-4          utf8_1.2.1         
## [87] officer_0.3.18      jpeg_0.1-8.1       
## [89] readxl_1.3.1        data.table_1.14.0  
## [91] reprex_1.0.0        digest_0.6.27      
## [93] munsell_0.5.0       bslib_0.2.4        
## [95] mitools_2.4
```
