---
title: "Week 2 Parsing Data Exploration"
author: "Mitchell"
date: "April 27, 2020"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.5.3
```

```
## Warning: package 'ggplot2' was built under R version 3.5.3
```

```
## Warning: package 'tibble' was built under R version 3.5.3
```

```
## Warning: package 'tidyr' was built under R version 3.5.3
```

```
## Warning: package 'readr' was built under R version 3.5.3
```

```
## Warning: package 'dplyr' was built under R version 3.5.3
```

```
## Warning: package 'forcats' was built under R version 3.5.3
```

```r
flowey <- read_csv("master_flowering_time_data_2019_10_15.csv")
```

The goal for this week if first to ensure that all data is parsed correctly. So we need to see how data was parsed. 

```r
head(flowey)
```

```
## # A tibble: 6 x 17
##   DNA_ID field_ID genotype group row   `end?` condition germ  yellow
##   <chr>  <chr>    <chr>    <chr> <chr> <chr>  <chr>     <chr> <chr> 
## 1 D132   SUN_A_01 WT       A     SUN_A end    sun       06_0~ 07_30~
## 2 D133   SUN_A_02 WT       A     SUN_A end    sun       06_0~ 07_30~
## 3 D134   SUN_A_03 WT       A     SUN_A middle sun       06_0~ 07_29~
## 4 D136   SUN_A_09 WT       A     SUN_A middle sun       06_0~ 07_30~
## 5 D137   SUN_A_10 WT       A     SUN_A middle sun       06_0~ 07_30~
## 6 D138   SUN_A_11 WT       A     SUN_A middle sun       06_0~ <NA>  
## # ... with 8 more variables: flowering <chr>, final <chr>, diameter <dbl>,
## #   direction <chr>, height <dbl>, leaf_num <dbl>, ax_heads <dbl>,
## #   notes <chr>
```

Personally, I would like a row ID (that has the numerical component of `field_ID`. So let's do that: 

```r
ID <- flowey %>%
  select(field_ID) %>%
  separate(field_ID, sep = "_", into = c("bleh1", "bleh2", "row_ID"), convert = TRUE) %>%
  select(row_ID) 

flowey2 <- bind_cols(flowey, ID)
head(flowey2)
```

```
## # A tibble: 6 x 18
##   DNA_ID field_ID genotype group row   `end?` condition germ  yellow
##   <chr>  <chr>    <chr>    <chr> <chr> <chr>  <chr>     <chr> <chr> 
## 1 D132   SUN_A_01 WT       A     SUN_A end    sun       06_0~ 07_30~
## 2 D133   SUN_A_02 WT       A     SUN_A end    sun       06_0~ 07_30~
## 3 D134   SUN_A_03 WT       A     SUN_A middle sun       06_0~ 07_29~
## 4 D136   SUN_A_09 WT       A     SUN_A middle sun       06_0~ 07_30~
## 5 D137   SUN_A_10 WT       A     SUN_A middle sun       06_0~ 07_30~
## 6 D138   SUN_A_11 WT       A     SUN_A middle sun       06_0~ <NA>  
## # ... with 9 more variables: flowering <chr>, final <chr>, diameter <dbl>,
## #   direction <chr>, height <dbl>, leaf_num <dbl>, ax_heads <dbl>,
## #   notes <chr>, row_ID <int>
```

```r
#Fricken easy. 
```

As we discussed last week, we need to parse some variables differently. Let's start with `genotype`, `group`, `end`, `condition`, all of which need to be parsed as factors: 

```r
flowey2$genotype <- parse_factor(flowey2$genotype)
flowey2$group <- parse_factor(flowey2$group)
flowey2$'end?' <- parse_factor(flowey2$'end?')
flowey2$condition <- parse_factor(flowey2$condition)
head(flowey2)
```

```
## # A tibble: 6 x 18
##   DNA_ID field_ID genotype group row   `end?` condition germ  yellow
##   <chr>  <chr>    <fct>    <fct> <chr> <fct>  <fct>     <chr> <chr> 
## 1 D132   SUN_A_01 WT       A     SUN_A end    sun       06_0~ 07_30~
## 2 D133   SUN_A_02 WT       A     SUN_A end    sun       06_0~ 07_30~
## 3 D134   SUN_A_03 WT       A     SUN_A middle sun       06_0~ 07_29~
## 4 D136   SUN_A_09 WT       A     SUN_A middle sun       06_0~ 07_30~
## 5 D137   SUN_A_10 WT       A     SUN_A middle sun       06_0~ 07_30~
## 6 D138   SUN_A_11 WT       A     SUN_A middle sun       06_0~ <NA>  
## # ... with 9 more variables: flowering <chr>, final <chr>, diameter <dbl>,
## #   direction <chr>, height <dbl>, leaf_num <dbl>, ax_heads <dbl>,
## #   notes <chr>, row_ID <int>
```

Now we need to parse the dates as, well, actual dates. So let's do it. The commented code shows we have some data parsing issues. Luckily, it seems issues are limited to `germ`. Problematic values are also only `07_2019_03` and `07_2019_04`. 

```r
#flowey2$germ <- parse_date(flowey2$germ, format = "%m_%d_%Y")
#problems(flowey2$germ)
flowey2$yellow <- parse_date(flowey2$yellow, format = "%m_%d_%Y") 
problems(flowey2$yellow)
```

```
## [1] row      col      expected actual  
## <0 rows> (or 0-length row.names)
```

```r
flowey2$flowering <- parse_date(flowey2$flowering, format = "%m_%d_%Y") 
problems(flowey2$flowering)
```

```
## [1] row      col      expected actual  
## <0 rows> (or 0-length row.names)
```

```r
flowey2$final <- parse_date(flowey2$final, format = "%m_%d_%Y") 
problems(flowey2$final)
```

```
## [1] row      col      expected actual  
## <0 rows> (or 0-length row.names)
```

Luckily, we can do some neat find-and-replace to make sure we parse all the stuff correctly. 

Approach: Pull out the problematic rows, parse them correctly, then stick them back in. 

```r
#first, make our final data frame with all the correct date-time values: 
flowey_3 <- flowey2 %>%
  filter(germ != "07_2019_03" | is.na(germ)) %>%
  filter(germ != "07_2019_04" | is.na(germ))

#next, make a date frame with the bad values: 
flowey_bad <- flowey2 %>%
  filter(germ == "07_2019_03" | germ == "07_2019_04") 

#next, parse `germ` correctly: 
flowey_3$germ <- parse_date(flowey_3$germ, format = "%m_%d_%Y")
flowey_bad$germ <- parse_date(flowey_bad$germ, format = "%m_%Y_%d")

head(flowey_bad)
```

```
## # A tibble: 6 x 18
##   DNA_ID field_ID genotype group row   `end?` condition germ      
##   <chr>  <chr>    <fct>    <fct> <chr> <fct>  <fct>     <date>    
## 1 D418   SH_D_05  WT       D     SH_D  middle shade     2019-07-03
## 2 D421   SH_D_06  WT       D     SH_D  middle shade     2019-07-04
## 3 D424   SH_D_07  WT       D     SH_D  middle shade     2019-07-04
## 4 D425   SH_D_08  WT       D     SH_D  middle shade     2019-07-04
## 5 D427   SH_D_13  WT       D     SH_D  middle shade     2019-07-04
## 6 D428   SH_D_14  WT       D     SH_D  middle shade     2019-07-04
## # ... with 10 more variables: yellow <date>, flowering <date>,
## #   final <date>, diameter <dbl>, direction <chr>, height <dbl>,
## #   leaf_num <dbl>, ax_heads <dbl>, notes <chr>, row_ID <int>
```

```r
head(flowey_3)
```

```
## # A tibble: 6 x 18
##   DNA_ID field_ID genotype group row   `end?` condition germ      
##   <chr>  <chr>    <fct>    <fct> <chr> <fct>  <fct>     <date>    
## 1 D132   SUN_A_01 WT       A     SUN_A end    sun       2019-06-07
## 2 D133   SUN_A_02 WT       A     SUN_A end    sun       2019-06-08
## 3 D134   SUN_A_03 WT       A     SUN_A middle sun       2019-06-08
## 4 D136   SUN_A_09 WT       A     SUN_A middle sun       2019-06-08
## 5 D137   SUN_A_10 WT       A     SUN_A middle sun       2019-06-08
## 6 D138   SUN_A_11 WT       A     SUN_A middle sun       2019-06-08
## # ... with 10 more variables: yellow <date>, flowering <date>,
## #   final <date>, diameter <dbl>, direction <chr>, height <dbl>,
## #   leaf_num <dbl>, ax_heads <dbl>, notes <chr>, row_ID <int>
```

```r
#finally, stick the correct columns back into the final data frame and check: 
flowey_4 <- bind_rows(flowey_3, flowey_bad)
head(flowey_4)
```

```
## # A tibble: 6 x 18
##   DNA_ID field_ID genotype group row   `end?` condition germ      
##   <chr>  <chr>    <fct>    <fct> <chr> <fct>  <fct>     <date>    
## 1 D132   SUN_A_01 WT       A     SUN_A end    sun       2019-06-07
## 2 D133   SUN_A_02 WT       A     SUN_A end    sun       2019-06-08
## 3 D134   SUN_A_03 WT       A     SUN_A middle sun       2019-06-08
## 4 D136   SUN_A_09 WT       A     SUN_A middle sun       2019-06-08
## 5 D137   SUN_A_10 WT       A     SUN_A middle sun       2019-06-08
## 6 D138   SUN_A_11 WT       A     SUN_A middle sun       2019-06-08
## # ... with 10 more variables: yellow <date>, flowering <date>,
## #   final <date>, diameter <dbl>, direction <chr>, height <dbl>,
## #   leaf_num <dbl>, ax_heads <dbl>, notes <chr>, row_ID <int>
```

The NAs in the filter let us keep all our values when we filter. 

```r
flowey_final <-  flowey_4 %>%
  mutate(flowering_time = difftime(flowering, germ)) 

flowey_final <- flowey_final[, c(1, 2, 3, 4, 5, 18, 6, 7, 8, 9, 10, 19, 11, 12, 13, 14, 15, 16, 17)] #reorder columns nicely. 
head(flowey_final)
```

```
## # A tibble: 6 x 19
##   DNA_ID field_ID genotype group row   row_ID `end?` condition germ      
##   <chr>  <chr>    <fct>    <fct> <chr>  <int> <fct>  <fct>     <date>    
## 1 D132   SUN_A_01 WT       A     SUN_A      1 end    sun       2019-06-07
## 2 D133   SUN_A_02 WT       A     SUN_A      2 end    sun       2019-06-08
## 3 D134   SUN_A_03 WT       A     SUN_A      3 middle sun       2019-06-08
## 4 D136   SUN_A_09 WT       A     SUN_A      9 middle sun       2019-06-08
## 5 D137   SUN_A_10 WT       A     SUN_A     10 middle sun       2019-06-08
## 6 D138   SUN_A_11 WT       A     SUN_A     11 middle sun       2019-06-08
## # ... with 10 more variables: yellow <date>, flowering <date>,
## #   flowering_time <time>, final <date>, diameter <dbl>, direction <chr>,
## #   height <dbl>, leaf_num <dbl>, ax_heads <dbl>, notes <chr>
```

Wow. That was not super easy. Regardless, we have not parsed every variable the way we want to and calculated the flowering time. 



  
