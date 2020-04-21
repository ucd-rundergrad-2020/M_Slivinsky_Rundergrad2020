---
title: 'Week 1: Flowering Time Data Exploration'
author: "Mitchell"
date: "April 21, 2020"
output: 
  html_document: 
    keep_md: yes
---
Let's go ahead and load the appropriate data sheet into R, along with the libraries. 

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.5.3
```

```
## -- Attaching packages --------------------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.2.0     v purrr   0.3.0
## v tibble  2.1.1     v dplyr   0.8.1
## v tidyr   0.8.3     v stringr 1.3.1
## v readr   1.3.1     v forcats 0.4.0
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

```
## -- Conflicts ------------------------------------------------------------------------------------------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
flowey <- read_csv("master_flowering_time_data_2019_10_15.csv")
```

```
## Parsed with column specification:
## cols(
##   DNA_ID = col_character(),
##   field_ID = col_character(),
##   genotype = col_character(),
##   group = col_character(),
##   row = col_character(),
##   `end?` = col_character(),
##   condition = col_character(),
##   germ = col_character(),
##   yellow = col_character(),
##   flowering = col_character(),
##   final = col_character(),
##   diameter = col_double(),
##   direction = col_character(),
##   height = col_double(),
##   leaf_num = col_double(),
##   ax_heads = col_double(),
##   notes = col_character()
## )
```

```r
view(flowey)
```

### Question 1

As a best guess, this was a controlled experiment designed to investigate the effect of an elf3 mutation on a variety of flowering-related outcomes of sunflowers in sunny or shady conditions. These conditions (the dependent variables) included flowering time, the diameter of flowers (`diameter`), the direction the flowers faced in polar coordinates (`direction`), the number of leaves on plants (`leaf_num`), and the number of axial heads on plants (`ax_heads`). In order to make this a controlled experiment, wild-type sunflowers (in either the shade or sun, shown in `condition`) would be the control group, and sunflowers with elf3 mutations would be the experimental group; therefore, the independent variable is genotype, shown in `genotype`. Depending on how the data is analyzed, sunlight condition (`condition`) or group (`group`) or position in the field (described in `end?` and `row`) might also be considered independent variables. To conduct the experiment, flowers were likely planted in 4 different rows (designated A-D); some of each row was then covered with shade. Support for this is seen in 

```r
table(flowey$row)
```

```
## 
##     SH_A     SH_B SH_B_end     SH_C     SH_D    SUN_A    SUN_B    SUN_C 
##       46       32        6       42       20       45       28       48 
##    SUN_D 
##       19
```

From this, we can see that there are shade groups (designated with prefix `SH`) and sun groups (designated with prefix `SUN`) and also that there are four groups. Interestingly, there appears to be a partially redundant 9th group, `SH_B_end`. I wonder what happened there? Was there something special about this group that necessitated it being separated from the other members of `SH_B`?
How were the flowers planted and grouped? How was shade applied to these groups? Let's take a look at a couple of the groups: 

```r
applyA <- flowey %>%
  filter(condition == "sun" & group == "A") %>%
  separate(field_ID, sep = -2, into = c("bleh", "ID"), convert = TRUE)
applyA[order(applyA$ID), ]
```

```
## # A tibble: 45 x 18
##    DNA_ID bleh     ID genotype group row   `end?` condition germ  yellow
##    <chr>  <chr> <int> <chr>    <chr> <chr> <chr>  <chr>     <chr> <chr> 
##  1 D132   SUN_~     1 WT       A     SUN_A end    sun       06_0~ 07_30~
##  2 D133   SUN_~     2 WT       A     SUN_A end    sun       06_0~ 07_30~
##  3 D134   SUN_~     3 WT       A     SUN_A middle sun       06_0~ 07_29~
##  4 D161   SUN_~     4 WT       A     SUN_A middle sun       06_0~ 08_01~
##  5 D168   SUN_~     5 elf3     A     SUN_A middle sun       06_0~ 08_01~
##  6 D169   SUN_~     6 elf3     A     SUN_A middle sun       06_0~ 08_02~
##  7 D171   SUN_~     7 elf3     A     SUN_A middle sun       06_0~ 08_01~
##  8 D172   SUN_~     8 elf3     A     SUN_A middle sun       06_0~ 07_29~
##  9 D136   SUN_~     9 WT       A     SUN_A middle sun       06_0~ 07_30~
## 10 D137   SUN_~    10 WT       A     SUN_A middle sun       06_0~ 07_30~
## # ... with 35 more rows, and 8 more variables: flowering <chr>,
## #   final <chr>, diameter <dbl>, direction <chr>, height <dbl>,
## #   leaf_num <dbl>, ax_heads <dbl>, notes <chr>
```

Looks like the flowers actually weren't planted in different rows! All of these correspond to group A in the sun, but we have multiple ends, indicating that multiple rows were used (otherwise, some of the plants close to the end of the rows were designated as `end`s.) Aside from this, looks like the plant genotypes were alternated in groups of 4; 4 wt, then 4 elf3, etc. 

```r
filter(applyA, `end?` == "end")
```

```
## # A tibble: 4 x 18
##   DNA_ID bleh     ID genotype group row   `end?` condition germ  yellow
##   <chr>  <chr> <int> <chr>    <chr> <chr> <chr>  <chr>     <chr> <chr> 
## 1 D132   SUN_~     1 WT       A     SUN_A end    sun       06_0~ 07_30~
## 2 D133   SUN_~     2 WT       A     SUN_A end    sun       06_0~ 07_30~
## 3 D158   SUN_~    44 WT       A     SUN_A end    sun       06_0~ <NA>  
## 4 D159   SUN_~    45 WT       A     SUN_A end    sun       06_0~ 07_31~
## # ... with 8 more variables: flowering <chr>, final <chr>, diameter <dbl>,
## #   direction <chr>, height <dbl>, leaf_num <dbl>, ax_heads <dbl>,
## #   notes <chr>
```

Well, scratch that. For each group A-D under each sun condition: 

* A single row of plants was planted. 
* Genotypes were alternated (roughly) in groups of 4. 
* Roughly equal numbers of WT and elf3 plants were used, slightly more WT? 
* The two plants marking the end of each row were designated as "end" plants in the data sheet. In sunshine row A, these were both WT plants; I'd guess this is to ensure that if something went wrong with the end plants, only controls would be lost. 

Now we should actually answer the questions. Are any variables redundant or related? 

* `group`, `row`, and `condition` are definitely redundant with `field_ID`. I'd guess this redundancy helps with data sorting later on. 
* `end?` is related to `field_ID` in that plants with low or high `ID` will be designated as "end" within `end?`. 

As far as I can tell, no other variables are redundant or related. 

### Question 2

For the most part, R parses the data as we would want and most data entries are internally consistent. A few problems, though: 

* `field_ID` is parsed as a `chr`. This is fine because the variable incorporates both position and condition. However, the position component is numerical. We might need to separate this out, then parse it later as `int` or `dbl` to make the data easier to sort or understand. 
* `germ`, `yellow`, `flowering`, and `final` are parsed as `chr` when they are actually dates. I need to refresh this, but don't we have a `dtt` data type for dates and times? We might also convert/parse these to numerical day measurements (`int` data type). 
* `direction` is definitely problematic; it is parsed as a `chr` due to the inclusion of "UP" entries, when it needs to be `int`. We might need to pull these entries out or convert them to something else (maybe `NA`?) so that we can perform mathematical analyses on this data.
* We might also want to pull out any observations that contain entries in the `notes` variable. It seems like a lot of these notes indicate plants that should be discarded. 
* Some observations have `NA` entries for some data related to flowering time (`germ`, `yellow`, `flowering`, and `final`). Depending on what we need to calculate, we might decide to either exclude or include these entries; for example, if we are only interested in understanding the time it takes for each plant to flower, we might include plants with missing values in `yellow`. 

### Question 3

Suggestions for calculations:

1. Calculate time from germination to flowering or `final` for each plant. This would involve either parsing `germ`, `yellow`, `flowering`, and `final` as `dtt` data types and/or converting them to numerical values. 
2. I already did this above, but separating plant `field_ID` into separate components and extracting the relevant `ID` value, parsing it as a `int` vector, is a useful skill that I had to use numerous times while workiing at Bio-Rad. 


