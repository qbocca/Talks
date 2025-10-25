Niche-based grouping for mixed species growth and yield models: SOMENS.
================

## Notes

    Contact me at: quentin.boccaleri@uga.edu
    Linkedin: Quentin Boccaleri
    Github: qbocca
    Eyield site: https://eyield.sref.info/
    PMRC: https://pmrc.uga.edu/

Some code was edited based on AI generated example material using the
ChatGPT o4 mini model via DuckDuckGo’s AI service.

Proverbs 1:7

## Code

### Data Cleanup

``` r
dat <- read.csv("F:/FIA/ratio.csv")

head(dat)
```

    ##   Unique_Tree_Identifier      Tree_CN Year CDIA CHT Current_CV SPP        VR
    ## 1           13-1-1-1-1-6 1.541219e+15 2022  9.8  79  15.178831 131  2.923007
    ## 2           13-1-1-1-1-6 6.038165e+14 2017  9.3  73  12.255824 131  6.047248
    ## 3           13-1-1-1-1-6 1.768699e+14 2012  8.1  54   6.208576 131  3.650582
    ## 4           13-1-1-1-1-6 2.384944e+14 2007  6.6  40   2.557994 131  0.000000
    ## 5          13-1-1-12-1-1 7.203733e+14 2018 14.7  89  42.491043 131 24.683288
    ## 6          13-1-1-12-1-1 2.511411e+14 2013 11.6  64  17.807755 131 11.081117
    ##         RM Years_Passed
    ## 1 1.142695           15
    ## 2 2.364059           10
    ## 3 1.427127            5
    ## 4 0.000000            0
    ## 5 3.669484           10
    ## 6 1.647348            5

``` r
summary(dat)
```

    ##  Unique_Tree_Identifier    Tree_CN               Year           CDIA      
    ##  Length:72403           Min.   :7.362e+12   Min.   :1997   Min.   : 5.00  
    ##  Class :character       1st Qu.:2.386e+14   1st Qu.:2005   1st Qu.: 7.80  
    ##  Mode  :character       Median :2.389e+14   Median :2010   Median :10.10  
    ##                         Mean   :4.171e+14   Mean   :2010   Mean   :10.95  
    ##                         3rd Qu.:4.229e+14   3rd Qu.:2015   3rd Qu.:13.20  
    ##                         Max.   :1.738e+15   Max.   :2023   Max.   :39.20  
    ##       CHT           Current_CV           SPP              VR           
    ##  Min.   :  5.00   Min.   :  0.000   Min.   :110.0   Min.   :-309.5681  
    ##  1st Qu.: 56.00   1st Qu.:  6.162   1st Qu.:131.0   1st Qu.:   0.0000  
    ##  Median : 68.00   Median : 13.696   Median :131.0   Median :   0.2517  
    ##  Mean   : 69.55   Mean   : 22.838   Mean   :127.9   Mean   :   2.8708  
    ##  3rd Qu.: 81.00   3rd Qu.: 29.101   3rd Qu.:131.0   3rd Qu.:   4.9182  
    ##  Max.   :145.00   Max.   :370.586   Max.   :131.0   Max.   :  89.4967  
    ##        RM            Years_Passed   
    ##  Min.   :-7.76708   Min.   : 0.000  
    ##  1st Qu.: 0.00000   1st Qu.: 0.000  
    ##  Median : 0.01511   Median : 7.000  
    ##  Mean   : 0.42608   Mean   : 7.735  
    ##  3rd Qu.: 0.59829   3rd Qu.:13.000  
    ##  Max.   :23.79902   Max.   :26.000

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
dat2 <- dat %>%
  group_by(Unique_Tree_Identifier) %>%
  arrange(Year) %>%
  # Filter out any rows with negative RM values
  filter(RM >= 0) %>%
  # Check if the height is strictly increasing
  filter(all(diff(CHT) > 0)) %>% 
  # Filter for trees that start at Years_Passed = 0
  filter(first(Years_Passed) == 0) %>%
  # Filter for trees with more than 3 measurements 
  filter(n() > 3) %>%
  # Filter for trees where the first VR value equals 0
  filter(first(VR) == 0) %>%
  ungroup()

summary(dat2)
```

    ##  Unique_Tree_Identifier    Tree_CN               Year           CDIA      
    ##  Length:22024           Min.   :7.362e+12   Min.   :1997   Min.   : 5.00  
    ##  Class :character       1st Qu.:2.386e+14   1st Qu.:2007   1st Qu.: 8.40  
    ##  Mode  :character       Median :2.523e+14   Median :2012   Median :10.40  
    ##                         Mean   :5.029e+14   Mean   :2012   Mean   :10.91  
    ##                         3rd Qu.:6.038e+14   3rd Qu.:2017   3rd Qu.:12.80  
    ##                         Max.   :1.738e+15   Max.   :2023   Max.   :37.80  
    ##       CHT           Current_CV            SPP              VR         
    ##  Min.   : 20.00   Min.   :  0.3454   Min.   :110.0   Min.   : 0.0000  
    ##  1st Qu.: 57.00   1st Qu.:  7.8291   1st Qu.:131.0   1st Qu.: 0.8622  
    ##  Median : 68.00   Median : 14.8465   Median :131.0   Median : 4.4030  
    ##  Mean   : 68.74   Mean   : 20.6281   Mean   :129.8   Mean   : 5.5683  
    ##  3rd Qu.: 80.00   3rd Qu.: 26.8726   3rd Qu.:131.0   3rd Qu.: 8.1484  
    ##  Max.   :142.00   Max.   :370.5861   Max.   :131.0   Max.   :77.0410  
    ##        RM           Years_Passed   
    ##  Min.   : 0.0000   Min.   : 0.000  
    ##  1st Qu.: 0.1295   1st Qu.: 5.000  
    ##  Median : 0.6487   Median :10.000  
    ##  Mean   : 0.9304   Mean   : 9.079  
    ##  3rd Qu.: 1.2581   3rd Qu.:15.000  
    ##  Max.   :23.7990   Max.   :26.000

``` r
dat2$SPP <- as.factor(dat2$SPP)

num <- n_distinct(dat2$Unique_Tree_Identifier)

print(num)
```

    ## [1] 5113

``` r
dat2$RM2 <- dat2$RM / dat2$Years_Passed


dat2$RM2[is.na(dat2$RM2)] <- 0

summary(dat2)
```

    ##  Unique_Tree_Identifier    Tree_CN               Year           CDIA      
    ##  Length:22024           Min.   :7.362e+12   Min.   :1997   Min.   : 5.00  
    ##  Class :character       1st Qu.:2.386e+14   1st Qu.:2007   1st Qu.: 8.40  
    ##  Mode  :character       Median :2.523e+14   Median :2012   Median :10.40  
    ##                         Mean   :5.029e+14   Mean   :2012   Mean   :10.91  
    ##                         3rd Qu.:6.038e+14   3rd Qu.:2017   3rd Qu.:12.80  
    ##                         Max.   :1.738e+15   Max.   :2023   Max.   :37.80  
    ##       CHT           Current_CV        SPP              VR         
    ##  Min.   : 20.00   Min.   :  0.3454   110: 1027   Min.   : 0.0000  
    ##  1st Qu.: 57.00   1st Qu.:  7.8291   121:  564   1st Qu.: 0.8622  
    ##  Median : 68.00   Median : 14.8465   131:20433   Median : 4.4030  
    ##  Mean   : 68.74   Mean   : 20.6281               Mean   : 5.5683  
    ##  3rd Qu.: 80.00   3rd Qu.: 26.8726               3rd Qu.: 8.1484  
    ##  Max.   :142.00   Max.   :370.5861               Max.   :77.0410  
    ##        RM           Years_Passed         RM2          
    ##  Min.   : 0.0000   Min.   : 0.000   Min.   :0.000000  
    ##  1st Qu.: 0.1295   1st Qu.: 5.000   1st Qu.:0.009997  
    ##  Median : 0.6487   Median :10.000   Median :0.063885  
    ##  Mean   : 0.9304   Mean   : 9.079   Mean   :0.087046  
    ##  3rd Qu.: 1.2581   3rd Qu.:15.000   3rd Qu.:0.125635  
    ##  Max.   :23.7990   Max.   :26.000   Max.   :1.709440

``` r
head(dat2)
```

    ## # A tibble: 6 × 11
    ##   Unique_Tree_Identifier Tree_CN  Year  CDIA   CHT Current_CV SPP      VR    RM
    ##   <chr>                    <dbl> <int> <dbl> <int>      <dbl> <fct> <dbl> <dbl>
    ## 1 13-1-107-79-3-3        2.39e14  1997  12.2    74      23.7  121       0     0
    ## 2 13-1-251-5-3-1         2.39e14  1997   8.1    55       6.99 131       0     0
    ## 3 13-3-125-10-1-3        2.39e14  1997  11.7    66      18.8  131       0     0
    ## 4 13-3-133-23-1-7        2.39e14  1997  10.8    65      16.4  110       0     0
    ## 5 13-3-133-34-2-1        2.39e14  1997  15.2    88      44.7  131       0     0
    ## 6 13-3-141-3-1-9         2.39e14  1997   8.1    60       7.08 131       0     0
    ## # ℹ 2 more variables: Years_Passed <int>, RM2 <dbl>

### Regression

``` r
library(lme4)
```

    ## Loading required package: Matrix

``` r
m1 <- lmer(RM2 ~ CDIA + CHT + Years_Passed + (1 |  Unique_Tree_Identifier), data = dat2)

summary(m1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: RM2 ~ CDIA + CHT + Years_Passed + (1 | Unique_Tree_Identifier)
    ##    Data: dat2
    ## 
    ## REML criterion at convergence: -42251.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -10.3824  -0.5134  -0.1875   0.4044  13.0418 
    ## 
    ## Random effects:
    ##  Groups                 Name        Variance Std.Dev.
    ##  Unique_Tree_Identifier (Intercept) 0.004208 0.06487 
    ##  Residual                           0.006258 0.07911 
    ## Number of obs: 22024, groups:  Unique_Tree_Identifier, 5113
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)  -6.772e-03  4.295e-03  -1.577
    ## CDIA          1.483e-03  4.080e-04   3.634
    ## CHT           9.495e-04  9.177e-05  10.346
    ## Years_Passed  1.462e-03  1.456e-04  10.045
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) CDIA   CHT   
    ## CDIA        -0.151              
    ## CHT         -0.679 -0.581       
    ## Years_Passd  0.696 -0.098 -0.613

``` r
residuals_m1 <- resid(m1)
fitted_m1 <- fitted(m1)
actual_m1 <- dat2$RM2 

par(mfrow = c(2, 2))

# 1. Residuals vs Fitted
plot(fitted_m1, residuals_m1,
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# 2. Q-Q Plot
qqnorm(residuals_m1, main = "Q-Q Plot")
qqline(residuals_m1, col = "blue")

# 3. Predicted vs Actual Plot
plot(actual_m1, fitted_m1,
     main = "Predicted vs Actual",
     xlab = "Actual Values",
     ylab = "Predicted Values")
abline(0, 1, col = "blue", lty = 2)  # Diagonal line for perfect predictions


# 4. Histogram of Residuals
hist(residuals_m1, breaks = 10, 
     main = "Histogram of Residuals",
     xlab = "Residuals", 
     col = "lightgrey")
```

![](DOC_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 4.4.3

``` r
fixed_effects <- data.frame(
  Term = c("(Intercept)", "CDIA", "CHT", "Years_Passed"),
  Estimate = c(-6.772e-03, 1.483e-03, 9.495e-04, 1.462e-03),
  Std_Error = c(4.295e-03, 4.080e-04, 9.177e-05, 1.456e-04),
  t_value = c(-1.577, 3.634, 10.346, 10.045)
)


kable(fixed_effects, caption = "Fixed Effects Estimates")
```

| Term         |   Estimate | Std_Error | t_value |
|:-------------|-----------:|----------:|--------:|
| (Intercept)  | -0.0067720 | 0.0042950 |  -1.577 |
| CDIA         |  0.0014830 | 0.0004080 |   3.634 |
| CHT          |  0.0009495 | 0.0000918 |  10.346 |
| Years_Passed |  0.0014620 | 0.0001456 |  10.045 |

Fixed Effects Estimates

## Sources

Bechtold, W. A., & Patterson, P. L. (2005). The enhanced forest
inventory and analysis program–national sampling design and estimation
procedures (No. 80). USDA Forest Service, Southern Research Station.

Bishop, G. N., & Peabody, G. F. (1953). Native trees of Georgia. Georgia
Department of Forestry, School of Forestry, University of Georgia,
Georgia Agricultural Extension Service.

Ogana, Friday N., P. Corey Green, and Phil Radtke. “Modeling the growth
and yield of natural hardwood stands in the southern United States using
the Forest Inventory and Analysis data.” Forest Ecology and Management
586 (2025): 122722.

U.S. Department of Agriculture, Forest Service. 2025. Forests of
Georgia, 2022: FIA annual snapshot. Resource Update FS-484. Washington,
DC: U.S.Department of Agriculture, Forest Service. 4
p. <https://doi.org/10.2737/FS-RU-484>.

USDA Forest Service. 2021. Forests of Georgia, 2019. Resource Update
FS-310. Asheville, NC: U.S. Department of Agriculture, Forest Service.
2p. <https://doi.org/10.2737/FS-RU-310> .

