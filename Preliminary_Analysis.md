Preliminary Analysis
================
Jaewon Royce Choi
6/15/2020

## Preliminary Analysis

Preliminary statistical analysis of relationships between the
    variables.

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✓ ggplot2 3.1.0     ✓ purrr   0.3.0
    ## ✓ tibble  3.0.1     ✓ dplyr   0.8.3
    ## ✓ tidyr   0.8.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
#### Read in the dataset ####

tx_bb_entrepreneur_merged <- read_csv("Broadband-Entrepreneurship-TX-merged.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   county = col_character(),
    ##   state = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
str(tx_bb_entrepreneur_merged)
```

> **Correlation Matrix for Preliminary Set of Variables**

Basic correlation matrix with some preliminary variables.

  - IRR2010: Rural index (larger = rural)
  - proprietors\_2017: \# of sole proprietors in TX
  - pct\_proprietors\_employment\_2017: % of sole proprietors in total
    employment
  - pct\_broadband\_FCC: FCC broadband availability in % (2017)
  - pct\_broadband\_MS: MS broadband availability in % (2019)
  - venturedensitydec19: Venture density by GoDaddy
  - highlyactive\_vddec19: Highly active venture density by GoDaddy
  - prosperityindex2016: Prosperity index
  - frac\_over\_25DL.dec: % of M-Lab testers with over 25mbps of DL
    speed
  - frac\_over\_3UL.dec: % of M-Lab testers with over 3mbps of UL speed

<!-- end list -->

``` r
#### Correlation between variabes ####

## Basic Correlation Table ##

tx_bb_entrepreneur_merged %>% 
  select(IRR2010, proprietors_2017, pct_proprietors_employment_2017, pct_broadband_FCC, pct_broadband_MS,
         venturedensitydec19, highlyactive_vddec19, prosperityindex2016, frac_over_25DL.dec, frac_over_3UL.dec) %>% 
  cor(method = "pearson", use = "complete.obs") %>% knitr::kable(format = "markdown", digits = 3)
```

|                                    | IRR2010 | proprietors\_2017 | pct\_proprietors\_employment\_2017 | pct\_broadband\_FCC | pct\_broadband\_MS | venturedensitydec19 | highlyactive\_vddec19 | prosperityindex2016 | frac\_over\_25DL.dec | frac\_over\_3UL.dec |
| :--------------------------------- | ------: | ----------------: | ---------------------------------: | ------------------: | -----------------: | ------------------: | --------------------: | ------------------: | -------------------: | ------------------: |
| IRR2010                            |   1.000 |           \-0.696 |                              0.471 |             \-0.440 |            \-0.739 |             \-0.443 |               \-0.378 |             \-0.413 |              \-0.519 |             \-0.468 |
| proprietors\_2017                  | \-0.696 |             1.000 |                            \-0.226 |               0.227 |              0.465 |               0.363 |                 0.315 |               0.281 |                0.278 |               0.252 |
| pct\_proprietors\_employment\_2017 |   0.471 |           \-0.226 |                              1.000 |             \-0.318 |            \-0.361 |               0.053 |                 0.194 |             \-0.001 |              \-0.234 |             \-0.209 |
| pct\_broadband\_FCC                | \-0.440 |             0.227 |                            \-0.318 |               1.000 |              0.535 |               0.183 |                 0.080 |               0.266 |                0.453 |               0.456 |
| pct\_broadband\_MS                 | \-0.739 |             0.465 |                            \-0.361 |               0.535 |              1.000 |               0.541 |                 0.482 |               0.610 |                0.687 |               0.662 |
| venturedensitydec19                | \-0.443 |             0.363 |                              0.053 |               0.183 |              0.541 |               1.000 |                 0.790 |               0.533 |                0.270 |               0.246 |
| highlyactive\_vddec19              | \-0.378 |             0.315 |                              0.194 |               0.080 |              0.482 |               0.790 |                 1.000 |               0.540 |                0.242 |               0.242 |
| prosperityindex2016                | \-0.413 |             0.281 |                            \-0.001 |               0.266 |              0.610 |               0.533 |                 0.540 |               1.000 |                0.293 |               0.242 |
| frac\_over\_25DL.dec               | \-0.519 |             0.278 |                            \-0.234 |               0.453 |              0.687 |               0.270 |                 0.242 |               0.293 |                1.000 |               0.857 |
| frac\_over\_3UL.dec                | \-0.468 |             0.252 |                            \-0.209 |               0.456 |              0.662 |               0.246 |                 0.242 |               0.242 |                0.857 |               1.000 |

> **Correlation Matrix with more Information**

A correlation matrix with more in-depth information using the
`chart.Correlation()` function in package `PerformanceAnalytics`.

``` r
#install.packages("PerformanceAnalytics")

tx_bb_entrepreneur_merged %>% 
  select(IRR2010, proprietors_2017, pct_proprietors_employment_2017, pct_broadband_FCC, pct_broadband_MS,
         venturedensitydec19, highlyactive_vddec19, prosperityindex2016, frac_over_25DL.dec, frac_over_3UL.dec) %>%
  PerformanceAnalytics::chart.Correlation(histogram = T)
```

<img src="Preliminary_Analysis_files/figure-gfm/Correlation Matrix-1.png" width="672" style="display: block; margin: auto;" />

> \*\* Preliminary Exploration of Relationships further with
> Regressions\*\*

Few models to examine relationships between **entrepreneurship** and
**broadband**.

``` r
#### Exploring the relationships further with regressions ####

## DV: Venture Density as Entrepreneurship Index
## IV: Rural index, Proprietors share in employment, Broadband (FCC), Broadband (MS), Broadband speed

prem_model <- lm(venturedensitydec19 ~ IRR2010 + pct_proprietors_employment_2017 + pct_broadband_FCC + pct_broadband_MS + frac_over_25DL.dec + frac_over_3UL.dec, data = tx_bb_entrepreneur_merged)

summary(prem_model)
```

    ## 
    ## Call:
    ## lm(formula = venturedensitydec19 ~ IRR2010 + pct_proprietors_employment_2017 + 
    ##     pct_broadband_FCC + pct_broadband_MS + frac_over_25DL.dec + 
    ##     frac_over_3UL.dec, data = tx_bb_entrepreneur_merged)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8968 -1.0503 -0.2917  0.5414  8.2788 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                       2.4980     1.1849   2.108  0.03608 *  
    ## IRR2010                          -5.3453     1.9369  -2.760  0.00625 ** 
    ## pct_proprietors_employment_2017   6.7077     1.1948   5.614 5.59e-08 ***
    ## pct_broadband_FCC                -0.4266     0.4669  -0.914  0.36186    
    ## pct_broadband_MS                  7.5314     1.1030   6.828 7.39e-11 ***
    ## frac_over_25DL.dec               -1.0688     1.0522  -1.016  0.31077    
    ## frac_over_3UL.dec                -0.7853     0.8793  -0.893  0.37269    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.691 on 233 degrees of freedom
    ##   (14 observations deleted due to missingness)
    ## Multiple R-squared:  0.3846, Adjusted R-squared:  0.3687 
    ## F-statistic: 24.27 on 6 and 233 DF,  p-value: < 2.2e-16

``` r
## IV added: Prosperity index

prem_model2 <- lm(venturedensitydec19 ~ IRR2010 + pct_proprietors_employment_2017 + pct_broadband_FCC + pct_broadband_MS + frac_over_25DL.dec + frac_over_3UL.dec + prosperityindex2016, data = tx_bb_entrepreneur_merged)

summary(prem_model2)
```

    ## 
    ## Call:
    ## lm(formula = venturedensitydec19 ~ IRR2010 + pct_proprietors_employment_2017 + 
    ##     pct_broadband_FCC + pct_broadband_MS + frac_over_25DL.dec + 
    ##     frac_over_3UL.dec + prosperityindex2016, data = tx_bb_entrepreneur_merged)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1026 -0.9362 -0.2510  0.5161  8.0063 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      2.686143   1.146267   2.343  0.02001 *  
    ## IRR2010                         -5.809828   1.873094  -3.102  0.00218 ** 
    ## pct_proprietors_employment_2017  5.760488   1.225637   4.700 4.61e-06 ***
    ## pct_broadband_FCC               -0.579557   0.464066  -1.249  0.21305    
    ## pct_broadband_MS                 5.717653   1.292959   4.422 1.54e-05 ***
    ## frac_over_25DL.dec              -0.944505   1.021560  -0.925  0.35621    
    ## frac_over_3UL.dec               -0.560824   0.865033  -0.648  0.51746    
    ## prosperityindex2016              0.020313   0.007377   2.754  0.00639 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.601 on 218 degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## Multiple R-squared:  0.4394, Adjusted R-squared:  0.4214 
    ## F-statistic: 24.41 on 7 and 218 DF,  p-value: < 2.2e-16

``` r
## DV: Highly active venture density

prem_model3 <- lm(highlyactive_vddec19 ~ IRR2010 + pct_proprietors_employment_2017 + pct_broadband_FCC + pct_broadband_MS + frac_over_25DL.dec + frac_over_3UL.dec + prosperityindex2016, data = tx_bb_entrepreneur_merged)

summary(prem_model3)
```

    ## 
    ## Call:
    ## lm(formula = highlyactive_vddec19 ~ IRR2010 + pct_proprietors_employment_2017 + 
    ##     pct_broadband_FCC + pct_broadband_MS + frac_over_25DL.dec + 
    ##     frac_over_3UL.dec + prosperityindex2016, data = tx_bb_entrepreneur_merged)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.73268 -0.21076 -0.05351  0.10314  2.40103 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      0.522194   0.251449   2.077 0.038998 *  
    ## IRR2010                         -1.464276   0.410888  -3.564 0.000449 ***
    ## pct_proprietors_employment_2017  1.850403   0.268860   6.882 6.17e-11 ***
    ## pct_broadband_FCC               -0.284615   0.101799  -2.796 0.005639 ** 
    ## pct_broadband_MS                 1.140806   0.283628   4.022 7.95e-05 ***
    ## frac_over_25DL.dec              -0.321008   0.224093  -1.432 0.153439    
    ## frac_over_3UL.dec                0.102377   0.189757   0.540 0.590079    
    ## prosperityindex2016              0.005688   0.001618   3.515 0.000535 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3513 on 218 degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## Multiple R-squared:  0.4833, Adjusted R-squared:  0.4667 
    ## F-statistic: 29.13 on 7 and 218 DF,  p-value: < 2.2e-16
