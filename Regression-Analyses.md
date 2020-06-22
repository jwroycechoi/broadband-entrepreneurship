Regression Analyses
================
Jaewon Royce Choi
6/19/2020

In this document, I will set up, run, and examine several multiple
regression models. In general, the models aim to investigate the
relationhips between **broadband** and **entrepreneurship**.
Specifically, **how broadband influences entrepreneurship**.
Furthermore, the models will explore how different measures of
broadband, specifically measures of availability and adoption, explain
entrepreneurial activities differently. Finally, I will explore how
broadband as well as entrepreneurial factors contribute to the overall
prosperity.

``` r
## Load packages
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  3.0.1     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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
#install.packages("stargazer", dependencies = T)    # A package for better looking tables for results
library(stargazer)
```

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

``` r
## Read in dataset
tx_bb_entrepreneur_merged <- read_csv("Broadband-Entrepreneurship-TX-merged.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   county = col_character(),
    ##   state = col_character()
    ## )

    ## See spec(...) for full column specifications.

> ### Broadband and Entrepreneurship

First, let’s take another look at the variables that I examined in the
previous preliminary analysis.

  - IRR2010: Rural index (larger = rural)
  - proprietors\_2017: \# of sole proprietors in TX
  - pct\_proprietors\_employment\_2017: % of sole proprietors in total
    employment
  - pct\_broadband\_FCC: FCC broadband availability in % (2017)
  - pct\_broadband\_MS: MS broadband availability in % (2019)
  - pctbbfrac\_ASU: ACS broadband subscription measure used by the ASU
    white paper
  - venturedensitydec19: Venture density by GoDaddy
  - highlyactive\_vddec19: Highly active venture density by GoDaddy
  - prosperityindex2016: Prosperity index
  - frac\_over\_25DL.dec: % of M-Lab testers with over 25mbps of DL
    speed
  - frac\_over\_3UL.dec: % of M-Lab testers with over 3mbps of UL speed

Since there are several venture density variables measured in different
time periods, I will create an aggregate variable averaging the venture
densities over the time period.

``` r
#### Create average aggregates of venture density ####
tx_bb_entrepreneur_merged <- tx_bb_entrepreneur_merged %>% 
  mutate(venturedensity_mean = (venturedensitymay18 + venturedensitynov18 + venturedensityfeb19 + venturedensitysep19 +
                                  venturedensityoct19 + venturedensitynov19 + venturedensitydec19) / 7,
         highlyactive_vd_mean = (highlyactive_vdmay18 + highlyactive_vdnov18 + highlyactive_vdfeb19 + highlyactive_vdsep19 +
                                   highlyactive_vdoct19 + highlyactive_vdnov19 + highlyactive_vddec19) / 7)

summary(tx_bb_entrepreneur_merged[c("venturedensity_mean", "venturedensitydec19", "highlyactive_vd_mean", "highlyactive_vddec19")])
```

    ##  venturedensity_mean venturedensitydec19 highlyactive_vd_mean
    ##  Min.   : 0.2449     Min.   : 0.1708     Min.   :0.05686     
    ##  1st Qu.: 1.3524     1st Qu.: 1.4150     1st Qu.:0.30287     
    ##  Median : 1.9272     Median : 2.0214     Median :0.46678     
    ##  Mean   : 2.5272     Mean   : 2.6595     Mean   :0.57712     
    ##  3rd Qu.: 2.9966     3rd Qu.: 3.1516     3rd Qu.:0.69424     
    ##  Max.   :11.3616     Max.   :12.1398     Max.   :2.83904     
    ##  NA's   :8           NA's   :6           NA's   :6           
    ##  highlyactive_vddec19
    ##  Min.   :0.05975     
    ##  1st Qu.:0.30851     
    ##  Median :0.46194     
    ##  Mean   :0.59549     
    ##  3rd Qu.:0.72416     
    ##  Max.   :3.48557     
    ##  NA's   :1

#### Broadband’s Effect on Entrepreneurial Outcome

Here I investigate **models that estimate broadband variables’ effect on
the entrepreneurial outcome.** Several different broadband measures are
in the dataset. I will first model **each measure’s relationship with
the entrepreneurial outcome separately.**

Entrepreneurial outcomes can be represented by several variables in this
dataset. As the ASU research team’s white paper argued, the venture
density provided by GoDaddy activities reflects actual entrepreneurial
activities in the form of online domain creation. In the dataset, we
also have Texas state’s sole proprietor statistics. These variables
reflect small and medium sized businesses in each county.

##### Control Variables

For control variables, various demographic measures derived from the ACS
estimates are available in the dataset. Furthermore, the ruralithy of
the counties are reflected in the IRR index. Number of population, and
regional intustry characteristics will be also included as control
variables.

> #### FCC Broadband Availability

``` r
#install.packages("lm.beta")
library(lm.beta)
#### Model 1 ####

## DV: Venture Density, Highly Active VD, Proprietors' Share
## IV: Broadband Availability (FCC)

model_fcc_vd <- lm(venturedensity_mean ~ pct_broadband_FCC + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
              data = tx_bb_entrepreneur_merged)

model_fcc_hvd <- lm(highlyactive_vd_mean ~ pct_broadband_FCC + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
              data = tx_bb_entrepreneur_merged)

model_fcc_pro <- lm(pct_proprietors_employment_2017 ~ pct_broadband_FCC + IRR2010 + population + pctblack + pctnative + pctasian +
                        pcthispanic + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation +
                        pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic +
                        pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
              data = tx_bb_entrepreneur_merged)
## Generate Table
stargazer(model_fcc_vd, model_fcc_hvd, model_fcc_pro, title = "Regression Results", type = "html",
          align = TRUE, dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("FCC Broadband Availability","Rurality Index (2010)","Population","Black","Native American",
                               "Asian","Hispanic","Agriculture","Construction","Wholesale","Retail","Transportation","IT",
                               "Finance","Professional","Education","Other occupation","Public","Highschool","College",
                               "Millenials","GenX","Babyboomer","Foreign Born","Median Income (2017)"),
          omit.stat = c("ser"), no.space = TRUE)
```

<table style="text-align:center">

<caption>

<strong>Regression Results</strong>

</caption>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Venture Density

</td>

<td>

Highly Active VD

</td>

<td>

Proprietors Share

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

FCC Broadband Availability

</td>

<td>

0.003

</td>

<td>

\-0.055

</td>

<td>

\-0.015

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.385)

</td>

<td>

(0.083)

</td>

<td>

(0.020)

</td>

</tr>

<tr>

<td style="text-align:left">

Rurality Index (2010)

</td>

<td>

\-4.536<sup>\*</sup>

</td>

<td>

\-0.637

</td>

<td>

0.131

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.716)

</td>

<td>

(0.581)

</td>

<td>

(0.137)

</td>

</tr>

<tr>

<td style="text-align:left">

Population

</td>

<td>

\-0.00000

</td>

<td>

\-0.00000

</td>

<td>

0.00000

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

Black

</td>

<td>

\-0.032<sup>\*</sup>

</td>

<td>

\-0.005

</td>

<td>

\-0.003<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.019)

</td>

<td>

(0.004)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Native American

</td>

<td>

\-0.187

</td>

<td>

\-0.047

</td>

<td>

\-0.010

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.218)

</td>

<td>

(0.047)

</td>

<td>

(0.011)

</td>

</tr>

<tr>

<td style="text-align:left">

Asian

</td>

<td>

\-0.007

</td>

<td>

\-0.007

</td>

<td>

0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.063)

</td>

<td>

(0.014)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Hispanic

</td>

<td>

\-0.017<sup>\*\*</sup>

</td>

<td>

\-0.002

</td>

<td>

\-0.002<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

<td>

(0.0004)

</td>

</tr>

<tr>

<td style="text-align:left">

Agriculture

</td>

<td>

0.009

</td>

<td>

\-0.004

</td>

<td>

0.005<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.022)

</td>

<td>

(0.005)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Construction

</td>

<td>

0.055

</td>

<td>

0.010

</td>

<td>

0.007<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.039)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Wholesale

</td>

<td>

\-0.029

</td>

<td>

0.004

</td>

<td>

0.007<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.084)

</td>

<td>

(0.018)

</td>

<td>

(0.004)

</td>

</tr>

<tr>

<td style="text-align:left">

Retail

</td>

<td>

0.065

</td>

<td>

0.017<sup>\*\*</sup>

</td>

<td>

0.005<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.039)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Transportation

</td>

<td>

0.047

</td>

<td>

0.006

</td>

<td>

0.008<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.041)

</td>

<td>

(0.009)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

IT

</td>

<td>

0.117

</td>

<td>

0.012

</td>

<td>

0.015<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.118)

</td>

<td>

(0.025)

</td>

<td>

(0.006)

</td>

</tr>

<tr>

<td style="text-align:left">

Finance

</td>

<td>

0.080

</td>

<td>

\-0.006

</td>

<td>

0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.063)

</td>

<td>

(0.013)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Professional

</td>

<td>

0.133<sup>\*\*</sup>

</td>

<td>

0.031<sup>\*\*\*</sup>

</td>

<td>

0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.053)

</td>

<td>

(0.012)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

\-0.048<sup>\*</sup>

</td>

<td>

\-0.013<sup>\*\*</sup>

</td>

<td>

0.004<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.027)

</td>

<td>

(0.006)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Other occupation

</td>

<td>

\-0.042

</td>

<td>

0.005

</td>

<td>

0.010<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.056)

</td>

<td>

(0.012)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Public

</td>

<td>

0.020

</td>

<td>

0.001

</td>

<td>

0.005<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.037)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Highschool

</td>

<td>

\-0.088<sup>\*\*\*</sup>

</td>

<td>

\-0.014<sup>\*\*</sup>

</td>

<td>

\-0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.033)

</td>

<td>

(0.007)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

College

</td>

<td>

0.184<sup>\*\*\*</sup>

</td>

<td>

0.042<sup>\*\*\*</sup>

</td>

<td>

0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.025)

</td>

<td>

(0.005)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Millenials

</td>

<td>

0.024

</td>

<td>

0.008

</td>

<td>

\-0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.037)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

GenX

</td>

<td>

0.160<sup>\*\*\*</sup>

</td>

<td>

0.024<sup>\*\*</sup>

</td>

<td>

0.003

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.047)

</td>

<td>

(0.010)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Babyboomer

</td>

<td>

0.078<sup>\*</sup>

</td>

<td>

0.028<sup>\*\*\*</sup>

</td>

<td>

0.009<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.043)

</td>

<td>

(0.009)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Foreign Born

</td>

<td>

\-0.034

</td>

<td>

\-0.003

</td>

<td>

\-0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.026)

</td>

<td>

(0.006)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Median Income (2017)

</td>

<td>

\-0.00001

</td>

<td>

0.00000

</td>

<td>

0.00000

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00001)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

2.334

</td>

<td>

\-0.123

</td>

<td>

\-0.213

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(4.194)

</td>

<td>

(0.902)

</td>

<td>

(0.213)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

225

</td>

<td>

227

</td>

<td>

227

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.677

</td>

<td>

0.707

</td>

<td>

0.699

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.636

</td>

<td>

0.670

</td>

<td>

0.661

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

16.688<sup>\*\*\*</sup> (df = 25; 199)

</td>

<td>

19.383<sup>\*\*\*</sup> (df = 25; 201)

</td>

<td>

18.642<sup>\*\*\*</sup> (df = 25; 201)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

``` r
## Standardized Models
std_model_fcc_vd <- lm(scale(venturedensity_mean) ~ scale(pct_broadband_FCC) + scale(IRR2010) + scale(population) + scale(pctblack) +
                scale(pctnative) + scale(pctasian) + scale(pcthispanic) + scale(pctagriculture) + scale(pctconstruction) +
                scale(pctwholesale) + scale(pctretail) + scale(pcttransportation) + scale(pctinformation_tech) + scale(pctfinance) +
                scale(pctprofessional) + scale(pcteducation) + scale(pctother_occupation) + scale(pctpublic) + scale(pcthighschool) +
                scale(pctcollege) + scale(pctmillennial) + scale(pctgenx) + scale(pctbabyboomer) + scale(pctforeignborn) +
                scale(medinc2017),
              data = tx_bb_entrepreneur_merged)

std_model_fcc_hvd <- lm(scale(highlyactive_vd_mean) ~ scale(pct_broadband_FCC) + scale(IRR2010) + scale(population) + scale(pctblack) +
                scale(pctnative) + scale(pctasian) + scale(pcthispanic) + scale(pctagriculture) + scale(pctconstruction) +
                scale(pctwholesale) + scale(pctretail) + scale(pcttransportation) + scale(pctinformation_tech) + scale(pctfinance) +
                scale(pctprofessional) + scale(pcteducation) + scale(pctother_occupation) + scale(pctpublic) + scale(pcthighschool) +
                scale(pctcollege) + scale(pctmillennial) + scale(pctgenx) + scale(pctbabyboomer) + scale(pctforeignborn) +
                scale(medinc2017),
              data = tx_bb_entrepreneur_merged)

std_model_fcc_pro <- lm(scale(pct_proprietors_employment_2017) ~ scale(pct_broadband_FCC) + scale(IRR2010) + scale(population) + scale(pctblack) +
                scale(pctnative) + scale(pctasian) + scale(pcthispanic) + scale(pctagriculture) + scale(pctconstruction) +
                scale(pctwholesale) + scale(pctretail) + scale(pcttransportation) + scale(pctinformation_tech) + scale(pctfinance) +
                scale(pctprofessional) + scale(pcteducation) + scale(pctother_occupation) + scale(pctpublic) + scale(pcthighschool) +
                scale(pctcollege) + scale(pctmillennial) + scale(pctgenx) + scale(pctbabyboomer) + scale(pctforeignborn) +
                scale(medinc2017),
              data = tx_bb_entrepreneur_merged)
## Generate Table
stargazer(std_model_fcc_vd, std_model_fcc_hvd, std_model_fcc_pro, title = "Standardized Regression Results", type = "html",
          align = TRUE, dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("FCC Broadband Availability","Rurality Index (2010)","Population","Black","Native American",
                               "Asian","Hispanic","Agriculture","Construction","Wholesale","Retail","Transportation","IT",
                               "Finance","Professional","Education","Other occupation","Public","Highschool","College",
                               "Millenials","GenX","Babyboomer","Foreign Born","Median Income (2017)"),
          omit.stat = c("ser"), no.space = TRUE)
```

<table style="text-align:center">

<caption>

<strong>Standardized Regression Results</strong>

</caption>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Venture Density

</td>

<td>

Highly Active VD

</td>

<td>

Proprietors Share

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

FCC Broadband Availability

</td>

<td>

0.0005

</td>

<td>

\-0.036

</td>

<td>

\-0.039

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.057)

</td>

<td>

(0.054)

</td>

<td>

(0.053)

</td>

</tr>

<tr>

<td style="text-align:left">

Rurality Index (2010)

</td>

<td>

\-0.215<sup>\*</sup>

</td>

<td>

\-0.134

</td>

<td>

0.114

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.129)

</td>

<td>

(0.123)

</td>

<td>

(0.119)

</td>

</tr>

<tr>

<td style="text-align:left">

Population

</td>

<td>

\-0.076

</td>

<td>

\-0.045

</td>

<td>

0.057

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.068)

</td>

<td>

(0.064)

</td>

<td>

(0.063)

</td>

</tr>

<tr>

<td style="text-align:left">

Black

</td>

<td>

\-0.110<sup>\*</sup>

</td>

<td>

\-0.077

</td>

<td>

\-0.217<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.066)

</td>

<td>

(0.063)

</td>

<td>

(0.062)

</td>

</tr>

<tr>

<td style="text-align:left">

Native American

</td>

<td>

\-0.038

</td>

<td>

\-0.042

</td>

<td>

\-0.038

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.044)

</td>

<td>

(0.042)

</td>

<td>

(0.041)

</td>

</tr>

<tr>

<td style="text-align:left">

Asian

</td>

<td>

\-0.007

</td>

<td>

\-0.032

</td>

<td>

0.044

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.063)

</td>

<td>

(0.061)

</td>

<td>

(0.059)

</td>

</tr>

<tr>

<td style="text-align:left">

Hispanic

</td>

<td>

\-0.199<sup>\*\*</sup>

</td>

<td>

\-0.083

</td>

<td>

\-0.334<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.099)

</td>

<td>

(0.095)

</td>

<td>

(0.093)

</td>

</tr>

<tr>

<td style="text-align:left">

Agriculture

</td>

<td>

0.041

</td>

<td>

\-0.086

</td>

<td>

0.398<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.097)

</td>

<td>

(0.089)

</td>

<td>

(0.087)

</td>

</tr>

<tr>

<td style="text-align:left">

Construction

</td>

<td>

0.076

</td>

<td>

0.063

</td>

<td>

0.172<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.054)

</td>

<td>

(0.050)

</td>

<td>

(0.048)

</td>

</tr>

<tr>

<td style="text-align:left">

Wholesale

</td>

<td>

\-0.018

</td>

<td>

0.010

</td>

<td>

0.079<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.051)

</td>

<td>

(0.049)

</td>

<td>

(0.047)

</td>

</tr>

<tr>

<td style="text-align:left">

Retail

</td>

<td>

0.095

</td>

<td>

0.112<sup>\*\*</sup>

</td>

<td>

0.142<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.058)

</td>

<td>

(0.055)

</td>

<td>

(0.054)

</td>

</tr>

<tr>

<td style="text-align:left">

Transportation

</td>

<td>

0.056

</td>

<td>

0.033

</td>

<td>

0.167<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.049)

</td>

<td>

(0.047)

</td>

<td>

(0.046)

</td>

</tr>

<tr>

<td style="text-align:left">

IT

</td>

<td>

0.049

</td>

<td>

0.023

</td>

<td>

0.118<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.049)

</td>

<td>

(0.047)

</td>

<td>

(0.046)

</td>

</tr>

<tr>

<td style="text-align:left">

Finance

</td>

<td>

0.073

</td>

<td>

\-0.023

</td>

<td>

0.027

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.058)

</td>

<td>

(0.055)

</td>

<td>

(0.053)

</td>

</tr>

<tr>

<td style="text-align:left">

Professional

</td>

<td>

0.197<sup>\*\*</sup>

</td>

<td>

0.204<sup>\*\*\*</sup>

</td>

<td>

0.031

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.079)

</td>

<td>

(0.075)

</td>

<td>

(0.074)

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

\-0.111<sup>\*</sup>

</td>

<td>

\-0.134<sup>\*\*</sup>

</td>

<td>

0.184<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.061)

</td>

<td>

(0.059)

</td>

<td>

(0.057)

</td>

</tr>

<tr>

<td style="text-align:left">

Other occupation

</td>

<td>

\-0.035

</td>

<td>

0.017

</td>

<td>

0.153<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.047)

</td>

<td>

(0.044)

</td>

<td>

(0.043)

</td>

</tr>

<tr>

<td style="text-align:left">

Public

</td>

<td>

0.030

</td>

<td>

0.004

</td>

<td>

0.136<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.056)

</td>

<td>

(0.054)

</td>

<td>

(0.053)

</td>

</tr>

<tr>

<td style="text-align:left">

Highschool

</td>

<td>

\-0.364<sup>\*\*\*</sup>

</td>

<td>

\-0.265<sup>\*\*</sup>

</td>

<td>

\-0.131

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.138)

</td>

<td>

(0.132)

</td>

<td>

(0.129)

</td>

</tr>

<tr>

<td style="text-align:left">

College

</td>

<td>

0.707<sup>\*\*\*</sup>

</td>

<td>

0.727<sup>\*\*\*</sup>

</td>

<td>

0.108

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.096)

</td>

<td>

(0.091)

</td>

<td>

(0.089)

</td>

</tr>

<tr>

<td style="text-align:left">

Millenials

</td>

<td>

0.053

</td>

<td>

0.079

</td>

<td>

\-0.076

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.081)

</td>

<td>

(0.077)

</td>

<td>

(0.075)

</td>

</tr>

<tr>

<td style="text-align:left">

GenX

</td>

<td>

0.200<sup>\*\*\*</sup>

</td>

<td>

0.133<sup>\*\*</sup>

</td>

<td>

0.075

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.058)

</td>

<td>

(0.056)

</td>

<td>

(0.054)

</td>

</tr>

<tr>

<td style="text-align:left">

Babyboomer

</td>

<td>

0.192<sup>\*</sup>

</td>

<td>

0.314<sup>\*\*\*</sup>

</td>

<td>

0.402<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.107)

</td>

<td>

(0.101)

</td>

<td>

(0.098)

</td>

</tr>

<tr>

<td style="text-align:left">

Foreign Born

</td>

<td>

\-0.129

</td>

<td>

\-0.050

</td>

<td>

\-0.072

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.098)

</td>

<td>

(0.093)

</td>

<td>

(0.091)

</td>

</tr>

<tr>

<td style="text-align:left">

Median Income (2017)

</td>

<td>

\-0.038

</td>

<td>

0.011

</td>

<td>

0.031

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.081)

</td>

<td>

(0.077)

</td>

<td>

(0.075)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

\-0.008

</td>

<td>

\-0.001

</td>

<td>

\-0.020

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.042)

</td>

<td>

(0.041)

</td>

<td>

(0.040)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

225

</td>

<td>

227

</td>

<td>

227

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.677

</td>

<td>

0.707

</td>

<td>

0.699

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.636

</td>

<td>

0.670

</td>

<td>

0.661

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

16.688<sup>\*\*\*</sup> (df = 25; 199)

</td>

<td>

19.383<sup>\*\*\*</sup> (df = 25; 201)

</td>

<td>

18.642<sup>\*\*\*</sup> (df = 25; 201)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

> #### MS Broadband Availability

``` r
#### Model 2 ####

## DV: Venture Density, Highly Active VD, Proprietors' Share
## IV: Broadband Availability (MS)

model_ms_vd <- lm(venturedensity_mean ~ pct_broadband_MS + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
              data = tx_bb_entrepreneur_merged)

model_ms_hvd <- lm(highlyactive_vd_mean ~ pct_broadband_MS + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
              data = tx_bb_entrepreneur_merged)

model_ms_pro <- lm(pct_proprietors_employment_2017 ~ pct_broadband_MS + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
              data = tx_bb_entrepreneur_merged)
## Generate Table
stargazer(model_ms_vd, model_ms_hvd, model_ms_pro, title = "Regression Results", type = "html",
          align = TRUE, dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("MS Broadband Availability","Rurality Index (2010)","Population","Black","Native American",
                               "Asian","Hispanic","Agriculture","Construction","Wholesale","Retail","Transportation","IT",
                               "Finance","Professional","Education","Other occupation","Public","Highschool","College",
                               "Millenials","GenX","Babyboomer","Foreign Born","Median Income (2017)"),
          omit.stat = c("ser"), no.space = TRUE)
```

<table style="text-align:center">

<caption>

<strong>Regression Results</strong>

</caption>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Venture Density

</td>

<td>

Highly Active VD

</td>

<td>

Proprietors Share

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

MS Broadband Availability

</td>

<td>

1.685<sup>\*</sup>

</td>

<td>

0.303

</td>

<td>

\-0.082<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.945)

</td>

<td>

(0.204)

</td>

<td>

(0.048)

</td>

</tr>

<tr>

<td style="text-align:left">

Rurality Index (2010)

</td>

<td>

\-2.924

</td>

<td>

\-0.261

</td>

<td>

0.094

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.752)

</td>

<td>

(0.595)

</td>

<td>

(0.139)

</td>

</tr>

<tr>

<td style="text-align:left">

Population

</td>

<td>

\-0.00000

</td>

<td>

\-0.00000

</td>

<td>

0.000

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

Black

</td>

<td>

\-0.030<sup>\*</sup>

</td>

<td>

\-0.004

</td>

<td>

\-0.003<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.018)

</td>

<td>

(0.004)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Native American

</td>

<td>

\-0.200

</td>

<td>

\-0.050

</td>

<td>

\-0.010

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.215)

</td>

<td>

(0.047)

</td>

<td>

(0.011)

</td>

</tr>

<tr>

<td style="text-align:left">

Asian

</td>

<td>

\-0.016

</td>

<td>

\-0.010

</td>

<td>

0.003

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.063)

</td>

<td>

(0.014)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Hispanic

</td>

<td>

\-0.018<sup>\*\*</sup>

</td>

<td>

\-0.002

</td>

<td>

\-0.002<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

<td>

(0.0004)

</td>

</tr>

<tr>

<td style="text-align:left">

Agriculture

</td>

<td>

0.011

</td>

<td>

\-0.004

</td>

<td>

0.005<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.021)

</td>

<td>

(0.004)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Construction

</td>

<td>

0.053

</td>

<td>

0.009

</td>

<td>

0.006<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.038)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Wholesale

</td>

<td>

\-0.036

</td>

<td>

0.0004

</td>

<td>

0.008<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.081)

</td>

<td>

(0.017)

</td>

<td>

(0.004)

</td>

</tr>

<tr>

<td style="text-align:left">

Retail

</td>

<td>

0.051

</td>

<td>

0.012

</td>

<td>

0.005<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.038)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Transportation

</td>

<td>

0.040

</td>

<td>

0.005

</td>

<td>

0.008<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.040)

</td>

<td>

(0.009)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

IT

</td>

<td>

0.128

</td>

<td>

0.019

</td>

<td>

0.016<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.115)

</td>

<td>

(0.025)

</td>

<td>

(0.006)

</td>

</tr>

<tr>

<td style="text-align:left">

Finance

</td>

<td>

0.059

</td>

<td>

\-0.008

</td>

<td>

0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.062)

</td>

<td>

(0.013)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Professional

</td>

<td>

0.120<sup>\*\*</sup>

</td>

<td>

0.029<sup>\*\*\*</sup>

</td>

<td>

0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.051)

</td>

<td>

(0.011)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

\-0.039

</td>

<td>

\-0.010<sup>\*</sup>

</td>

<td>

0.004<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.026)

</td>

<td>

(0.006)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Other occupation

</td>

<td>

\-0.029

</td>

<td>

0.008

</td>

<td>

0.010<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.055)

</td>

<td>

(0.012)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Public

</td>

<td>

0.007

</td>

<td>

\-0.003

</td>

<td>

0.005<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.037)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Highschool

</td>

<td>

\-0.077<sup>\*\*</sup>

</td>

<td>

\-0.012<sup>\*</sup>

</td>

<td>

\-0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.032)

</td>

<td>

(0.007)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

College

</td>

<td>

0.158<sup>\*\*\*</sup>

</td>

<td>

0.036<sup>\*\*\*</sup>

</td>

<td>

0.002<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.025)

</td>

<td>

(0.005)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Millenials

</td>

<td>

0.033

</td>

<td>

0.010

</td>

<td>

\-0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.036)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

GenX

</td>

<td>

0.160<sup>\*\*\*</sup>

</td>

<td>

0.023<sup>\*\*</sup>

</td>

<td>

0.003

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.046)

</td>

<td>

(0.010)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Babyboomer

</td>

<td>

0.096<sup>\*\*</sup>

</td>

<td>

0.033<sup>\*\*\*</sup>

</td>

<td>

0.008<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.043)

</td>

<td>

(0.009)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Foreign Born

</td>

<td>

\-0.024

</td>

<td>

\-0.0003

</td>

<td>

\-0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.025)

</td>

<td>

(0.005)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Median Income (2017)

</td>

<td>

\-0.00001

</td>

<td>

0.00000

</td>

<td>

0.00000

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00001)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

0.180

</td>

<td>

\-0.664

</td>

<td>

\-0.164

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(4.087)

</td>

<td>

(0.886)

</td>

<td>

(0.207)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

230

</td>

<td>

232

</td>

<td>

232

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.678

</td>

<td>

0.702

</td>

<td>

0.702

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.638

</td>

<td>

0.666

</td>

<td>

0.666

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

17.161<sup>\*\*\*</sup> (df = 25; 204)

</td>

<td>

19.402<sup>\*\*\*</sup> (df = 25; 206)

</td>

<td>

19.398<sup>\*\*\*</sup> (df = 25; 206)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

``` r
## Standardized Models
std_model_ms_vd <- lm(scale(venturedensity_mean) ~ scale(pct_broadband_MS) + scale(IRR2010) + scale(population) + scale(pctblack) +
                scale(pctnative) + scale(pctasian) + scale(pcthispanic) + scale(pctagriculture) + scale(pctconstruction) +
                scale(pctwholesale) + scale(pctretail) + scale(pcttransportation) + scale(pctinformation_tech) + scale(pctfinance) +
                scale(pctprofessional) + scale(pcteducation) + scale(pctother_occupation) + scale(pctpublic) + scale(pcthighschool) +
                scale(pctcollege) + scale(pctmillennial) + scale(pctgenx) + scale(pctbabyboomer) + scale(pctforeignborn) +
                scale(medinc2017),
              data = tx_bb_entrepreneur_merged)

std_model_ms_hvd <- lm(scale(highlyactive_vd_mean) ~ scale(pct_broadband_MS) + scale(IRR2010) + scale(population) + scale(pctblack) +
                scale(pctnative) + scale(pctasian) + scale(pcthispanic) + scale(pctagriculture) + scale(pctconstruction) +
                scale(pctwholesale) + scale(pctretail) + scale(pcttransportation) + scale(pctinformation_tech) + scale(pctfinance) +
                scale(pctprofessional) + scale(pcteducation) + scale(pctother_occupation) + scale(pctpublic) + scale(pcthighschool) +
                scale(pctcollege) + scale(pctmillennial) + scale(pctgenx) + scale(pctbabyboomer) + scale(pctforeignborn) +
                scale(medinc2017),
              data = tx_bb_entrepreneur_merged)

std_model_ms_pro <- lm(scale(pct_proprietors_employment_2017) ~ scale(pct_broadband_MS) + scale(IRR2010) + scale(population) + scale(pctblack) +
                scale(pctnative) + scale(pctasian) + scale(pcthispanic) + scale(pctagriculture) + scale(pctconstruction) +
                scale(pctwholesale) + scale(pctretail) + scale(pcttransportation) + scale(pctinformation_tech) + scale(pctfinance) +
                scale(pctprofessional) + scale(pcteducation) + scale(pctother_occupation) + scale(pctpublic) + scale(pcthighschool) +
                scale(pctcollege) + scale(pctmillennial) + scale(pctgenx) + scale(pctbabyboomer) + scale(pctforeignborn) +
                scale(medinc2017),
              data = tx_bb_entrepreneur_merged)
## Generate Table
stargazer(std_model_ms_vd, std_model_ms_hvd, std_model_ms_pro, title = "Standardized Regression Results", type = "html",
          align = TRUE, dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("MS Broadband Availability","Rurality Index (2010)","Population","Black","Native American",
                               "Asian","Hispanic","Agriculture","Construction","Wholesale","Retail","Transportation","IT",
                               "Finance","Professional","Education","Other occupation","Public","Highschool","College",
                               "Millenials","GenX","Babyboomer","Foreign Born","Median Income (2017)"),
          omit.stat = c("ser"), no.space = TRUE)
```

<table style="text-align:center">

<caption>

<strong>Standardized Regression Results</strong>

</caption>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Venture Density

</td>

<td>

Highly Active VD

</td>

<td>

Proprietors Share

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

MS Broadband Availability

</td>

<td>

0.160<sup>\*</sup>

</td>

<td>

0.128

</td>

<td>

\-0.142<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.090)

</td>

<td>

(0.086)

</td>

<td>

(0.083)

</td>

</tr>

<tr>

<td style="text-align:left">

Rurality Index (2010)

</td>

<td>

\-0.139

</td>

<td>

\-0.055

</td>

<td>

0.082

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.130)

</td>

<td>

(0.125)

</td>

<td>

(0.121)

</td>

</tr>

<tr>

<td style="text-align:left">

Population

</td>

<td>

\-0.046

</td>

<td>

\-0.018

</td>

<td>

0.039

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.068)

</td>

<td>

(0.065)

</td>

<td>

(0.063)

</td>

</tr>

<tr>

<td style="text-align:left">

Black

</td>

<td>

\-0.105<sup>\*</sup>

</td>

<td>

\-0.060

</td>

<td>

\-0.215<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.063)

</td>

<td>

(0.061)

</td>

<td>

(0.059)

</td>

</tr>

<tr>

<td style="text-align:left">

Native American

</td>

<td>

\-0.040

</td>

<td>

\-0.045

</td>

<td>

\-0.036

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.043)

</td>

<td>

(0.042)

</td>

<td>

(0.040)

</td>

</tr>

<tr>

<td style="text-align:left">

Asian

</td>

<td>

\-0.016

</td>

<td>

\-0.044

</td>

<td>

0.047

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.063)

</td>

<td>

(0.061)

</td>

<td>

(0.058)

</td>

</tr>

<tr>

<td style="text-align:left">

Hispanic

</td>

<td>

\-0.209<sup>\*\*</sup>

</td>

<td>

\-0.092

</td>

<td>

\-0.344<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.097)

</td>

<td>

(0.093)

</td>

<td>

(0.090)

</td>

</tr>

<tr>

<td style="text-align:left">

Agriculture

</td>

<td>

0.048

</td>

<td>

\-0.076

</td>

<td>

0.384<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.093)

</td>

<td>

(0.087)

</td>

<td>

(0.084)

</td>

</tr>

<tr>

<td style="text-align:left">

Construction

</td>

<td>

0.073

</td>

<td>

0.058

</td>

<td>

0.164<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.052)

</td>

<td>

(0.048)

</td>

<td>

(0.046)

</td>

</tr>

<tr>

<td style="text-align:left">

Wholesale

</td>

<td>

\-0.022

</td>

<td>

0.001

</td>

<td>

0.087<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.049)

</td>

<td>

(0.047)

</td>

<td>

(0.046)

</td>

</tr>

<tr>

<td style="text-align:left">

Retail

</td>

<td>

0.075

</td>

<td>

0.079

</td>

<td>

0.148<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.055)

</td>

<td>

(0.054)

</td>

<td>

(0.052)

</td>

</tr>

<tr>

<td style="text-align:left">

Transportation

</td>

<td>

0.047

</td>

<td>

0.028

</td>

<td>

0.178<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.048)

</td>

<td>

(0.046)

</td>

<td>

(0.045)

</td>

</tr>

<tr>

<td style="text-align:left">

IT

</td>

<td>

0.054

</td>

<td>

0.036

</td>

<td>

0.124<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.048)

</td>

<td>

(0.046)

</td>

<td>

(0.045)

</td>

</tr>

<tr>

<td style="text-align:left">

Finance

</td>

<td>

0.054

</td>

<td>

\-0.034

</td>

<td>

0.041

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.057)

</td>

<td>

(0.054)

</td>

<td>

(0.052)

</td>

</tr>

<tr>

<td style="text-align:left">

Professional

</td>

<td>

0.176<sup>\*\*</sup>

</td>

<td>

0.190<sup>\*\*\*</sup>

</td>

<td>

0.064

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.076)

</td>

<td>

(0.073)

</td>

<td>

(0.071)

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

\-0.089

</td>

<td>

\-0.107<sup>\*</sup>

</td>

<td>

0.181<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.059)

</td>

<td>

(0.058)

</td>

<td>

(0.055)

</td>

</tr>

<tr>

<td style="text-align:left">

Other occupation

</td>

<td>

\-0.024

</td>

<td>

0.031

</td>

<td>

0.157<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.046)

</td>

<td>

(0.043)

</td>

<td>

(0.042)

</td>

</tr>

<tr>

<td style="text-align:left">

Public

</td>

<td>

0.010

</td>

<td>

\-0.021

</td>

<td>

0.148<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.056)

</td>

<td>

(0.054)

</td>

<td>

(0.052)

</td>

</tr>

<tr>

<td style="text-align:left">

Highschool

</td>

<td>

\-0.317<sup>\*\*</sup>

</td>

<td>

\-0.214<sup>\*</sup>

</td>

<td>

\-0.162

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.133)

</td>

<td>

(0.129)

</td>

<td>

(0.124)

</td>

</tr>

<tr>

<td style="text-align:left">

College

</td>

<td>

0.607<sup>\*\*\*</sup>

</td>

<td>

0.623<sup>\*\*\*</sup>

</td>

<td>

0.165<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.098)

</td>

<td>

(0.094)

</td>

<td>

(0.091)

</td>

</tr>

<tr>

<td style="text-align:left">

Millenials

</td>

<td>

0.072

</td>

<td>

0.094

</td>

<td>

\-0.087

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.080)

</td>

<td>

(0.077)

</td>

<td>

(0.074)

</td>

</tr>

<tr>

<td style="text-align:left">

GenX

</td>

<td>

0.200<sup>\*\*\*</sup>

</td>

<td>

0.129<sup>\*\*</sup>

</td>

<td>

0.068

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.058)

</td>

<td>

(0.056)

</td>

<td>

(0.054)

</td>

</tr>

<tr>

<td style="text-align:left">

Babyboomer

</td>

<td>

0.237<sup>\*\*</sup>

</td>

<td>

0.359<sup>\*\*\*</sup>

</td>

<td>

0.375<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.106)

</td>

<td>

(0.101)

</td>

<td>

(0.097)

</td>

</tr>

<tr>

<td style="text-align:left">

Foreign Born

</td>

<td>

\-0.092

</td>

<td>

\-0.005

</td>

<td>

\-0.075

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.095)

</td>

<td>

(0.092)

</td>

<td>

(0.089)

</td>

</tr>

<tr>

<td style="text-align:left">

Median Income (2017)

</td>

<td>

\-0.037

</td>

<td>

0.025

</td>

<td>

0.047

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.080)

</td>

<td>

(0.077)

</td>

<td>

(0.074)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

\-0.007

</td>

<td>

0.002

</td>

<td>

\-0.020

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.041)

</td>

<td>

(0.040)

</td>

<td>

(0.039)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

230

</td>

<td>

232

</td>

<td>

232

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.678

</td>

<td>

0.702

</td>

<td>

0.702

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.638

</td>

<td>

0.666

</td>

<td>

0.666

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

17.161<sup>\*\*\*</sup> (df = 25; 204)

</td>

<td>

19.402<sup>\*\*\*</sup> (df = 25; 206)

</td>

<td>

19.398<sup>\*\*\*</sup> (df = 25; 206)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

> #### ACS Broadband Subscription Level

``` r
#### Model 3 ####

## DV: Venture Density, Highly Active VD, Proprietors' Share
## IV: Broadband Subscription (ACS)

model_acs_vd <- lm(venturedensity_mean ~ pctbbfrac_ASU + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
              data = tx_bb_entrepreneur_merged)

model_acs_hvd <- lm(highlyactive_vd_mean ~ pctbbfrac_ASU + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
              data = tx_bb_entrepreneur_merged)

model_acs_pro <- lm(pct_proprietors_employment_2017 ~ pctbbfrac_ASU + IRR2010 + population + pctblack + pctnative + pctasian + 
                      pcthispanic + pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + 
                      pctinformation_tech + pctfinance + pctprofessional + pcteducation + pctother_occupation + pctpublic + 
                      pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
              data = tx_bb_entrepreneur_merged)
## Generate Table
stargazer(model_acs_vd, model_acs_hvd, model_acs_pro, title = "Regression Results", type = "html",
          align = TRUE, dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("ACS Broadband Subscription","Rurality Index (2010)","Population","Black","Native American",
                               "Asian","Hispanic","Agriculture","Construction","Wholesale","Retail","Transportation","IT",
                               "Finance","Professional","Education","Other occupation","Public","Highschool","College",
                               "Millenials","GenX","Babyboomer","Foreign Born","Median Income (2017)"),
          omit.stat = c("ser"), no.space = TRUE)
```

<table style="text-align:center">

<caption>

<strong>Regression Results</strong>

</caption>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Venture Density

</td>

<td>

Highly Active VD

</td>

<td>

Proprietors Share

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

ACS Broadband Subscription

</td>

<td>

\-0.118

</td>

<td>

\-0.294

</td>

<td>

\-0.167<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.769)

</td>

<td>

(0.384)

</td>

<td>

(0.089)

</td>

</tr>

<tr>

<td style="text-align:left">

Rurality Index (2010)

</td>

<td>

\-4.778<sup>\*</sup>

</td>

<td>

\-0.715

</td>

<td>

0.112

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.686)

</td>

<td>

(0.578)

</td>

<td>

(0.134)

</td>

</tr>

<tr>

<td style="text-align:left">

Population

</td>

<td>

\-0.00000

</td>

<td>

\-0.00000

</td>

<td>

0.000

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

Black

</td>

<td>

\-0.033<sup>\*</sup>

</td>

<td>

\-0.005

</td>

<td>

\-0.004<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.019)

</td>

<td>

(0.004)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Native American

</td>

<td>

\-0.187

</td>

<td>

\-0.047

</td>

<td>

\-0.009

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.217)

</td>

<td>

(0.047)

</td>

<td>

(0.011)

</td>

</tr>

<tr>

<td style="text-align:left">

Asian

</td>

<td>

\-0.005

</td>

<td>

\-0.008

</td>

<td>

0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.063)

</td>

<td>

(0.014)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Hispanic

</td>

<td>

\-0.016<sup>\*</sup>

</td>

<td>

\-0.002

</td>

<td>

\-0.002<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

<td>

(0.0004)

</td>

</tr>

<tr>

<td style="text-align:left">

Agriculture

</td>

<td>

0.010

</td>

<td>

\-0.004

</td>

<td>

0.005<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.021)

</td>

<td>

(0.004)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Construction

</td>

<td>

0.054

</td>

<td>

0.010

</td>

<td>

0.007<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.038)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Wholesale

</td>

<td>

\-0.035

</td>

<td>

0.001

</td>

<td>

0.008<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.081)

</td>

<td>

(0.018)

</td>

<td>

(0.004)

</td>

</tr>

<tr>

<td style="text-align:left">

Retail

</td>

<td>

0.054

</td>

<td>

0.013

</td>

<td>

0.005<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.038)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Transportation

</td>

<td>

0.035

</td>

<td>

0.004

</td>

<td>

0.008<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.040)

</td>

<td>

(0.009)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

IT

</td>

<td>

0.126

</td>

<td>

0.019

</td>

<td>

0.017<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.116)

</td>

<td>

(0.025)

</td>

<td>

(0.006)

</td>

</tr>

<tr>

<td style="text-align:left">

Finance

</td>

<td>

0.072

</td>

<td>

\-0.007

</td>

<td>

0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.062)

</td>

<td>

(0.013)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Professional

</td>

<td>

0.131<sup>\*\*</sup>

</td>

<td>

0.030<sup>\*\*\*</sup>

</td>

<td>

0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.052)

</td>

<td>

(0.011)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

\-0.044<sup>\*</sup>

</td>

<td>

\-0.012<sup>\*\*</sup>

</td>

<td>

0.004<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.026)

</td>

<td>

(0.006)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Other occupation

</td>

<td>

\-0.030

</td>

<td>

0.008

</td>

<td>

0.010<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.055)

</td>

<td>

(0.012)

</td>

<td>

(0.003)

</td>

</tr>

<tr>

<td style="text-align:left">

Public

</td>

<td>

0.019

</td>

<td>

\-0.0001

</td>

<td>

0.005<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.037)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Highschool

</td>

<td>

\-0.080<sup>\*\*</sup>

</td>

<td>

\-0.012

</td>

<td>

\-0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.033)

</td>

<td>

(0.007)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

College

</td>

<td>

0.174<sup>\*\*\*</sup>

</td>

<td>

0.040<sup>\*\*\*</sup>

</td>

<td>

0.002<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.025)

</td>

<td>

(0.005)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Millenials

</td>

<td>

0.027

</td>

<td>

0.009

</td>

<td>

\-0.002

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.037)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

GenX

</td>

<td>

0.150<sup>\*\*\*</sup>

</td>

<td>

0.021<sup>\*\*</sup>

</td>

<td>

0.003

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.047)

</td>

<td>

(0.010)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Babyboomer

</td>

<td>

0.081<sup>\*</sup>

</td>

<td>

0.029<sup>\*\*\*</sup>

</td>

<td>

0.008<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.043)

</td>

<td>

(0.009)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Foreign Born

</td>

<td>

\-0.029

</td>

<td>

\-0.001

</td>

<td>

\-0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.025)

</td>

<td>

(0.005)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Median Income (2017)

</td>

<td>

\-0.00000

</td>

<td>

0.00000

</td>

<td>

0.00000

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00001)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

2.024

</td>

<td>

\-0.136

</td>

<td>

\-0.123

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(4.211)

</td>

<td>

(0.911)

</td>

<td>

(0.211)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

230

</td>

<td>

232

</td>

<td>

232

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.673

</td>

<td>

0.700

</td>

<td>

0.703

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.633

</td>

<td>

0.663

</td>

<td>

0.667

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

16.773<sup>\*\*\*</sup> (df = 25; 204)

</td>

<td>

19.186<sup>\*\*\*</sup> (df = 25; 206)

</td>

<td>

19.474<sup>\*\*\*</sup> (df = 25; 206)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

``` r
## Standardized Models
std_model_acs_vd <- lm(scale(venturedensity_mean) ~ scale(pctbbfrac_ASU) + scale(IRR2010) + scale(population) + scale(pctblack) +
                scale(pctnative) + scale(pctasian) + scale(pcthispanic) + scale(pctagriculture) + scale(pctconstruction) +
                scale(pctwholesale) + scale(pctretail) + scale(pcttransportation) + scale(pctinformation_tech) + scale(pctfinance) +
                scale(pctprofessional) + scale(pcteducation) + scale(pctother_occupation) + scale(pctpublic) + scale(pcthighschool) +
                scale(pctcollege) + scale(pctmillennial) + scale(pctgenx) + scale(pctbabyboomer) + scale(pctforeignborn) +
                scale(medinc2017),
              data = tx_bb_entrepreneur_merged)

std_model_acs_hvd <- lm(scale(highlyactive_vd_mean) ~ scale(pctbbfrac_ASU) + scale(IRR2010) + scale(population) + scale(pctblack) +
                scale(pctnative) + scale(pctasian) + scale(pcthispanic) + scale(pctagriculture) + scale(pctconstruction) +
                scale(pctwholesale) + scale(pctretail) + scale(pcttransportation) + scale(pctinformation_tech) + scale(pctfinance) +
                scale(pctprofessional) + scale(pcteducation) + scale(pctother_occupation) + scale(pctpublic) + scale(pcthighschool) +
                scale(pctcollege) + scale(pctmillennial) + scale(pctgenx) + scale(pctbabyboomer) + scale(pctforeignborn) +
                scale(medinc2017),
              data = tx_bb_entrepreneur_merged)

std_model_acs_pro <- lm(scale(pct_proprietors_employment_2017) ~ scale(pctbbfrac_ASU) + scale(IRR2010) + scale(population) + scale(pctblack) +
                scale(pctnative) + scale(pctasian) + scale(pcthispanic) + scale(pctagriculture) + scale(pctconstruction) +
                scale(pctwholesale) + scale(pctretail) + scale(pcttransportation) + scale(pctinformation_tech) + scale(pctfinance) +
                scale(pctprofessional) + scale(pcteducation) + scale(pctother_occupation) + scale(pctpublic) + scale(pcthighschool) +
                scale(pctcollege) + scale(pctmillennial) + scale(pctgenx) + scale(pctbabyboomer) + scale(pctforeignborn) +
                scale(medinc2017),
              data = tx_bb_entrepreneur_merged)
## Generate Table
stargazer(std_model_acs_vd, std_model_acs_hvd, std_model_acs_pro, title = "Standardized Regression Results", type = "html",
          align = TRUE, dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("ACS Broadband Subscription","Rurality Index (2010)","Population","Black","Native American",
                               "Asian","Hispanic","Agriculture","Construction","Wholesale","Retail","Transportation","IT",
                               "Finance","Professional","Education","Other occupation","Public","Highschool","College",
                               "Millenials","GenX","Babyboomer","Foreign Born","Median Income (2017)"),
          omit.stat = c("ser"), no.space = TRUE)
```

<table style="text-align:center">

<caption>

<strong>Standardized Regression Results</strong>

</caption>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Venture Density

</td>

<td>

Highly Active VD

</td>

<td>

Proprietors Share

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

ACS Broadband Subscription

</td>

<td>

\-0.006

</td>

<td>

\-0.063

</td>

<td>

\-0.147<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.085)

</td>

<td>

(0.082)

</td>

<td>

(0.078)

</td>

</tr>

<tr>

<td style="text-align:left">

Rurality Index (2010)

</td>

<td>

\-0.226<sup>\*</sup>

</td>

<td>

\-0.151

</td>

<td>

0.097

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.127)

</td>

<td>

(0.122)

</td>

<td>

(0.117)

</td>

</tr>

<tr>

<td style="text-align:left">

Population

</td>

<td>

\-0.073

</td>

<td>

\-0.044

</td>

<td>

0.052

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.067)

</td>

<td>

(0.064)

</td>

<td>

(0.061)

</td>

</tr>

<tr>

<td style="text-align:left">

Black

</td>

<td>

\-0.115<sup>\*</sup>

</td>

<td>

\-0.079

</td>

<td>

\-0.236<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.066)

</td>

<td>

(0.063)

</td>

<td>

(0.061)

</td>

</tr>

<tr>

<td style="text-align:left">

Native American

</td>

<td>

\-0.038

</td>

<td>

\-0.042

</td>

<td>

\-0.035

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.044)

</td>

<td>

(0.042)

</td>

<td>

(0.040)

</td>

</tr>

<tr>

<td style="text-align:left">

Asian

</td>

<td>

\-0.005

</td>

<td>

\-0.037

</td>

<td>

0.032

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.063)

</td>

<td>

(0.061)

</td>

<td>

(0.058)

</td>

</tr>

<tr>

<td style="text-align:left">

Hispanic

</td>

<td>

\-0.196<sup>\*</sup>

</td>

<td>

\-0.098

</td>

<td>

\-0.400<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.100)

</td>

<td>

(0.096)

</td>

<td>

(0.092)

</td>

</tr>

<tr>

<td style="text-align:left">

Agriculture

</td>

<td>

0.044

</td>

<td>

\-0.075

</td>

<td>

0.395<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.094)

</td>

<td>

(0.087)

</td>

<td>

(0.084)

</td>

</tr>

<tr>

<td style="text-align:left">

Construction

</td>

<td>

0.074

</td>

<td>

0.063

</td>

<td>

0.171<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.052)

</td>

<td>

(0.048)

</td>

<td>

(0.046)

</td>

</tr>

<tr>

<td style="text-align:left">

Wholesale

</td>

<td>

\-0.022

</td>

<td>

0.003

</td>

<td>

0.091<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.049)

</td>

<td>

(0.047)

</td>

<td>

(0.046)

</td>

</tr>

<tr>

<td style="text-align:left">

Retail

</td>

<td>

0.080

</td>

<td>

0.084

</td>

<td>

0.144<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.056)

</td>

<td>

(0.054)

</td>

<td>

(0.051)

</td>

</tr>

<tr>

<td style="text-align:left">

Transportation

</td>

<td>

0.041

</td>

<td>

0.022

</td>

<td>

0.180<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.048)

</td>

<td>

(0.046)

</td>

<td>

(0.045)

</td>

</tr>

<tr>

<td style="text-align:left">

IT

</td>

<td>

0.053

</td>

<td>

0.036

</td>

<td>

0.128<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.049)

</td>

<td>

(0.047)

</td>

<td>

(0.045)

</td>

</tr>

<tr>

<td style="text-align:left">

Finance

</td>

<td>

0.066

</td>

<td>

\-0.028

</td>

<td>

0.024

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.057)

</td>

<td>

(0.054)

</td>

<td>

(0.052)

</td>

</tr>

<tr>

<td style="text-align:left">

Professional

</td>

<td>

0.193<sup>\*\*</sup>

</td>

<td>

0.197<sup>\*\*\*</sup>

</td>

<td>

0.034

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.076)

</td>

<td>

(0.073)

</td>

<td>

(0.070)

</td>

</tr>

<tr>

<td style="text-align:left">

Education

</td>

<td>

\-0.102<sup>\*</sup>

</td>

<td>

\-0.120<sup>\*\*</sup>

</td>

<td>

0.186<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.060)

</td>

<td>

(0.057)

</td>

<td>

(0.055)

</td>

</tr>

<tr>

<td style="text-align:left">

Other occupation

</td>

<td>

\-0.025

</td>

<td>

0.028

</td>

<td>

0.153<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.046)

</td>

<td>

(0.043)

</td>

<td>

(0.042)

</td>

</tr>

<tr>

<td style="text-align:left">

Public

</td>

<td>

0.029

</td>

<td>

\-0.0004

</td>

<td>

0.146<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.056)

</td>

<td>

(0.054)

</td>

<td>

(0.052)

</td>

</tr>

<tr>

<td style="text-align:left">

Highschool

</td>

<td>

\-0.329<sup>\*\*</sup>

</td>

<td>

\-0.212

</td>

<td>

\-0.122

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.135)

</td>

<td>

(0.130)

</td>

<td>

(0.125)

</td>

</tr>

<tr>

<td style="text-align:left">

College

</td>

<td>

0.668<sup>\*\*\*</sup>

</td>

<td>

0.686<sup>\*\*\*</sup>

</td>

<td>

0.145<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.095)

</td>

<td>

(0.090)

</td>

<td>

(0.087)

</td>

</tr>

<tr>

<td style="text-align:left">

Millenials

</td>

<td>

0.059

</td>

<td>

0.084

</td>

<td>

\-0.073

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.080)

</td>

<td>

(0.077)

</td>

<td>

(0.074)

</td>

</tr>

<tr>

<td style="text-align:left">

GenX

</td>

<td>

0.187<sup>\*\*\*</sup>

</td>

<td>

0.115<sup>\*\*</sup>

</td>

<td>

0.068

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.058)

</td>

<td>

(0.056)

</td>

<td>

(0.054)

</td>

</tr>

<tr>

<td style="text-align:left">

Babyboomer

</td>

<td>

0.201<sup>\*</sup>

</td>

<td>

0.319<sup>\*\*\*</sup>

</td>

<td>

0.369<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.107)

</td>

<td>

(0.102)

</td>

<td>

(0.097)

</td>

</tr>

<tr>

<td style="text-align:left">

Foreign Born

</td>

<td>

\-0.110

</td>

<td>

\-0.017

</td>

<td>

\-0.056

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.096)

</td>

<td>

(0.092)

</td>

<td>

(0.088)

</td>

</tr>

<tr>

<td style="text-align:left">

Median Income (2017)

</td>

<td>

\-0.014

</td>

<td>

0.065

</td>

<td>

0.084

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.086)

</td>

<td>

(0.082)

</td>

<td>

(0.079)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

\-0.008

</td>

<td>

0.0001

</td>

<td>

\-0.025

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.042)

</td>

<td>

(0.040)

</td>

<td>

(0.039)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

230

</td>

<td>

232

</td>

<td>

232

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.673

</td>

<td>

0.700

</td>

<td>

0.703

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.633

</td>

<td>

0.663

</td>

<td>

0.667

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

16.773<sup>\*\*\*</sup> (df = 25; 204)

</td>

<td>

19.186<sup>\*\*\*</sup> (df = 25; 206)

</td>

<td>

19.474<sup>\*\*\*</sup> (df = 25; 206)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

##### Models with Fewer Control Variables

Below, simpler models with fewer, yet important control variables will
be explored. In a previous meeting with Dr.Brian Whitacre, he suggested
age and education might be meaningful control variables. Here I will use
rurality index, population, education, and age cohort variables as
control
variables.

> #### FCC Broadband Availability

``` r
## Fewer Control Variables: Rurality, population, education level, age cohorts
## FCC Broadband
# VD model
model_fcc_vd2 <- lm(venturedensity_mean ~ pct_broadband_FCC + IRR2010 + population + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                    data = tx_bb_entrepreneur_merged)

# HVD model
model_fcc_hvd2 <- lm(highlyactive_vd_mean ~ pct_broadband_FCC + IRR2010 + population + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                    data = tx_bb_entrepreneur_merged)

# Proprietor model
model_fcc_pro2 <- lm(pct_proprietors_employment_2017 ~ pct_broadband_FCC + IRR2010 + population + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                     data = tx_bb_entrepreneur_merged)

## Generate Table
stargazer(model_fcc_vd2, model_fcc_hvd2, model_fcc_pro2, type = "html", align = T,
          dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("FCC Broadband Availability",
                               "Rurality Index (2010)",
                               "Population","Highschool","College",
                               "Millenials","GenX","Babyboomer"),
          omit.stat = c("ser"), no.space = T)
```

<table style="text-align:center">

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Venture Density

</td>

<td>

Highly Active VD

</td>

<td>

Proprietors Share

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

FCC Broadband Availability

</td>

<td>

0.163

</td>

<td>

\-0.018

</td>

<td>

\-0.011

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.347)

</td>

<td>

(0.076)

</td>

<td>

(0.019)

</td>

</tr>

<tr>

<td style="text-align:left">

Rurality Index (2010)

</td>

<td>

\-5.869<sup>\*\*\*</sup>

</td>

<td>

\-1.586<sup>\*\*\*</sup>

</td>

<td>

0.486<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.706)

</td>

<td>

(0.369)

</td>

<td>

(0.095)

</td>

</tr>

<tr>

<td style="text-align:left">

Population

</td>

<td>

\-0.00000

</td>

<td>

\-0.00000

</td>

<td>

0.00000<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

Highschool

</td>

<td>

\-0.026<sup>\*</sup>

</td>

<td>

\-0.007<sup>\*</sup>

</td>

<td>

0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.016)

</td>

<td>

(0.003)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

College

</td>

<td>

0.170<sup>\*\*\*</sup>

</td>

<td>

0.040<sup>\*\*\*</sup>

</td>

<td>

0.001<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.015)

</td>

<td>

(0.003)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Millenials

</td>

<td>

0.012

</td>

<td>

0.002

</td>

<td>

\-0.004<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.033)

</td>

<td>

(0.007)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

GenX

</td>

<td>

0.192<sup>\*\*\*</sup>

</td>

<td>

0.029<sup>\*\*\*</sup>

</td>

<td>

0.004<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.039)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Babyboomer

</td>

<td>

0.122<sup>\*\*\*</sup>

</td>

<td>

0.034<sup>\*\*\*</sup>

</td>

<td>

0.009<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.035)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

\-3.147

</td>

<td>

\-0.302

</td>

<td>

\-0.268<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.250)

</td>

<td>

(0.490)

</td>

<td>

(0.126)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

225

</td>

<td>

227

</td>

<td>

227

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.623

</td>

<td>

0.650

</td>

<td>

0.575

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.609

</td>

<td>

0.637

</td>

<td>

0.559

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

44.566<sup>\*\*\*</sup> (df = 8; 216)

</td>

<td>

50.656<sup>\*\*\*</sup> (df = 8; 218)

</td>

<td>

36.881<sup>\*\*\*</sup> (df = 8; 218)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

> #### MS Broadband Availability

``` r
## MS Broadband
# VD model
model_ms_vd2 <- lm(venturedensity_mean ~ pct_broadband_MS + IRR2010 + population + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                   data = tx_bb_entrepreneur_merged)

# HVD model
model_ms_hvd2 <- lm(highlyactive_vd_mean ~ pct_broadband_MS + IRR2010 + population + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                   data = tx_bb_entrepreneur_merged)

# Proprietor model
model_ms_pro2 <- lm(pct_proprietors_employment_2017 ~ pct_broadband_MS + IRR2010 + population + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                   data = tx_bb_entrepreneur_merged)

## Generate Table
stargazer(model_ms_vd2, model_ms_hvd2, model_ms_pro2, type = "html", align = T,
          dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("MS Broadband Availability",
                               "Rurality Index (2010)",
                               "Population","Highschool","College",
                               "Millenials","GenX","Babyboomer"),
          omit.stat = c("ser"), no.space = T)
```

<table style="text-align:center">

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Venture Density

</td>

<td>

Highly Active VD

</td>

<td>

Proprietors Share

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

MS Broadband Availability

</td>

<td>

2.566<sup>\*\*\*</sup>

</td>

<td>

0.494<sup>\*\*\*</sup>

</td>

<td>

\-0.077

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.862)

</td>

<td>

(0.188)

</td>

<td>

(0.049)

</td>

</tr>

<tr>

<td style="text-align:left">

Rurality Index (2010)

</td>

<td>

\-3.369<sup>\*</sup>

</td>

<td>

\-1.004<sup>\*\*</sup>

</td>

<td>

0.406<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.838)

</td>

<td>

(0.401)

</td>

<td>

(0.104)

</td>

</tr>

<tr>

<td style="text-align:left">

Population

</td>

<td>

\-0.00000

</td>

<td>

\-0.00000

</td>

<td>

0.00000

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

Highschool

</td>

<td>

\-0.024

</td>

<td>

\-0.006<sup>\*</sup>

</td>

<td>

0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.015)

</td>

<td>

(0.003)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

College

</td>

<td>

0.139<sup>\*\*\*</sup>

</td>

<td>

0.034<sup>\*\*\*</sup>

</td>

<td>

0.002<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.018)

</td>

<td>

(0.004)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Millenials

</td>

<td>

0.019

</td>

<td>

0.003

</td>

<td>

\-0.004<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.032)

</td>

<td>

(0.007)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

GenX

</td>

<td>

0.185<sup>\*\*\*</sup>

</td>

<td>

0.028<sup>\*\*\*</sup>

</td>

<td>

0.005<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.037)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Babyboomer

</td>

<td>

0.144<sup>\*\*\*</sup>

</td>

<td>

0.039<sup>\*\*\*</sup>

</td>

<td>

0.008<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.034)

</td>

<td>

(0.007)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

\-4.955<sup>\*\*</sup>

</td>

<td>

\-0.763

</td>

<td>

\-0.218<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.186)

</td>

<td>

(0.480)

</td>

<td>

(0.124)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

230

</td>

<td>

232

</td>

<td>

232

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.635

</td>

<td>

0.657

</td>

<td>

0.580

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.622

</td>

<td>

0.645

</td>

<td>

0.565

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

48.085<sup>\*\*\*</sup> (df = 8; 221)

</td>

<td>

53.393<sup>\*\*\*</sup> (df = 8; 223)

</td>

<td>

38.464<sup>\*\*\*</sup> (df = 8; 223)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

> #### ACS Broadband Subscription Level

``` r
## ACS Broadband
# VD model
model_acs_vd2 <- lm(venturedensity_mean ~ pctbbfrac_ASU + IRR2010 + population + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                   data = tx_bb_entrepreneur_merged)

# HVD model
model_acs_hvd2 <- lm(highlyactive_vd_mean ~ pctbbfrac_ASU + IRR2010 + population + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                    data = tx_bb_entrepreneur_merged)

# Proprietor model
model_acs_pro2 <- lm(pct_proprietors_employment_2017 ~ pctbbfrac_ASU + IRR2010 + population + pcthighschool + pctcollege + pctmillennial + pctgenx + pctbabyboomer,
                    data = tx_bb_entrepreneur_merged)

## Generate Table
stargazer(model_acs_vd2, model_acs_hvd2, model_acs_pro2, type = "html", align = T,
          dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("ACS Broadband Subscription",
                               "Rurality Index (2010)",
                               "Population","Highschool","College",
                               "Millenials","GenX","Babyboomer"),
          omit.stat = c("ser"), no.space = T)
```

<table style="text-align:center">

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Venture Density

</td>

<td>

Highly Active VD

</td>

<td>

Proprietors Share

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

ACS Broadband Subscription

</td>

<td>

1.946

</td>

<td>

0.185

</td>

<td>

0.089

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.455)

</td>

<td>

(0.318)

</td>

<td>

(0.081)

</td>

</tr>

<tr>

<td style="text-align:left">

Rurality Index (2010)

</td>

<td>

\-5.989<sup>\*\*\*</sup>

</td>

<td>

\-1.542<sup>\*\*\*</sup>

</td>

<td>

0.510<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.589)

</td>

<td>

(0.346)

</td>

<td>

(0.088)

</td>

</tr>

<tr>

<td style="text-align:left">

Population

</td>

<td>

\-0.00000

</td>

<td>

\-0.00000

</td>

<td>

0.00000<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

<td>

(0.00000)

</td>

</tr>

<tr>

<td style="text-align:left">

Highschool

</td>

<td>

\-0.037<sup>\*\*</sup>

</td>

<td>

\-0.007<sup>\*</sup>

</td>

<td>

0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.017)

</td>

<td>

(0.004)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

College

</td>

<td>

0.162<sup>\*\*\*</sup>

</td>

<td>

0.039<sup>\*\*\*</sup>

</td>

<td>

0.001

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.016)

</td>

<td>

(0.003)

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

Millenials

</td>

<td>

0.020

</td>

<td>

0.003

</td>

<td>

\-0.004<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.033)

</td>

<td>

(0.007)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

GenX

</td>

<td>

0.183<sup>\*\*\*</sup>

</td>

<td>

0.028<sup>\*\*\*</sup>

</td>

<td>

0.004<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.038)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Babyboomer

</td>

<td>

0.139<sup>\*\*\*</sup>

</td>

<td>

0.036<sup>\*\*\*</sup>

</td>

<td>

0.010<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.036)

</td>

<td>

(0.008)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

\-3.541

</td>

<td>

\-0.408

</td>

<td>

\-0.324<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.176)

</td>

<td>

(0.477)

</td>

<td>

(0.122)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

230

</td>

<td>

232

</td>

<td>

232

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.624

</td>

<td>

0.647

</td>

<td>

0.577

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.610

</td>

<td>

0.634

</td>

<td>

0.562

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

45.758<sup>\*\*\*</sup> (df = 8; 221)

</td>

<td>

51.079<sup>\*\*\*</sup> (df = 8; 223)

</td>

<td>

38.085<sup>\*\*\*</sup> (df = 8; 223)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>
