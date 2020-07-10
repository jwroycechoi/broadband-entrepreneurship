#### Regression Models Exploration ####
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(stargazer)
library(lm.beta)

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
stargazer(model_fcc_vd, model_fcc_hvd, model_fcc_pro, title = "Regression Results",
          align = TRUE, dep.var.labels = c("Venture Density","Highly Active VD", "Proprietors Share"),
          covariate.labels = c("FCC Broadband Availability","Rurality Index (2010)","Population","Black","Native American",
                               "Asian","Hispanic","Agriculture","Construction","Wholesale","Retail","Transportation","IT",
                               "Finance","Professional","Education","Other occupation","Public","Highschool","College",
                               "Millenials","GenX","Babyboomer","Foreign Born","Median Income (2017)"),
          omit.stat = c("ser"), no.space = TRUE)

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

summary(model_fcc_vd)

## Fewer Control Variables: Rurality, population, education level, age cohorts
## FCC Broadband
# VD model
model_fcc_vd2 <- lm(venturedensity_mean ~ pct_broadband_FCC + IRR2010 + population + pcthighschool + pctcollege +
                      pctmillennial + pctgenx + pctbabyboomer,
                    data = tx_bb_entrepreneur_merged)

summary(model_fcc_vd2)

# HVD model
model_fcc_hvd2 <- lm(highlyactive_vd_mean ~ pct_broadband_FCC + IRR2010 + population + pcthighschool + pctcollege +
                      pctmillennial + pctgenx + pctbabyboomer,
                    data = tx_bb_entrepreneur_merged)

summary(model_fcc_hvd2)

# Proprietor model
model_fcc_pro2 <- lm(pct_proprietors_employment_2017 ~ pct_broadband_FCC + IRR2010 + population + pcthighschool + pctcollege +
                       pctmillennial + pctgenx + pctbabyboomer,
                     data = tx_bb_entrepreneur_merged)

summary(model_fcc_pro2)

## MS Broadband
# VD model
model_ms_vd2 <- lm(venturedensity_mean ~ pct_broadband_MS + IRR2010 + population + pcthighschool + pctcollege +
                     pctmillennial + pctgenx + pctbabyboomer,
                   data = tx_bb_entrepreneur_merged)

summary(model_ms_vd2)

# HVD model
model_ms_hvd2 <- lm(highlyactive_vd_mean ~ pct_broadband_MS + IRR2010 + population + pcthighschool + pctcollege +
                     pctmillennial + pctgenx + pctbabyboomer,
                   data = tx_bb_entrepreneur_merged)

summary(model_ms_hvd2)

# Proprietor model
model_ms_pro2 <- lm(pct_proprietors_employment_2017 ~ pct_broadband_MS + IRR2010 + population + pcthighschool + pctcollege +
                     pctmillennial + pctgenx + pctbabyboomer,
                   data = tx_bb_entrepreneur_merged)

summary(model_ms_pro2)

## ACS Broadband
# VD model
model_acs_vd2 <- lm(venturedensity_mean ~ pctbbfrac_ASU + IRR2010 + population + pcthighschool + pctcollege +
                     pctmillennial + pctgenx + pctbabyboomer,
                   data = tx_bb_entrepreneur_merged)

summary(model_acs_vd2)

# HVD model
model_acs_hvd2 <- lm(highlyactive_vd_mean ~ pctbbfrac_ASU + IRR2010 + population + pcthighschool + pctcollege +
                      pctmillennial + pctgenx + pctbabyboomer,
                    data = tx_bb_entrepreneur_merged)

summary(model_acs_hvd2)

# Proprietor model
model_acs_pro2 <- lm(pct_proprietors_employment_2017 ~ pctbbfrac_ASU + IRR2010 + population + pcthighschool + pctcollege +
                      pctmillennial + pctgenx + pctbabyboomer,
                    data = tx_bb_entrepreneur_merged)

summary(model_acs_pro2)

#### Proprietor Share Change as DV ####
## Change in the proprietor's share might be a better measure of entrepreneurship
tx_bb_entrepreneur_merged$pct_change_pro_emp_2010_2017
model_fcc_prochg <- lm(pct_change_pro_emp_2010_2017 ~ pct_broadband_FCC + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                         pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                         pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                         pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
                       data = tx_bb_entrepreneur_merged)
summary(model_fcc_prochg)


model_ms_prochg <- lm(pct_change_pro_emp_2010_2017 ~ pct_broadband_MS + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                        pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                        pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                        pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
                      data = tx_bb_entrepreneur_merged)
summary(model_ms_prochg)

model_acs_prochg <- lm(pct_change_pro_emp_2010_2017 ~ pctbbfrac_ASU + IRR2010 + population + pctblack + pctnative + pctasian + pcthispanic +
                         pctagriculture + pctconstruction + pctwholesale + pctretail + pcttransportation + pctinformation_tech + pctfinance +
                         pctprofessional + pcteducation + pctother_occupation + pctpublic + pcthighschool + pctcollege +
                         pctmillennial + pctgenx + pctbabyboomer + pctforeignborn + medinc2017,
                       data = tx_bb_entrepreneur_merged)

summary(model_acs_prochg)

model_fcc_prochg2 <- lm(pct_change_pro_emp_2010_2017 ~ pct_broadband_FCC + IRR2010 + population + pcthighschool + pctcollege +
                          pctmillennial + pctgenx + pctbabyboomer,
                        data = tx_bb_entrepreneur_merged)

model_ms_prochg2 <- lm(pct_change_pro_emp_2010_2017 ~ pct_broadband_MS + IRR2010 + population + pcthighschool + pctcollege +
                          pctmillennial + pctgenx + pctbabyboomer,
                        data = tx_bb_entrepreneur_merged)

model_acs_prochg2 <- lm(pct_change_pro_emp_2010_2017 ~ pctbbfrac_ASU + IRR2010 + population + pcthighschool + pctcollege +
                          pctmillennial + pctgenx + pctbabyboomer,
                        data = tx_bb_entrepreneur_merged)

summary(model_fcc_prochg2)
summary(model_ms_prochg2)
summary(model_acs_prochg2)  
  
  
  
  