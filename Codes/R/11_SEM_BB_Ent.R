#### Structural Equation Modelin of Broadband & Entrepreneurship ####

library(lavaan)

## IV: Broadband, Entrepreneurship
## Mediator: Entrepreneurship (potentially)
## DV: Prosperity
## Control variables: rurality, population, education, age

tx_bb_entrepreneur_merged_v2 <- tx_bb_entrepreneur_merged_v2 %>% 
  mutate(population_std = scale(population),
         prosperityindex2016 = scale(prosperityindex2016))

#### Empathy Structural Model ####
bb_ent_sem_model <- "
# Measurement model
  broadband =~ pct_broadband_FCC + pct_broadband_MS + pct_fixed_acs_2018 + pct_broadband_mlab
  entrepreneurship =~ pct_chg_bea_2012_2018 + venturedensity_mean + highlyactive_vd_mean
# Direct effect
  prosperityindex2016 ~ a*broadband + b*entrepreneurship + d1*IRR2010 + d2*population_std + d3*pcthighschool + 
  d4*pctcollege + d5*pctmillennial + d6*pctgenx + d7*pctbabyboomer
# Mediator
  entrepreneurship ~ c*broadband + d8*IRR2010 + d9*population_std + d10*pcthighschool + 
  d11*pctcollege + d12*pctmillennial + d13*pctgenx + d14*pctbabyboomer
# Indirect effect
  broad_x_entrepreneur := c * b
# Total effect
  total := a + (c * b)
"

set.seed(1234)

## Fit the structural model ##
# SE estimated by bootstrap sample of 5000

fit.bb_ent_sem_model <- sem(model = bb_ent_sem_model, data = tx_bb_entrepreneur_merged_v2)
summary(fit.bb_ent_sem_model, fit.measures = T, rsq = T, standardize = T)
semPaths(fit.emp.model)