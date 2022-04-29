library(tidyverse)
library(lfe)

setwd("~/Projects/coastal_adaptation_model/")

regdat = read_csv("data/coastal_regdat.csv")

# Table 7 Replication -- Modeling Housing Price Index with Coastal Adaptation Indicators
lm_lag = lm(log(HPI_med) ~ CAI + protection_grant + accommodation_grant + relocation_grant + planning_grant + 
              FP100_change_rate + Mean_rel_humidity + pct_hs_only + pct_white + pct_hispanic + log(population) + 
              log(household_income) + Census_Division, data = regdat)

summary(lm_lag)


mod = felm( log(HPI_med) ~ CAI + protection_grant + accommodation_grant + relocation_grant + planning_grant + 
                        FP100_change_rate + Mean_rel_humidity + pct_hs_only + pct_white + pct_hispanic + 
                        log(population) + log(household_income) + 
                        + unemployment_rate | region, data = regdat)

summary(mod)

