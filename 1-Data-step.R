library(tidyverse)

## load in datasets ----

setwd("~/Projects/coastal_adaptation_model/")

coastal_panel = as.data.frame(read_csv("data/coastal_panel_6_30.csv"))

interview_data = as.data.frame(read_csv("data/interview_data_6_30.csv"))

coastal_cs <- coastal_panel %>% 
  group_by(GeoName, StateAbbrev) %>% 
  summarise(HPI_med = median(HPI, na.rm = TRUE),
            HPI_med_annual_change = median(HPI_annual_pct_change, na.rm=TRUE),
            Med_house_count = median(housing_count, na.rm = TRUE),
            grant_amount = mean(grant_amount, na.rm = TRUE),
            number_of_properties = mean(number_of_properties, na.rm = TRUE),
            number_of_grants = mean(number_of_grants, na.rm = TRUE),
            protection_grant = mean(protection_grant, na.rm = TRUE),
            accommodation_grant = mean(accommodation_grant, na.rm = TRUE),
            relocation_grant = mean(relocation_grant, na.rm = TRUE),
            planning_grant = mean(planning_grant, na.rm = TRUE),
            protection_amount = mean(protection_amount, na.rm = TRUE),
            accommodation_amount = mean(accommodation_amount, na.rm = TRUE),
            relocation_amount = mean(relocation_amount, na.rm = TRUE),
            planning_amount = mean(planning_amount, na.rm = TRUE),
            unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
            population = median(population, na.rm = TRUE),
            fips = last(fips.x),
            Schools = last(Schools),
            LawEnforcement = last(LawEnforcement),
            FireEMS = last(FireEMS),
            HospitalMedical = last(HospitalMedical),
            Census_Division = last(Census_Division),
            amenity_scale = last(nat_amenity_scale),
            amenity_rank = last(nat_amenity_rank),
            household_income = last(Median_Household_Income_2019),
            hh_income_pct_state_total = last(Med_HH_Income_Percent_of_State_Total_2019),
            Mean_Jan_Temp = last(`mean_temp_January_1941-70`),
            Mean_Jul_Temp = last(`mean_temp_July_1941-70`),
            Mean_rel_humidity = last(`mean_rel_humidity_July_1941-70`),
            pct_no_hs = last(percent_no_hs_diploma),
            pct_hs_only = last(percent_only_hs_diploma),
            pct_some_college = last(percent_some_college_diploma),
            pct_bachelors_or_higher = last(percent_bachelors_or_higher),
            white_only = last(white_only),
            black_only = last(black_only),
            asian_only = last(asian_only),
            native_american_only = last(native_american_only),
            multiracial = last(multiracial),
            hispanic = last(hispanic),
            Inundated_2020 = last(Inundated_2020),
            Inundated_2100 = last(Inundated_2100),
            FP100_2020 = last(FP_100_year_2020),
            FP100_2100 = last(FP_100_year_2100),
            July_avg_temp_19 = last(July_2019_Mean_Temp),
            July_1901_2000_avg = last(`1901-2000 Mean`))


coastal_cs <- coastal_cs %>% left_join(interview_data, by=c("StateAbbrev"="State"))

coastal_cs <- coastal_cs %>% mutate(grants_pc = (number_of_grants / population),
                                    grant_money_pc = (grant_amount / population),
                                    pct_black = (black_only / population)*100,
                                    pct_hispanic = (hispanic / population)*100,
                                    pct_white = (white_only / population)*100,
                                    pct_native_american = (native_american_only / population)*100,
                                    pct_asian = (asian_only / population)*100,
                                    FP100_change_rate = ((abs(FP100_2020 - FP100_2100)) / 80),
                                    July_avg_med = median(c(July_1901_2000_avg, July_avg_temp_19)))

# Setup regions
coastal_cs$region = "NA"
coastal_cs$region = if_else(coastal_cs$StateAbbrev == "TX" | 
                            coastal_cs$StateAbbrev == "LA" | 
                            coastal_cs$StateAbbrev == "AL" |
                            coastal_cs$StateAbbrev == "MS" | 
                            coastal_cs$StateAbbrev == "FL" | 
                            coastal_cs$StateAbbrev == "GA", "South", coastal_cs$region)

coastal_cs$region = if_else(coastal_cs$StateAbbrev == "NJ" | 
                            coastal_cs$StateAbbrev == "NY" | 
                            coastal_cs$StateAbbrev == "CT" |
                            coastal_cs$StateAbbrev == "RI" | 
                            coastal_cs$StateAbbrev == "MA" | 
                            coastal_cs$StateAbbrev == "NH" |
                            coastal_cs$StateAbbrev == "ME" |
                            coastal_cs$StateAbbrev == "DE", "Northeast", coastal_cs$region)

coastal_cs$region = if_else(coastal_cs$StateAbbrev == "CA" | 
                            coastal_cs$StateAbbrev == "OR" | 
                            coastal_cs$StateAbbrev == "WA", "West", coastal_cs$region)

coastal_cs$region = if_else(coastal_cs$StateAbbrev == "SC" | 
                            coastal_cs$StateAbbrev == "NC" | 
                            coastal_cs$StateAbbrev == "VA" |
                            coastal_cs$StateAbbrev == "MD", "East", coastal_cs$region)



write_csv(coastal_cs, "data/coastal_regdat.csv")
