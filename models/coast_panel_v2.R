library(tidyverse)
library(sf)
library(tigris)
library(tmap)
library(lfe)
library(corrplot)
library(naniar)
library(table1)


## load in datasets ----

coastal_panel <- read_csv("coastal_panel.csv", 
                          col_types = cols(X1 = col_skip()))
interview_data <- read_csv("interview_data.csv", 
                        col_types = cols(X1 = col_skip()))

coastal_panel <- coastal_panel %>%  mutate(HPI_annual_pct_change = as.numeric(HPI_annual_pct_change)) #rename("HPI_annual_pct_change"="Annual Change (%)")

coastal_panel <- coastal_panel %>% filter(GeoName != "Chautauqua County")

state_shp <- states(cb=T) %>% st_as_sf()
state_shp <- state_shp %>% mutate(STATEFP = as.numeric(STATEFP))

interview_shp <- interview_data %>% left_join(state_shp, by=c("state_fips"="STATEFP"))
interview_shp <- st_as_sf(interview_shp)

CRE <- read_csv("cre-2018-a11.csv")

CRE <- CRE %>% filter(geo_level == "County") %>% unite("fips",state:county,sep = "",remove=F,na.rm=T) %>% 
  mutate(fips = as.numeric(fips))
CRE <- CRE %>% select(-tract)
county_shp <- county_shp %>% mutate(STATEFP=as.numeric(STATEFP)) %>% mutate(COUNTYFP=as.numeric(COUNTYFP))

ACS <- read_csv("usa_00021.csv", 
                col_types = cols(COUNTYFIP = col_double()))
ACS <- ACS %>% filter(MIGRATE1 !=1) %>%  mutate(Migrants = PERWT*MIGRATE1)
ACS <- ACS %>% filter(COUNTYFIP != is.na(COUNTYFIP))
ACS <- ACS %>% mutate(STATEFIP=as.numeric(STATEFIP)) %>% mutate(COUNTYFIP=as.numeric(COUNTYFIP))
ACS <- ACS %>% left_join(county_shp,by=c("STATEFIP"="STATEFP","COUNTYFIP"="COUNTYFP"))
ACS <- ACS %>% select(YEAR,STATEFIP,COUNTYFIP,NAME,GEOID,Migrants,MIGRATE1,MIGRATE1D)
ACS <- ACS %>% mutate(Within_State = if_else(MIGRATE1==2,Migrants,0)) %>% mutate(Out_State = if_else(MIGRATE1==3,Migrants,0)) %>% 
  mutate(Abroad = if_else(MIGRATE1==4,Migrants,0))

ACS_group <- ACS %>% group_by(YEAR,GEOID) %>% summarise(Total_Migrants = sum(Migrants),
                                                        Within_State = sum(Within_State),
                                                        Out_State = sum(Out_State),
                                                        Abroad = sum(Abroad))
ACS_group <- ACS_group %>% ungroup() %>% group_by(GEOID) %>% summarise(Med_Total_Migrants = median(Total_Migrants),
                                                                       Med_Within_State = median(Within_State),
                                                                       Med_Out_State = median(Out_State),
                                                                       Med_Abroad = median(Abroad))


Housing_Inv <- read_csv("CO-EST2019-ANNHU.csv", 
                        skip = 0)

Housing_Inv[] <- lapply(Housing_Inv, gsub, pattern='\\.', replacement='')
Housing_Inv <- Housing_Inv %>% select(-Census,-`Estimates Base`) %>% 
   pivot_longer(c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                names_to = "year",values_to = "housing_count")

Housing_Inv <- Housing_Inv %>% separate(County,into=c("County","State"),sep = ", ")
Housing_Inv <- Housing_Inv %>% mutate(housing_count = as.numeric(housing_count))


#load in natural amenity, education, income, and race data
nat_amenity <- read_csv("natamenf_1_.csv", 
                        col_types = cols(FIPS_Code = col_double(), 
                                         `for measures` = col_skip()))

education <- read_csv("Education.csv", col_types = cols(FIPS = col_double()))
education <- education %>% pivot_longer(c("Percent_no_hs_diploma_2000","Percent_no_hs_2015-19"),names_to = "year",values_to = "percent_no_hs_diploma")
education <- education %>% pivot_longer(c("Percent_only_hs_diploma_2000","Percent_only_hs_diploma_2015-19"),
                                        names_to = "year2",values_to = "percent_only_hs_diploma")
education <- education %>% pivot_longer(c("Percent_some_college_2000","Percent_some_college_2015-19"),
                                        names_to = "year3",values_to = "percent_some_college_diploma")
education <- education %>% pivot_longer(c("Percent_bachelors_or_higher_2000","Percent_bachelors_or_higher_2015-19"),
                                        names_to = "year4",values_to = "percent_bachelors_or_higher")
education <- education %>% select(-year,-year2,-year3,-year4)
education <- education %>% group_by(FIPS,State) %>% summarize(percent_no_hs_diploma=median(percent_no_hs_diploma,na.rm = T),
                                                              percent_only_hs_diploma=median(percent_only_hs_diploma),
                                                              percent_some_college_diploma=median(percent_some_college_diploma),
                                                              percent_bachelors_or_higher=median(percent_bachelors_or_higher))


income <- read_csv("Unemployment.csv", col_types = cols(fips_txt = col_double()))
income <- income %>% separate("area_name",into=c("county_name","state"),sep = ", ") %>% select(-state)
income <- income %>% select(-county_name)

race <- read_csv("race_census2010.csv")
race2 <- read_csv("race_census2010_p2.csv")
hispanic <- race %>% select(-SEX,-IMPRACE) %>% pivot_wider(names_from = ORIGIN,values_from = RESPOP,values_fn = list(RESPOP= sum))
hispanic2 <- race2 %>% select(-IMPRACE) %>% pivot_wider(names_from = ORIGIN,values_from = RESPOP,values_fn = list(RESPOP= sum))

hispanic <- hispanic %>% rename("not_hispanic"="1") %>% rename("hispanic"="2") %>% select(-STNAME,-CTYNAME)
hispanic2 <- hispanic2 %>% rename("not_hispanic"="1") %>% rename("hispanic"="2") %>% select(-STNAME,-CTYNAME)

hispanic <- hispanic %>% rbind(hispanic2)

race <- race %>% select(-SEX,-ORIGIN) %>% pivot_wider(names_from = IMPRACE,values_from = RESPOP,values_fn = list(RESPOP= sum))
race2 <- race2 %>% select(-ORIGIN) %>% pivot_wider(names_from = IMPRACE,values_from = RESPOP,values_fn = list(RESPOP= sum))

race <- race %>% rename("white_only"="1") %>% rename("black_only"="2") %>% rename("native_american_only"="3") %>% 
  rename("asian_only"="4")
race2 <- race2 %>% rename("white_only"="1") %>% rename("black_only"="2") %>% rename("native_american_only"="3") %>% 
  rename("asian_only"="4")
race2 <- race2 %>% select(black_only:asian_only,everything())
race <- race %>% mutate(multiracial = select(.,"6":"31") %>% rowSums(na.rm = T))
race2 <- race2 %>% mutate(multiracial = select(.,"5":"31") %>% rowSums(na.rm = T))

race <- race %>% select(.,-"6":-"31")
race2 <- race2 %>% select(.,-"5":-"30")
race2 <- race2 %>% select(STATE,COUNTY,STNAME,CTYNAME,white_only,black_only,native_american_only,asian_only,multiracial)

race <- race %>% rbind(race2) %>% select(-STNAME,-CTYNAME)

July_temp <- read_csv("July_temp_avg.csv")
#load fema disaster declarations 
dis_dec <- read_csv("DisasterDeclarationsSummaries.csv", 
                    col_types = cols(fipsCountyCode = col_double(), 
                                     fipsStateCode = col_double()))

dis_dec <- dis_dec %>%
  filter(fyDeclared >= 2000) %>% 
  group_by(fyDeclared,fipsStateCode,fipsCountyCode) %>% 
  summarise(disasters_declared=n()) 

coastal_panel_test_2 <- coastal_panel_test %>% left_join(dis_dec,by=c("Year"="fyDeclared","state_fips"="fipsStateCode","COUNTYFP"="fipsCountyCode"))
coastal_panel_test_2 <- coastal_panel_test_2 %>% mutate(disasters_declared = if_else(is.na(.),0,disasters_declared))

coastal_panel_test_2$disasters_declared[is.na(coastal_panel_test_2$disasters_declared)] <- 0

write.csv(coastal_panel_test_2,"coastal_panel_6_30.csv")

#typology data 
typology <- TypologyData %>% filter(GEOID10 > 90091941001) %>%  separate(GEOID10,into = c("fips","tract"),sep=5)
typology_subset <- TypologyData %>% filter(GEOID10 <= 90091941001) %>%  separate(GEOID10,into = c("fips","tract"),sep=4) 

typology <- typology %>% filter(year == 2020| year==2100)
typology <- typology %>% select(-variable) %>%  pivot_wider(names_from = Type,values_from = count,values_fn = list(count=sum))
typology <- typology %>% select(-SSP,-SSP2,-prob2) %>% filter(prob == "p95") %>% 
  mutate(fips = as.numeric(fips))
typology <- typology %>% group_by(fips,year) %>% summarize(Inundated=sum(Inundated, na.rm = T),
                                                           FP_100_year=sum(`100-year FP`,na.rm = T))
typology <- typology %>% mutate(Inundated_2020 = if_else(year==2020, 1,0)) %>% 
  mutate(Inundated_2100 = if_else(year==2100,1,0)) %>% 
  mutate(FP_100_year_2020 = if_else(year==2020,1,0)) %>% 
  mutate(FP_100_year_2100 = if_else(year==2100,1,0))

typology <- typology %>% select(-year) %>%  mutate(Inundated_2020= Inundated*Inundated_2020) %>%
  mutate(Inundated_2100= Inundated*Inundated_2100) %>% 
  mutate(FP_100_year_2020= FP_100_year*FP_100_year_2020) %>% 
  mutate(FP_100_year_2100= FP_100_year*FP_100_year_2100) #%>% 
typology <- typology %>% ungroup() %>% group_by(fips) %>% 
  summarise(Inundated_2020 = sum(Inundated_2020, na.rm = T),
            Inundated_2100 = sum(Inundated_2100,na.rm = T),
            FP_100_year_2020 = sum(FP_100_year_2020,na.rm = T),
            FP_100_year_2100 = sum(FP_100_year_2100,na.rm = T))
          #  Inundated_pct_change = ((Inundated_2100-Inundated_2020)/Inundated_2020)*100,
           # FP_100_year_pct_change = ((FP_100_year_2100-FP_100_year_2020)/FP_100_year_2020)*100)
  #mutate(Inundated_pct_change = ((Inundated_2100-Inundated_2020)/Inundated_2020)*100) %>% 
  #mutate(FP_100_year_pct_change = ((FP_100_year_2100-FP_100_year_2020)/FP_100_year_2020)*100)
typology <- typology %>% ungroup() %>% select(-Inundated_pct_change,-FP_100_year_pct_change)

typology <- bind_rows(typology_subset,typology)


coastal_panel <- coastal_panel %>% select(-housing_count)
coastal_panel <- coastal_panel %>% left_join(Housing_Inv,by=c("GeoName"="County","NAME.x"="State","Year"="year"))
coastal_panel <- coastal_panel %>% left_join(nat_amenity,by=c("fips.x"="FIPS_Code","StateAbbrev"="STATE"))
coastal_panel <- coastal_panel %>% left_join(income, by=c("fips.x"="fips_txt","StateAbbrev"="Stabr"))
coastal_panel <- coastal_panel %>% left_join(education, by=c("fips.x"="FIPS","StateAbbrev"="State"))
coastal_panel <- coastal_panel %>% left_join(race, by=c("state_fips"="STATE","COUNTYFP"="COUNTY"))
coastal_panel <- coastal_panel %>% left_join(hispanic, by=c("state_fips"="STATE","COUNTYFP"="COUNTY"))
coastal_panel <- coastal_panel %>% left_join(July_temp, by=c("fips.x"="Fips"))

coastal_panel <- coastal_panel %>% select(-STNAME:-hispanic)

coastal_panel_test <- coastal_panel %>% left_join(typology,by=c("fips.x"="fips"))
coastal_panel <- coastal_panel_test
coastal_panel_test <- coastal_panel
# summarize panel data to make cross-section 

coastal_cs <- coastal_panel %>% ungroup() %>%  group_by(GeoName,StateAbbrev) %>% 
  summarise(HPI_med = median(HPI,na.rm = T),
            HPI_med_annual_change = median(HPI_annual_pct_change,na.rm=T),
            Med_house_count = median(housing_count,na.rm = T),
            fips = last(fips.x),
            grant_amount=sum(grant_amount),
            number_of_properties=sum(number_of_properties),
            number_of_grants=sum(number_of_grants),
            protection_grant=sum(protection_grant),
            accommodation_grant=sum(accommodation_grant),
            relocation_grant=sum(relocation_grant),
            planning_grant=sum(planning_grant),
            protection_amount=sum(protection_amount),
            accommodation_amount=sum(accommodation_amount),
            relocation_amount=sum(relocation_amount),
            planning_amount=sum(planning_amount),
            unemployment_rate=max(unemployment_rate),
            population=median(population),
            Schools=last(Schools),
            LawEnforcement=last(LawEnforcement),
            FireEMS=last(FireEMS),
            HospitalMedical=last(HospitalMedical),
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
            native_american_only=last(native_american_only),
            multiracial = last(multiracial),
            hispanic = last(hispanic),
            Inundated_2020 = last(Inundated_2020),
            Inundated_2100 = last(Inundated_2100),
            FP100_2020 = last(FP_100_year_2020),
            FP100_2100 = last(FP_100_year_2100),
            July_avg_temp_19 = last(July_2019_Mean_Temp),
            July_1901_2000_avg = last(`1901-2000 Mean`))

coastal_cs <- coastal_cs %>% left_join(interview_data,by=c("StateAbbrev"="State"))
coastal_cs <- coastal_cs %>% left_join(CRE,by=c("StateAbbrev"="stabrev","GeoName"="ctname"))

coastal_19 <- coastal_19 %>% left_join(interview_data,by=c("StateAbbrev"="State"))

coastal_cs <- coastal_cs %>% mutate(grants_pc = (number_of_grants/population))
coastal_cs <- coastal_cs %>% mutate(grant_money_pc = (grant_amount/population))
#coastal_cs <- coastal_cs %>% mutate(income_pc = (household_income/population))
coastal_cs <- coastal_cs %>% mutate(South = if_else(StateAbbrev =="TX"|StateAbbrev=="LA"|StateAbbrev=="AL"|
                                                     StateAbbrev=="MS"|StateAbbrev=="FL"|StateAbbrev=="GA",1,0))
coastal_cs <- coastal_cs %>% mutate(Northeast = if_else(StateAbbrev =="NJ"|StateAbbrev=="NY"|StateAbbrev=="CT"|
                                                      StateAbbrev=="RI"|StateAbbrev=="MA"|StateAbbrev=="NH"|
                                                      StateAbbrev=="ME",1,0))
coastal_cs <- coastal_cs %>% mutate(West = if_else(StateAbbrev =="CA"|StateAbbrev=="OR"|StateAbbrev=="WA"|
                                                          StateAbbrev=="RI"|StateAbbrev=="MA"|StateAbbrev=="NH"|
                                                          StateAbbrev=="ME",1,0))

coastal_cs <- coastal_cs %>% mutate(Mid_atlantic = if_else(StateAbbrev =="SC"|StateAbbrev=="NC"|StateAbbrev=="VA"|
                                                     StateAbbrev=="MD",1,0))

coastal_cs <-coastal_cs %>% mutate(pct_black = (black_only/population)*100) %>% mutate(pct_hispanic = (hispanic/population)*100) %>% 
  mutate(pct_white = (white_only/population)*100) %>% mutate(pct_native_american=(native_american_only/population)*100) %>% 
  mutate(pct_asian = (asian_only/population)*100)

coastal_cs <- coastal_cs %>% left_join(ACS_group,by=c("fips.x"="GEOID"))

coastal_shp <- coastal_shp %>% mutate(emergency_services = LawEnforcement+FireEMS+HospitalMedical) %>% 
      mutate(ALAND = as.numeric(ALAND))  

coastal_shp <- coastal_shp  %>%  mutate(pop_density = (population/(ALAND/1000000)))

coastal_cs <- coastal_cs %>% mutate(FP100_change_rate = ((abs(FP100_2020-FP100_2100))/80))
?tigris::counties

coastal_cs <- coastal_cs %>% mutate(July_avg_med = median(c(July_1901_2000_avg,July_avg_temp_19)))

#coastal_cs <- coastal_cs %>% mutate_at(vars(Med_Total_Migrants:Med_Abroad), ~replace(., is.na(.), 0.1)) %>% select(-pop_density)

gg_miss_var(coastal_cs)
# plots and maps ----
tm_shape(state_shp, projection=proj) + tm_polygons(col="black",alpha = 0.8,border.col = "white",border.alpha = 0.4) +
tm_shape(interview_shp,is.master = TRUE, projection=proj) + 
  tm_polygons("CAI",title="",border.col = "black",border.alpha = 0.2,style="cat",palette="-YlGnBu",
              labels=c("Developing","Intermediate","Advanced")) +
  tm_layout(frame = FALSE,fontfamily = "Times", main.title = "Coastal Adaptation Index Stage") + tm_legend()

class(coastal_shp)
coastal_shp <- coastal_shp %>% ungroup()
coastal_shp <- st_as_sf(coastal_shp)
coastal_shp <- coastal_shp %>% mutate(grants_pc = (number_of_grants/population))
coastal_shp <- coastal_shp %>% mutate(grants_money_pc = (grant_amount/population))
coastal_shp <- coastal_shp %>% mutate(FP100_change_pd = (FP100_change_rate/pop_density))


tm_shape(state_shp, projection=proj) + tm_polygons(col="black",alpha = 0.8,border.col = "white",border.alpha = 0.4) +
  tm_shape(coastal_shp,is.master = TRUE, projection=proj) + 
  tm_polygons("HPI_med",title="Median HPI",border.col = NULL,border.alpha = 0.8,palette="reds") +
  tm_layout(frame = FALSE,fontfamily = "Times", main.title = "Median Housing Price Index 2000-2019") + tm_legend()


png("exposure_v2.png",pointsize = 11,height = 900,width = 1150,res = 150)
dev.off()

proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
coastal_shp$
tm_shape(state_shp, projection = proj) + tm_polygons(col="lightgrey",alpha = 0.9,border.col = "white",border.alpha = 0.5) +
  tm_shape(coastal_shp,is.master = TRUE, projection = proj) + 
  tm_bubbles(title.size="Dollars Per Person",size="grants_pc",col="blue",border.col="blue",border.alpha=0.75,
             alpha=0.45,scale=3) +
  tm_layout(frame = FALSE,main.title = "Hazard Mitigation Grant Funding Per Capita 2000-2019",fontfamily = "Times") +
  tm_legend(text.size=0.85)

tm_shape(state_shp, projection = proj) + tm_polygons(col="lightgrey",alpha = 0.9,border.col = "white",border.alpha = 0.5) +
  tm_shape(coastal_shp,is.master = TRUE, projection = proj) + 
  tm_bubbles(title.size="Annual Population Increase \n (Normalized by Population Density)",size="FP100_change_pd",col="red",border.col="red",border.alpha=0.75,
             alpha=0.45,scale=2.3) +
  tm_layout(frame = FALSE,main.title = "Projected Increase in 100-Year Floodplain Inhabitants 2020-2100",fontfamily = "Times") +
  tm_legend(text.size=0.85)
  #tm_legend(text.size=0.6)#+ tm_legend() #+tm_compass()+
 # tm_scale_bar(breaks = c(0,500,1000),position = c("left","bottom")) tm_legend(title="Dollars Per Person",text.size=0.75) +
?tm_bubbles
#tm_polygons("grant_money_pc",title="Dollars Per Person",border.col = "white",border.alpha = 0.5,
 #           palette = "BuGn",style="quantile",n=5) +
?tm_scale_bar
qtm(coastal_shp$geometry)
corplot(cor_cs,graphType="full")
heatmap(cor_cs,symm = TRUE)
?geom_col
cor_cs <- cor(coastal_cs[,c(3:22,36:39)])
ggplot(coastal_cs,aes(x=unemployment_rate, (y=log10(coastal_cs$HPI_med)))) + geom_point() +geom_smooth(method=lm) #+theme_bw(base_family = "Times")+
  #ggtitle("Number of Hazard Mitigation Grants 2000-2019") + xlab("Year") +ylab("Number of Grants")
 # ggtitle("Log Claims Per County Population Vs. Log Out Migration")  
ggplot(coastal_cs,aes(HPI_pct_change_med)) + geom_boxplot()
cor(coastal_cs$Schools,coastal_cs$HPI_pct_change_med,)
summary(coastal_cs$HPI_pct_change_med)

ggplot(coastal_cs, aes(x=HPI_med_annual_change)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

# summary statistics table 
coastal_cs2 <- coastal_cs
coastal_cs2 <- arrange(coastal_cs2,Census_Division)
?arrange
coastal_cs2$CAI <- 
  factor(coastal_cs2$CAI, levels=c(1,2,3),
         labels=c("Developing", 
                  "Intermediate",
                  "Advanced"))

coastal_cs2$Census_Division <- 
  factor(coastal_cs2$Census_Division, levels=c(1,2,5,6,7,9),
         labels=c("New England", 
                  "Middle Atlantic",
                  "South Atlantic",
                  "East South Central",
                  "West South Central",
                  "Pacific"))

label(coastal_cs2$HPI_med)       <- "Median Housing Price Index"
label(coastal_cs2$CAI)       <- "Coastal Adaptation Index"
label(coastal_cs2$protection_grant)       <- "Protection Grant"
label(coastal_cs2$accommodation_grant)     <- "Accommodation Grant"
label(coastal_cs2$relocation_grant) <- "Relocation Grant"
label(coastal_cs2$planning_grant) <- "Planning Grant"
label(coastal_cs2$Mean_rel_humidity) <- "Mean Relative Humidity"
label(coastal_cs2$pct_bachelors_or_higher) <- "Percent Bachelors or Higher"
label(coastal_cs2$pct_white) <- "Percent White"
label(coastal_cs2$FP100_change_rate) <- "100-Year Floodplain Change Rate"
label(coastal_cs2$population) <- "Population Estimate"
label(coastal_cs2$household_income) <- "Household Income"
label(coastal_cs2$Census_Division) <- "Census Division"


units(coastal_shp$age)       <- "years"
units(coastal_shp$thickness) <- "mm"

table1(~ HPI_med + CAI + protection_grant + accommodation_grant + relocation_grant +
         planning_grant + Mean_rel_humidity + pct_bachelors_or_higher + 
         pct_white + FP100_change_rate + population + household_income +
         Census_Division, data=coastal_cs2,topclass="Rtable1-times")


# models -----
library(relaimpo)
library(earth)
library(caret)
library(randomForest)
library(varImp)

coastal_shp <- coastal_shp %>% mutate_all(~replace(., is.na(.), 0))

rf <- randomForest(log(HPI_med)~CAI+protection_grant+accommodation_grant+relocation_grant+planning_grant+
            Mean_rel_humidity+pct_hs_only+pct_white+pct_hispanic+
            FP100_change_rate+log(population)+log(household_income)+Census_Division, data = coastal_cs,importance=T,na.action = na.omit)
car::vif(ols)
gvlma::gvlma(ols)

relImport <- calc.relimp(ols,type = "lmg",rela=T)
sort(relImport$lmg,decreasing = T)
?evimp
ev <- evimp(reg)
plot(ev)
print(ev)

ols <- felm(log(HPI_med)~CAI+protection_grant+accommodation_grant+relocation_grant+planning_grant+
              pct_hs_only+pct_hispanic+July_avg_med+amenity_rank+
              FP100_change_rate+log(population)+log(household_income)+Census_Division|0|0|0, data=coastal_cs)
summary(ols)
plot(ols,5)

fe_county <- felm(log(HPI_med)~CAI+protection_grant+accommodation_grant+relocation_grant+planning_grant+
                    July_avg_med+pct_hs_only+pct_white+pct_hispanic+
                    FP100_change_rate+log(population)+log(household_income)|South|0|0, data=coastal_shp,na.action = NULL)

glm <- glm(log(HPI_med)~Schools+log(Med_house_count)+
             protection_grant+accommodation_grant+relocation_grant+planning_grant+
             unemployment_rate+CAI, data=coastal_shp)

gstsls <- gstsls(log(HPI_med)~LawEnforcement+FireEMS+HospitalMedical+log(population)+
                   protection_grant+accommodation_grant+relocation_grant+planning_grant+
                   RPS+Years_since_first_state_climate_report+CAI,data=coastal_cs,listw = coastal_listw,
                 zero.policy = T,robust=T)

sac <- spatialreg::lagsarlm(log(HPI_med)~CAI+unemployment_rate+Schools+LawEnforcement+
                              protection_grant+accommodation_grant+relocation_grant+planning_grant+
                              FireEMS+HospitalMedical+RPS+log(Med_house_count)+West+South,data = coastal_cs,
                            listw=coastal_listw,zero.policy = T, Durbin = ~log(Med_house_count)+Schools+LawEnforcement+
                              FireEMS+HospitalMedical)
?sacsarlm
length(lag$residuals)
summary(fe_county)
# check spatial dependence 
library(spdep)
library(spatialreg)
library(spmoran)
library(sf)
county_shp <- county_shp %>% mutate(GEOID = as.numeric(GEOID))
county_shp <- st_as_sf(county_shp)
coastal_shp <- coastal_cs %>% left_join(county_shp,by=c("fips"="GEOID"))
coastal_shp <- st_as_sf(coastal_shp)

coastal_sp <- as(coastal_shp,Class="Spatial")
coastal_nb <- poly2nb(coastal_sp,queen=T)
coastal_listw <- nb2listw(coastal_nb,style = "W",zero.policy = T)
?nb2listw
y <- log(coastal_cs$HPI_med)
x <- coastal_shp[,c("CAI","relocation_grant")]
x <- coastal_shp[,c("CAI","protection_grant","accommodation_grant",
                     "relocation_grant","planning_grant","Mean_rel_humidity","population","amenity_scale",
                     "pct_white","pct_hispanic","pct_hs_only","household_income","FP100_change_rate")]
x<- x %>%st_drop_geometry() %>% mutate(household_income =log(household_income)) %>%   mutate(population =log(population)) #%>% 
  #mutate(emergency_services = log(emergency_services))
x <- x %>% mutate(amenity_rank == if_else(is.na(amenity_rank),3,amenity_rank))
x <- x %>% mutate_all(~replace(., is.na(.), 0)) # %>% mutate(FP100_2100 = if_else(FP100_2100 !=0,log(FP100_2100),0))
#x <- x %>% filter(predrt_3 == if_else(is.na(predrt_3),0,predrt_3))
coords <- coordinates(coastal_sp)
meig <- meigen(coords=coords,model = "exp")
weig1 <- weigen(coords)

resf <- resf(y=y,x=x,meig = meig)
resf
?lslm
lag <- lslm(y=y,x=x,weig = weig1,method = "reml")
lag
coastal_shp$pred_val <- lag$pred

error <- lsem(y=y,x=x,weig = weig1,method = "ml")
error
length(error$resid)
?weigen
lm_lag <-lm(log10(HPI_med)~CAI+protection_grant+accommodation_grant+relocation_grant+planning_grant+
              Mean_rel_humidity+pct_bachelors_or_higher+pct_white+
              FP100_change_rate+log(population)+log(household_income)+Census_Division, data = coastal_shp)
AIC(fe_county)
moran.mc(error$resid,listw = coastal_listw,nsim = 99,zero.policy = T)
olslgm <-lm.LMtests(ols,coastal_listw,test = "all",zero.policy = T)
print(lgm)

# report regression results -----
library(stargazer)
library(dotwhisker)

multiply.100 <- function(x) (x*100)


stargazer(ols,fe_county, lm_lag,type = "latex",
          align = FALSE,no.space = TRUE,style = "aer", digits = 5,
          title = "Modeling Housing Price Index with Coastal Adaptation Indicators",
          #column.labels = c("Non-Spatial Weights","Spatial Weights"),
        #  column.separate = c(2,1),
          dep.var.labels = "Log(Median Housing Price Index)",
          model.names = FALSE, omit.stat = c("ser","f"),
          apply.coef = multiply.100, apply.se = multiply.100,
          notes = "Coefficients and Standard Errors multiplied by 100",
          font.size = "footnotesize",
          add.lines = list(c("Region FE","-","Yes","-"),
                           c("Moran's I","0.26","0.21","0.02")),
                           #c("CV Avg. $R2$","0.52","0.57","0.44","0.50","-","-","-"),
                           #c("Avg.Train RMSE","0.46","0.43","0.43","0.40","-","-","-"),
                           #c("Avg. Test RMSE","0.45","0.42","0.42","0.39","-","-","-"),
                           #c("CV Moran's I","0.30*","0.30*","0.21*","0.20*","-","-","-")),
          out = "HPI_CAI_table6_new_data.tex", 
          notes.append = T, notes.align = "r")

stargazer(lag,type="html",title = "Spatial Regression Output (lag)",
          omit.stat = c("rsq","f","ser","adj.rsq"),
          apply.coef = multiply.100, apply.se = multiply.100,
          dep.var.labels = "Median HPI",
          out = "lag.htm")

dwplot(list(ols,glm,fe_county)) + 
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Modeling HPI with Coastal Adaptation") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.80, 0.65),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank())
