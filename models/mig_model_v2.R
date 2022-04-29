#migration model development version 2 

#load relevant packages
library(tidyverse)
library(lfe)
library(splm)
library(sf)
library(rgdal)
library(spatialreg)
library(sp)
library(car)
library(multiwayvcov)
library(gmm)
library(spanel)
library(stargazer)
library(miceadds)
library(estimatr)
library(modelsummary)
library(broom.mixed)
library(dotwhisker)
library(mgcv)
library(ncf)
library(tmap)
library(caret)

# upload new data ----
nat_amenity <- read_csv("~/Desktop/Thesis/Coastal_Zone/natamenf_1_.csv", 
                        col_types = cols(FIPS_Code = col_double()))

Education <- read_csv("~/Desktop/Thesis/Coastal_Zone/Education.csv", 
                      col_types = cols(FIPS = col_double()))

mig_data_v2 <- mig_data_v2 %>% left_join(nat_amenity,by=c("fips"="FIPS_Code"))
mig_clm_shp <- mig_clm_shp %>% left_join(nat_amenity,by=c("fips"="FIPS_Code"))

mig_clm_shp <- mig_clm_shp %>% mutate(coastal = if_else(state_name == "Michigan"|
                                                          state_name== "Minnesota"|
                                                          state_name=="Wisconsin"|
                                                          state_name=="Illinois"|
                                                          state_name=="Ohio"|
                                                          state_name=="Indiana"|
                                                          state_name=="Pennsylvania",0,coastal)) %>% 
  mutate(coastal = if_else(fips == 36029|fips==36045|fips==36055|fips==36063|fips==36073|
                             fips==36075|fips==36117,0,coastal))

mig_claims <- read_csv("~/Desktop/Thesis/IRS_mig_data/mig_claims_4_22.csv")

knitr::knit("test.Rmd")
?knit
# data plots and maps ----
mig_clm_shp <- mig_clm_shp %>% mutate(disaster = (flood_disaster + other_disaster))
ggplot(mig_dat_v2, aes(log10(total_claims),log10(total_claim_pay))) + geom_point()
ggplot(mig_clm_shp,aes(disaster,out_flow_pc)) + geom_point() + geom_smooth(method=lm)
ggplot(mig_dat_v2,aes(x=(year^3),y=log10(out_flow))) + geom_jitter() 

lm_year<-lm(log10(out_flow)~(year)+as.integer(year^3),data = mig_data_v2)
summary(lm_year)
mig_clm_shp %>% 
  summarize(out = sum(out_flow)) %>% ggplot(aes(x=(year),y=out)) + geom_point()

proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

tm_shape(county_shp, projection = proj) + tm_polygons(col="lightgrey",alpha = 0.9,border.col = "white",border.alpha = 0.5) +
  tm_shape(mig_fil,is.master = TRUE, projection = proj) + 
  tm_polygons("disasters_pc",style="quantile", border.col=NULL,
              alpha=0.8,palette="PuRd",title="FEMA Disasters Declared") +
  tm_layout(frame = FALSE)
?tm_bubbles
png("study_area.png",pointsize = 11,height = 900,width = 1150,res = 150)
dev.off()

tm_shape(mig_clm_shp,is.master = T) + tm_polygons(col = "predicted")
qtm(mig_clm_shp,"predicted")
tm_shape(mig_fil) + tm_polygons("predicted_values",border.col = NULL,style="fixed",
                                breaks=c(-3,1,2,3,7),
                                palette="YlOrRd") + tm_layout(frame=F)

tm_shape(mig_fil,is.master = T) + tm_bubbles("predicted_values",col="violet",
                                             alpha=0.3,style="equal",border.col="blue") +
 tm_layout(frame=F) +
  tm_shape(county_shp) +tm_polygons("grey60",alpha=0.1,border.col = NULL)

mig_clm_sp@data$predicted <- gstsls.mig$fitted.values
mig_clm_shp$predicted <- gstsls.mig$fitted.values

mig_data_v2$predicted <- spatialreg::predict.SLX(gstsls.mig)
mig_data_v2 <- mig_data_v2 %>% mutate(disasters= flood_disaster+other_disaster) %>% 
  mutate(disasters_pc = disasters/population)
mig_fil <- mig_data_v2 %>% ungroup() %>% group_by(fips) %>% 
  summarise(disasters = sum(disasters),
            population = last(population))
mig_fil <- mig_fil %>%  left_join(county_shp, by=c("fips"="GEOID"))
mig_fil <- st_as_sf(mig_fil)
mig_clm_shp <- st_as_sf(mig_clm_shp)

mig_fil <- mig_fil %>% mutate(disasters_pc = disasters/population)

summary(lm_year)
?ts
ts <- as.ts(mig_dat_v2,start = c(1990),end=c(2010),frequency = 1)
plot.ts(ts[8])
ts.mig <- mig_dat_v2[,5,8]
trend<-lm(log10(out_flow)~as.factor(year),data = ts)
plot(resid(trend),type="l")
sp_cor <-sp.correlogram(mig.nb,var = mig_clm_sp$out_flow,order = 5,method ="corr",zero.policy = T)
sp_cor
# base model specifications -----
form1 <- as.formula(log10(out_flow)~flood_disaster+other_disaster+total_claims+
  coastal+HPI+Unemployment_Rate+Jan_tmin+year)

form2 <- as.formula(log10(out_flow)~flood_disaster+other_disaster+total_claims+
                      coastal+HPI+Unemployment_Rate+Jan_tmin+year)

form3 <- as.formula(log10(out_flow)~flood_disaster+other_disaster+total_claims+total_claim_pay+
                      HPI+Unemployment_Rate+Jan_tmin+year | coastal)


M4 <- lm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+nat_amenity_rank+
           HPI+Unemployment_Rate+Jan_tmin+log(pop_density),
         data=mig_clm_shp, na.action = NULL)

summary(M4)

M3 <- lm(log10(out_flow)~flood_disaster+other_disaster+total_claims+coastal+HPI+Unemployment_Rate+Jan_tmin+as.factor(year)+
           as.factor(state_name), data=mig_clm_shp)


M3 <- tsls(form1,~HPI+Unemployment_Rate+Jan_tmin+year+coastal+fips,data=mig_dat_v2)

?morantest


mig.fe_lfe <- felm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+nat_amenity_rank+
                     HPI+Unemployment_Rate+Jan_tmin+log(pop_density)|
                     state_name+year|0|state_name,
                   data = mig_clm_shp,na.action = NULL)

mig.fe_year <- felm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+nat_amenity_rank+
                      HPI+Unemployment_Rate+Jan_tmin+log(pop_density)|
                     year|0|0,
                   data = mig_clm_shp,na.action = NULL)

mig.fe_state <- felm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+nat_amenity_rank+
                       HPI+Unemployment_Rate+Jan_tmin+log(pop_density)+year|
                       state_name|0|0,
                    data = mig_clm_shp,na.action = NULL)

mig.fe_coastal <- felm(log10(out_flow)~flood_disaster+other_disaster+total_claims+HPI+Unemployment_Rate+Jan_tmin+lag_s_out_flow|
                         year|0|0,
                    data = mig_data_v2,na.action = NULL)

mig.fe_lag <- felm(log10(out_flow)~flood_disaster+other_disaster+total_claims+HPI+Unemployment_Rate+Jan_tmin+lag_s_out_flow|
                         year|0|0,
                       data = mig_data_v2,na.action = NULL)
mig.ols_trend <- felm(out_flow~flood_disaster+other_disaster+total_claims+HPI+Unemployment_Rate+Jan_tmin+year+(year^4)|
                      0|0|0,
                    data = mig_data_v2,na.action = NULL)

mig.ols_lfe <- felm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+nat_amenity_rank+
                      HPI+Unemployment_Rate+log(pop_density)+year
                      |0|0|0,
                    data = mig_clm_shp,na.action = NULL)
summary(mig.fe_lfe)
?lead
mig_data_v2 <- mig_data_v2 %>% mutate(log_out_flow = log10(out_flow))
dplyr::lag()
plot(gstsls.mig$residuals)

mig_glm <-glm(log10(out_flow)~flood_disaster+other_disaster+total_claims+coastal+HPI+Unemployment_Rate+Jan_tmin+year,data = mig_clm_shp,na.action=NULL)

mean(M4$residuals)

demeanlist(mig_data_v2, fl = list(fips = factor(mig_data_v2[10:13])))
demeanlist()

felm(log10(out_flow)~(year)|fips|0|0,data = mig_dat_v2,na.action = NULL)
summary(mig.fe_year)

mig.gam <- gam(log10(out_flow)~flood_disaster+other_disaster+total_claims+
                 coastal+log10(HPI)+Unemployment_Rate+Jan_tmin+as.integer(year),
               data = mig_dat_v2, family = gammals())
summary(mig.gam)
coeftest(mig.bam)
mig.ols_estimatr <- lm_robust(form2,data = mig_dat_v2, clusters = state_name,na.action = NULL)

mig_ols_cluster <-lm.cluster(form2,data = mig_dat_v2,cluster = "state_name")
summary(M1)

mig.plm <- pdata.frame(mig_dat_v2,index = c("fips","year"))

mig.plm <- make.pbalanced(mig_dat_v2,balance.type = "fill",index = c("fips","year"))
mig.plm <- pdata.frame(mig.plm,index = c("fips","year"))
is.pbalanced(mig.plm)

?felm()
mig.pooled <- plm(form2, mig.plm,model="pooling")

summary(mig.fe_lfe)
coeftest(mig.fe_lfe,vcov. = cbind(vcov_county,vcov_year))

## output tables ----------------

#summary stats table 
install.packages("arsenal")
library(arsenal)
my_labels <- list(
  out_flow = "Out Flows",
  flood_disaster="Flood Disaster Declared",
  other_disaster="Non-Flood Disaster Declared",
  total_claims = "NFIP Claims",
  coastal = "Coastal County",
  nat_amenity_rank="Natural Amenity Rank",
  HPI = "Housing Price Index",
  Unemployment_Rate = "Unemployment Rate",
  Jan_tmin = "January Temperature Minimum",
  pop_density="Population Density",
  year="Year"
  )

my_controls <- tableby.control(
  test = F,
  total = T,
  numeric.stats = c("meansd","medianq1q3","range","Nmiss2"),
  cat.stats = c("countpct","Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1,Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing"
  )
)

?rmarkdown::render
table_one <- tableby(~out_flow +flood_disaster + other_disaster+total_claims+coastal+nat_amenity_rank+
                       HPI+Unemployment_Rate+Jan_tmin+pop_density+year,data = mig_clm_shp,control = my_controls)

summary(table_one,labelTranslations=my_labels,
        title="Summary Statistics Table")
write2html(table_one,"~/Desktop/Thesis/IRS_mig_data/Summary_table.html",title="Summary Statistics Table")
write2word(table_one,"~/Desktop/Thesis/IRS_mig_data/Summary_table.doc",title="Summary Statistics Table")
multiply.100 <- function(x) (x*100)

stargazer(M1,mig.ols_lfe,mig.fe_year,mig.fe_state,mig.fe_coastal,mig.fe_lfe,type = "html",
          model.names = FALSE,
          add.lines = list(c("Year FE","-","-","Yes","-","Yes","Yes"),
                           c("Region FE","-","-","-","Yes","Yes","Yes"),
                           c("Corrected Spatial Autocorrelation","-","-","-","-","-","Yes")),
          out = "reg_table4.htm")


stargazer(mig.ols_lfe,mig.fe_year,mig.fe_state,mig.fe_lfe,type = "html",
          title = "Modeling Migration Outflows with Natural Disaster Indicators",
          model.names = FALSE,omit.stat = c("ser","f"),
          apply.coef = multiply.100, apply.se = multiply.100,
          add.lines = list(c("Year FE","-","Yes","-","Yes"),
                           c("Region FE","-","-","Yes","Yes")),
          out = "clusters_table2.htm")

stargazer(mig.ols_lfe,mig.fe_year,mig.fe_state,mig.fe_lfe,type = "html",
          title = "Modeling Migration Outflows with Natural Disaster Indicators",
          model.names = FALSE,omit.stat = c("ser","f"),
          apply.coef = multiply.100, apply.se = multiply.100,
          add.lines = list(c("Year FE","-","Yes","-","Yes"),
                           c("Cluster FE","-","-","Yes","Yes")),
          out = "clus_fe_table1.htm")


dwplot(list(M1,mig.ols_lfe,mig.fe_year,mig.fe_state,mig.fe_coastal,mig.fe_lfe)) + 
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Modeling Disaster Mobility") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.80, 0.65),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank())

dwplot(list(mig.ols_lfe,mig.fe_year,mig.fe_state,mig.fe_lfe)) + 
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Modeling Disaster Mobility (Clustered FE)") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.80, 0.65),
        legend.justification = c(0, 0), 
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank())

dwplot(gstsls.mig)

stargazer(gstsls.mig,type = "html",out = "stsls_table.htm")
modelsummary(gstsls.mig)

linearHypothesis(mig.fe_lfe,"flood_disaster=0")
 
mig.fe <- plm(form2,data = mig_dat_v2,index = c("state_name","year"),model = "within")
mig.re <- plm(form2,data = mig.plm,model = "random")
phtest(mig.fe,mig.re) # fixed effects is preferred 

coeftest(M1, vcov = vcov_state,type="HC1")
mig.fe_time <- summary(fixef(mig.fe,effect = "time"))
mig.fe_time

stargazer(mig.pooled,mig.fe,type="html",title = "regression output table",out = "table1.htm")

fe <- getfe(mig.fe_lfe,se=F,robust = T,cluster = T)
summary(fe$effect)
# check assumptions ----
library(ggfortify)
library(broom)
library(sandwich)
library(lmtest)
library(gvlma)
library(MASS)

gvlma::gvlma(M3)
?spatialreg::stsls
model_diag <-augment(gstsls.mig)
car::vif(M4)
cor.test(mig_state_year_demeaned$coastal,M4$residuals)
spatialreg::Hausman.test.gmsar(gstsls.mig)

NWvcov<- NeweyWest(M4)
NWreg<-coeftest(M4,NWvcov)
NW_M4 <- coeftest(M4,vcov. = vcovHAC(M4), save=TRUE)

plot(x,y, frame= F)
df.residual
str(NW_M4)
summary(NWvcov)
HAC
hist(gstsls.mig$residuals, breaks = 50)
cor.test(mig_state_year_demeaned$flood_disaster,M4$residuals)

model_diag <- model_diag %>%
  mutate(index = 1:nrow(model_diag)) %>%
  select(index, everything(), -.se.fit, -.sigma)
head(model_diag)

model_diag %>% top_n(5,wt=.cooksd)

NeweyWest(lm())

par(mfrow=c(2,2))
plot(gstsls.mig$fitted.values,gstsls.mig$residuals)
qqnorm(M4$residuals)
plot(M4,1)
plot(mig.fe_lfe$fitted.values,mig.fe_lfe$residuals)
# add spatial data and create spatial neighborhood structure ---------
county_shp <- counties(cb=TRUE) # load county spatial data to join with main dataset 
county_shp <- st_as_sf(county_shp) %>% mutate(GEOID=as.numeric(GEOID))
mig_clm_shp <- left_join(mig_claims,county_shp, by=c("fips"="GEOID"))
mig_clm_shp <- mig_clm_shp %>% select(-STATEFP.y:-AWATER.y)

mig_clm_shp_08 <- mig_clm_shp %>% filter(year == 2005)


mig_clm_shp <- st_as_sf(mig_clm_shp)
mig_clm_sp <- as(mig_clm_shp,Class = "Spatial")

mig_clm_shp_08 <- st_as_sf(mig_clm_shp_08)
mig_clm_sp_08 <- as(mig_clm_shp_08,Class = "Spatial")

mig.nb_08 <- poly2nb(mig_clm_sp_08,queen = TRUE)
mig.listw_08 <- nb2listw(mig.nb_08,style="W",zero.policy=TRUE)

mig.nb <- spdep::poly2nb(mig_clm_sp,queen = TRUE)
mig.listw <- spdep::nb2listw(mig.nb,style="W",zero.policy=TRUE)

# two stage least squares 
library(spatialreg)
demeanlist(fe_mat)
fe_mat <- as.matrix(mig_dat_v2$year)
?demeanlist

s2sls.mig <- s2sls(form2,data = mig_dat_v2,mig.mat)

span.mig <- span(form2,data = mig_dat_v2,mig.listw,n=63312,t=21,model = "fe")

mig.mat <- as.matrix(mig.listw)
h <- kronecker(mig.mat,diag(nrow = mig.mat))

stsls.mig <- stsls(out_flow~flood_disaster+other_disaster+total_claims+
                     coastal+HPI+Unemployment_Rate+Jan_tmin, data = mig_state_year_demeaned,listw = mig.listw,
                   na.action = NULL,robust = TRUE)
summary(stsls.mig)

stsls.res2 <- residuals(stsls.mig)#stsls.mig$residuals

mig_clm_shp <- mig_clm_shp %>% mutate(out_flow_pc = (out_flow/population))
mig_clm_shp <- mig_clm_shp %>%  mutate(out_flow_pc = if_else(out_flow_pc==0,0.1,out_flow_pc))
# mutate_at(vars(out_flow_pc), ~replace(., is.nan(.), 0))
mig_clm_shp <- mig_clm_shp %>% mutate_all(~replace(.,is.na(.),0.1))
mig_clm_shp <- mig_clm_shp %>% mutate(ALAND = as.numeric(ALAND))  
mig_clm_shp <- mig_clm_shp  %>%  mutate(pop_density = (population/(ALAND/1000000)))
mig_clm_shp <- mig_clm_shp %>%  mutate(pop_density = if_else(pop_density==0,0.1,pop_density))


naniar::gg_miss_var(mig_clm_shp)
x <- mig_clm_shp$out_flow_pc
sum(is.null(mig_clm_shp$out_flow_pc))

gstsls.mig <- spatialreg::gstsls(out_flow~flood_disaster+other_disaster+total_claims+coastal+amenity_rank+
                                   HPI+Unemployment_Rate+Jan_tmin+pop_density
                                 , data = mig_year_demeaned,listw = mig.listw,
                     na.action = NULL,robust = TRUE)
summary(gstsls.mig)
gstsls.mig$fitted.values
gstsls.res <- residuals(gstsls.mig,mig.listw)

impacts.gmsar(stsls.mig,listw = mig.listw)

summary(gstsls.mig)

missings <- mig_data_v2 %>% 
  group_by(fips) %>% 
  mutate(count = n()) %>% 
  select(fips, year, count) %>% 
  filter(count <2)

# take these out
mig_samp_shp2 <- mig_samp_shp %>% 
  filter(!(fips %in% missings$fips))
county_shp2 <- county_shp %>% 
  filter(!(GEOID %in% missings$fips))

# check again
mig_samp_shp2 %>% 
  group_by(fips) %>% 
  mutate(count = n()) %>% 
  select(fips, year, count) %>% 
  filter(count <2)

gc()
sample <- mig_data_v2 %>% filter()
sample <- sample %>% select(fips,year,out_flow,flood_disaster,other_disaster,total_claims,coastal,HPI,Unemployment_Rate,Jan_tmin)
pdat <- pdat %>% mutate_all(~replace(.,is.na(.),0.1))

pdat <- pdata.frame(sample,index = c("fips","year"),drop.unused.levels = T)
is.pbalanced(pdat)
pdat<-make.pconsecutive(pdat,balanced = T)
class(pdat)

GM <- spgm(log(out_flow)~flood_disaster+other_disaster+total_claims+
             coastal+HPI+Unemployment_Rate+Jan_tmin,data=pdat,index=c("fips","year"),listw=samp.matrix,
          method="g2sls")

summary(gstsls.mig)
data(Produc, package = "plm") 
data(usaww)
spdep::moran.mc(gstsls.mig$residuals,listw = mig.listw,zero.policy = T,nsim=99)
pdat <- pdat %>% mutate(fips = as.numeric(fips))
county_shp <- county_shp %>% mutate(fips=as.numeric(fips))

county_shp <- county_shp %>% mutate(fips = as.factor(GEOID))

mig_samp_shp <- sample %>% left_join(county_shp,by=c("fips"="GEOID"))
mig_samp_shp <- county_shp %>% right_join(pdat,by="fips")
mig_samp_shp <- county_shp %>% semi_join(sample,by=c("GEOID"="fips"))



mig_samp_shp <- st_as_sf(mig_samp_shp)
mig_samp_sp <- as(mig_samp_shp,Class = "Spatial")
samp.nb <-poly2nb(mig_samp_sp,queen=TRUE)
samp.listw <- nb2listw(samp.nb,style = "W",zero.policy=T)
samp.matrix <- nb2mat(samp.nb,zero.policy = T)
W_C <- as(samp.listw, "CsparseMatrix")


set.ZeroPolicyOption(TRUE)
samp.mat <- nb2mat(samp.nb2,style = "W")
spdep::moran.mc(mig.fe_year$residuals,listw = mig.listw,nsim = 99)

tidy.gmsar <- function(x, ...) {
  s <- summary(x, ...)
  ret <- data.frame(
    term      = row.names(s$solutions),
    estimate  = s$solutions[, 1],
    conf.low  = s$solutions[, 2],
    conf.high = s$solutions[, 3])
  ret
}

glance.gmsar <- function(x, ...) {
  ret <- data.frame(
    dic = x$DIC,
    n   = nrow(x$X))
  ret
}
class(gstsls.mig)

# cluster standard errors 
library(lmtest)
?vcovHC
coef.stsls(stsls.mig)

vcov(M1)
vcov_year <- cluster.vcov(gstsls.mig,mig_dat_v2$year) # cluster by year 
coeftest(M1,vcov_year)

vcov_county <- cluster.vcov(M1,mig_dat_v2$fips) # cluster by county 
coeftest(M1,vcov_county)
waldtest(M1,vcov_county,test = "F")
bgtest(M1,vcov_county )

vcov_state <- cluster.vcov(M1, mig_dat_v2$state_name)
vcov_state

# check assumptions 

moran.test(mig.ols_lfe$residuals,mig.listw_08,zero.policy = T)

spdep::moran.mc(gstsls.mig$resid,mig.listw,nsim = 99,zero.policy = T)

# test new spatial models ----------
install.packages("spmoran")
library(spmoran)
library(spdep)

moran.plot(log10(mig_clm_shp$out_flow),mig.listw)

local_m <- localmoran(mig_clm_shp$log_out_flow,mig.listw)

## adding scale and spatial lag variable 
mig_clm_shp <- mig_clm_shp %>% mutate(log_out_flow = log10(out_flow))
mig_dat_v2 <- mig_data_v2 %>% mutate(log_out_flow = log10(out_flow))


mig_clm_shp$scale_out_flow <- scale(mig_clm_shp$log_out_flow) %>% as.vector()
mig_clm_shp$lag_s_out_flow <- lag.listw(mig.listw, mig_clm_shp$log_out_flow)

mig_data_v2$scale_out_flow <- scale(mig_dat_v2$log_out_flow) %>% as.vector()
mig_data_v2$lag_s_out_flow <- lag.listw(mig.listw, mig_dat_v2$log_out_flow)

summary(mig_data_v2$lag_s_out_flow)
summary(mig_clm_shp$scale_out_flow)
quantile(mig_clm_shp$lag_s_out_flow)

## creating new variable with conditionals to find clusters of local SAC

mig_clm_shp <- mig_clm_shp %>% st_as_sf(mig_clm_shp) %>% 
  mutate(quad_sig = ifelse(mig_clm_shp$scale_out_flow > 0 & 
                             mig_clm_shp$lag_s_out_flow > 3.1 & 
                             local_m[,5] <= 0.05, 
                           "high-high",
                    ifelse(mig_clm_shp$scale_out_flow <= 0 & 
                             mig_clm_shp$lag_s_out_flow <= 3.1 & 
                             local_m[,5] <= 0.05, 
                            "low-low", 
                    ifelse(mig_clm_shp$scale_out_flow > 0 & 
                             mig_clm_shp$lag_s_out_flow <= 3.1 & 
                             local_m[,5] <= 0.05, 
                              "high-low",
                    ifelse(mig_clm_shp$scale_out_flow <= 0 & 
                             mig_clm_shp$lag_s_out_flow > 3.1 & 
                             local_m[,5] <= 0.05,
                              "low-high", 
                           "non-significant")))))

mig_data_v2 <- mig_data_v2 %>% mutate(sac_cluster = ifelse(quad_sig=="high-high",1,0))

mig_clusters <- mig_clm_shp %>% filter(sac_cluster==1)
mig_non_clusters <- mig_clm_shp %>% filter(sac_cluster!=1)

qtm(mig_clm_shp, fill="quad_sig", fill.title="LISA")
library(viridis)
tm_shape(mig_clm_shp) +
  tm_polygons(col = "quad_sig" ,style="quantile",border.col = "grey60",border.alpha = 0.6,
              palette="-Reds",title="") +
  tm_layout(main.title = "Local Indicators of Spatial Autocorrelation on Outflows",  
            main.title.size = 0.95, frame = FALSE) 

tm_shape(mig_clm_shp) + tm_fill("quad_sig",palette="-Reds")

tm_shape(mig_clm_shp) + tm_polygons(col="quad_sig",palette="-Reds",border.col = "grey60", border.alpha = 0.4,title="") +
  tm_layout(main.title = "Local Indicators of Spatial Autocorrelation on Outflows",frame=FALSE)

table(mig_clm_shp$quad_sig)
length(mig_clm_shp$quad_sig)

coords <- mig_clm_sp@polygons
coords<-coordinates(mig_clm_sp)
meig_f <- meigen_f(coords = coords,enum = 250)
meig <- meigen(coords = coords)
weig <- weigen(coords)


y <- mig_year_demeaned$out_flow
x <- mig_year_demeaned[,c("flood_disaster","other_disaster","total_claims",
                  "coastal","amenity_rank","HPI","Unemployment_Rate","Jan_tmin","pop_density","year")]

mig.lslm <-lslm(y=y,x=x,weig = weig)
mig.lslm
lm_esf <- lm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+nat_amenity_rank+
               HPI+Unemployment_Rate+Jan_tmin+log(pop_density)+year,data = mig_clm_shp)
lm_resf <- lm(log10(out_flow)~flood_disaster+other_disaster+total_claims+
               coastal+HPI+Unemployment_Rate+Jan_tmin+year,data = mig_clm_shp)
lm_lslm <- lm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+nat_amenity_rank+
                HPI+Unemployment_Rate+Jan_tmin+log(pop_density)+year,data = mig_clm_shp)

lm_gstsls <- lm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+nat_amenity_rank+
                  HPI+Unemployment_Rate+Jan_tmin+log(pop_density),data = mig_clm_shp)

resf.mig <- besf(y=y,x=x,coords = coords)
resf.mig

meig0<-meigen0(meig_f,coords)

esf_mig <- esf(y=y,x=x,meig = meig_f,fn="all")
esf_mig

resf.mig <- resf(y=y,x=x, meig = meig_f)
res_est <- as.list(resf.mig$b)
esf_est <- as.list(esf_mig$b)
mig_est <- as.list(mig.lslm$b)
gstsls_est <- as.list(gstsls.mig$coefficients)

res_est <- c(esf_est,res_est)
res_se <- as.list(res_est[2])
esf_se <- as.list(esf_est[2])
lslm_se <- as.list(mig_est[2])
res_t <- as.list(res_est[3])
esf_t <- as.list(esf_est[3])
esf_t <- as.list(esf_est[3])

res_p <- as.list(res_est[4])
esf_p <- as.list(esf_est[4])
lslm_p <- as.list(mig_est[4])

res_coefs$coefficients<-rownames(resf.mig$b)
res_coefs$coefficients
resf.mig$b
lm_res$coefficients

names(res_coefs$p_value) <- rownames(resf.mig$b)

## cross-validation attempts ----
library(caret)
library(forecast)
library(devtools)
devtools::install_github("Techtonique/crossval")
library(crossval)
install.packages("blockCV")
library(blockCV)
library(fpp)
library(sf)
library(raster)

mig_year_fe <- mig_data_v2 %>% mutate(year_1990 = if_else(year==1990,1,0)) %>% 
  mutate(year_1991 = if_else(year==1991,1,0)) %>%
  mutate(year_1992 = if_else(year==1992,1,0)) %>% 
  mutate(year_1993 = if_else(year==1993,1,0)) %>% 
  mutate(year_1994 = if_else(year==1994,1,0)) %>% 
  mutate(year_1995 = if_else(year==1995,1,0)) %>% 
  mutate(year_1996 = if_else(year==1996,1,0)) %>% 
  mutate(year_1997 = if_else(year==1997,1,0)) %>% 
  mutate(year_1998 = if_else(year==1998,1,0)) %>% 
  mutate(year_1999 = if_else(year==1999,1,0)) %>% 
  mutate(year_2000 = if_else(year==2000,1,0)) %>% 
  mutate(year_2001 = if_else(year==2001,1,0)) %>% 
  mutate(year_2002 = if_else(year==2002,1,0)) %>% 
  mutate(year_2003 = if_else(year==2003,1,0)) %>% 
  mutate(year_2004 = if_else(year==2004,1,0)) %>%
  mutate(year_2005 = if_else(year==2005,1,0)) %>%
  mutate(year_2006 = if_else(year==2006,1,0)) %>%
  mutate(year_2007 = if_else(year==2007,1,0)) %>%
  mutate(year_2008 = if_else(year==2008,1,0)) %>%
  mutate(year_2009 = if_else(year==2009,1,0)) %>%
  mutate(year_2010 = if_else(year==2010,1,0))

mig_year_fe <- mig_year_fe %>% dplyr::select(out_flow,flood_disaster,other_disaster,total_claims,coastal,HPI,
                                      Unemployment_Rate,Jan_tmin,year,year_1990,year_1991,year_1992,year_1993,
                                      year_1994,year_1995,year_1996,year_1997,year_1998,year_1999,year_2000,
                                      year_2001,year_2002,year_2003,year_2004,year_2005,year_2006,year_2007,
                                      year_2008,year_2009,year_2010)

mig_year_fe <- mig_year_fe %>% dplyr::select(-year_1990)
mig_clm_shp <- mig_clm_shp %>%  mutate(in_flow = if_else(in_flow==0,0.1,in_flow))

mig_year_demeaned <- with(mig_clm_shp,
                           data.frame(out_flow = log(out_flow) - ave(log(out_flow), year),#-ave(log(out_flow),state_name),
                                      net_flow = net_flow - ave(net_flow,year),#-ave(net_flow,state_name),
                                      in_flow = log(in_flow) - ave(log(in_flow),year),#-ave(log(in_flow),state_name),
                                      total_movement = log(total_movement) - ave(log(total_movement),year),#-ave(log(total_movement),state_name),
                                     # disaster = disaster - ave(disaster,year)-ave(disaster,nat_amenity_rank),
                                      flood_disaster = flood_disaster - ave(flood_disaster, year),#-ave(flood_disaster,state_name),#-ave(flood_disaster,state_name),
                                      other_disaster = other_disaster - ave(other_disaster, year),#-ave(other_disaster,state_name),#-ave(other_disaster,state_name),
                                      total_claims = total_claims - ave(total_claims, year),#-ave(total_claims,state_name),#-ave(total_claims,state_name),
                                      coastal = coastal - ave(coastal, year),#-ave(coastal,state_name),
                                      amenity_rank = nat_amenity_rank-ave(nat_amenity_rank,year),#-ave(nat_amenity_rank,state_name),
                                      HPI = HPI - ave(HPI, year),#-ave(HPI,state_name),#-ave(HPI,state_name),
                                      Unemployment_Rate = Unemployment_Rate - ave(Unemployment_Rate, year),#-ave(Unemployment_Rate,state_name),#-ave(Unemployment_Rate,state_name),
                                      Jan_tmin = Jan_tmin - ave(Jan_tmin, year),#-ave(Jan_tmin,state_name),
                                      pop_density = log(pop_density) - ave(log(pop_density), year),#-ave(log(pop_density),state_name),#-ave(Jan_tmin,state_name),
                                      year=year,
                                      geometry=geometry))

mig_clm_shp <- mig_clm_shp %>% mutate(log_out_flow=log10(out_flow))
coords0<-coordinates(test_sp)
coords<- coordinates(train_sp)
meig_f <- meigen_f(coords = coords0,enum = 250)

y <- log10(train_data$out_flow)
x <- train_data[,c("flood_disaster","other_disaster","total_claims",
                   "coastal","HPI","Unemployment_Rate","Jan_tmin","year")]
meig0 <- meigen0(meig_f,coords0)
# simple data split validation 
library(caret)
library(plm)
library(spatialreg)
library(spdep)
library(beepr)

# Split the data into training and test set
set.seed(1234)
reps <- 500
sim <- function(reps) {}
out_df <- data.frame()

for (i in 1:100) {
training.samples <- mig_state_year_demeaned$out_flow %>%
  createDataPartition(p = 0.7, list = F)
train.data  <- mig_state_year_demeaned[training.samples, ]
test.data <- mig_state_year_demeaned[-training.samples, ]
# Build the model
model <- lm(out_flow~flood_disaster+other_disaster+total_claims+coastal+HPI+Unemployment_Rate+Jan_tmin,
                           data = train.data, na.action = NULL)
# Make predictions and compute the R2, RMSE and MAE
predictions <- predict(model,newdata = test.data)
pred_2 <- model %>% predict(train.data)
Train_RMSE <-  RMSE(pred_2, train.data$out_flow)
Test_RMSE <- RMSE(predictions, test.data$out_flow)
in_df <- data.frame(sim = i, Train_RMSE = Train_RMSE, Test_RMSE=Test_RMSE)
out_df <- rbind(out_df, in_df)
#out_df[[i]] <- list( #Train_R2 = R2(pred_2, test.data$out_flow), 
            #Test_R2 = R2(predictions, test.data$out_flow),
            #Train_RMSE = RMSE(pred_2, train.data$out_flow)
            #Test_RMSE = RMSE(predictions, test.data$out_flow)
            #MAE = MAE(predictions, test.data$out_flow))
}

min(out_df$Train_RMSE)
max(out_df$Test_RMSE)
data_split <- replicate()
?simulate
?createDataPartition
simulate
# Set random seed for replication
set.seed(1234)

# Sample data
regdat <- data.frame(mig_year_demeaned)
?distinct
# Cross-validate by year
?size
test_data <- regdat %>% filter(cv_ == 3) 
pb <- txtProgressBar(min=0,max=length(regdat$year),initial=0)

working <- regdat %>% 
  arrange(year) %>% 
  mutate(year_bin = (cut_number(year, 6)))

year_bins <- as.list(unique(working$year_bin))

working_2 <- regdat %>% 
  arrange(year) %>% 
  mutate(year_bin = (cut_number(year, 3)))

year_bins_all <- as.list(unique(working_2$year_bin))


lm(out_flow~flood_disaster+other_disaster+total_claims+coastal+HPI+Unemployment_Rate+Jan_tmin,
   data = regdat, na.action = NULL)
library(spmoran)
regdat <- data.frame(mig_clm_shp)
head(year_bins)
test <- lapply(year_bins, function(x) {

#  temp <- working %>% 
  #  filter(year_bin == x) 
  
 temp <- working %>% 
   filter(year_bin == x)
  
  #train_shp <- st_as_sf(temp)
  #train_sp <- as(train_shp,Class = "Spatial")
  #train_nb <- poly2nb(train_sp,queen = TRUE)
  
  test_shp <- st_as_sf(temp)
  test_sp <- as(test_shp,Class = "Spatial")
  test_nb <- poly2nb(test_sp,queen = TRUE)
  
  #coords<-coordinates(train_sp)
  #meig_f <- meigen_f(coords = coords,enum = 250)
  #meig <- meigen(coords = coords)
  #weig <- weigen(coords)
  
  
  #y <- log(temp$out_flow)
 # x1 <- temp[,c("flood_disaster","other_disaster","total_claims",
          #      "coastal","nat_amenity_rank","HPI","Unemployment_Rate","Jan_tmin","pop_density","year")]
  
  #x1 <- x1 %>% mutate(pop_density = log(pop_density))
  #reg1 <-lslm(y=y,x=x1,weig = weig)
  
  #reg1 <- esf(y=y,x=x1,meig = meig_f,fn="all")
  
  
  reg1 <- spatialreg::gstsls(out_flow~flood_disaster+other_disaster+total_claims+coastal+amenity_rank+
                               HPI+Unemployment_Rate+Jan_tmin+pop_density, data = temp,listw = nb2listw(test_nb,zero.policy = T),
                             na.action = NULL)
  
  
  
  # want to do this in three year groups
  #test_data <- regdat %>% filter(year== cv_)
  #train_data <- regdat %>% filter(year != cv_)
  
 # reg1 <- lm(out_flow~flood_disaster+other_disaster+total_claims+coastal+amenity_rank+
     #          HPI+Unemployment_Rate+Jan_tmin+pop_density,
   #          data = temp, na.action = NULL)

  #r2 <- summary(reg1)$r.squared
  moran <- moran.mc(reg1$residuals,listw = nb2listw(test_nb,zero.policy = T),nsim = 99)
 # rmse_train <- sqrt(mean(reg1$residuals^2))
  rmse <- sqrt(mean(reg1$residuals^2))
  
  #data.frame(year= x ,train_rmse= rmse,train_moran_stat = moran$statistic, train_moran_p = moran$p.value)
  data.frame(year = x,test_rmse= rmse,test_moran_stat = moran$statistic, test_moran_p = moran$p.value)
  #test_rmse= rmse,test_r2 = r2,train_rmse= rmse,train_r2 = r2,
 
  
})  
test <- bind_rows(test)
beep(1)

mean(test$test_moran_stat)

cv_outdat <- data.frame()
for (cv_ in unique(regdat$year)){
  # Train test split by year
  test_data <- regdat %>% filter(year == cv_)
  train_data <- regdat %>%filter(year != cv_)
  
    #filter(regdat, year == cv_)
  
  # Make spatial weights matrix for train data
  train_shp <- st_as_sf(train_data)
  train_sp <- as(train_shp,Class = "Spatial")
  train_nb <- poly2nb(train_sp,queen = TRUE)
  
  test_shp <- st_as_sf(test_data)
  test_sp <- as(test_shp,Class = "Spatial")
  test_nb <- poly2nb(test_sp,queen = TRUE)
  
  # Estimate model with train_data
  #mod <- spatialreg::gstsls(log10(out_flow)~flood_disaster+other_disaster+total_claims+
    #                   coastal+HPI+Unemployment_Rate+Jan_tmin, data = train_data,listw = nb2listw(train_nb,zero.policy = T),
     #                na.action = NULL,robust = TRUE)
  

    #mod2 <- spatialreg::gstsls(log10(out_flow)~flood_disaster+other_disaster+total_claims+
                              # coastal+HPI+Unemployment_Rate+Jan_tmin, data = test_data,listw = nb2listw(test_nb,zero.policy = T),
                             #na.action = NULL)
  mod <- lm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+amenity_rank+
              HPI+Unemployment_Rate+Jan_tmin+log(pop_density),
                                data = train_data, na.action = NULL)
  
  mod2 <- lm(log(out_flow)~flood_disaster+other_disaster+total_claims+coastal+amenity_rank+
              HPI+Unemployment_Rate+Jan_tmin+log(pop_density),
            data = test_data, na.action = NULL)
  #mod <- besf(y=y,x=x,coords = coords
 # residuals <- residuals(mod)
  
  # Get pred_y
  #train_pred_y <- predict(mod)
  #test_pred_y <- predict(mod,newdata=test_data)
  
  # Get true_pred_y
  #train_true_y <- train_data$out_flow
  #test_true_y <- test_data$out_flow
  
  # Get stats (r-sq, rmse, Moran I)
  #rsq <- summary(mod)[[8]]
  #test_rsq <- summary(mod2)[[8]]
  #train_rmse <- sqrt(mean(mod$residuals^2))
  #test_rmse <- sqrt(mean((test_true_y - test_pred_y)^2))
  train_moran <- moran.mc(mod$residuals,listw = nb2listw(train_nb,zero.policy = T),nsim = 49)
  test_moran <- moran.mc(mod2$residuals,listw = nb2listw(test_nb,zero.policy = T),nsim = 49)
  #print(cv_)
  #Sys.sleep(0.01)
  #flush.console()
  # (OTHER STATS)
  
  # Bind data together
  cv_indat <- data.frame(year = cv_, train_moran_stat = train_moran$statistic,train_moran_p =train_moran$p.value,
                         test_moran_stat = test_moran$statistic,test_moran_p =test_moran$p.value )#train_rmse = train_rmse,test_rmse=test_rmse,train_rsq=rsq,test_rsq=test_rsq)
  cv_outdat <- rbind(cv_outdat, cv_indat)
}

head(cv_outdat)

cv_outdat %>% summarise(across(everything(),list(mean)))



library(naniar)
dim(is.nan(test_data$log_out_flow))
train_pred_y <- mod$pred
test_pred_y <- mod2$pred

mean(cv_outdat$test_rmse)
train_rmse <- sqrt(mean((train_true_y - train_pred_y)^2))
test_rmse <- sqrt(mean((test_true_y - test_pred_y)^2))
test_true_y <- test_data$out_flow

train_true_y <- train_data$log_out_flow
test_true_y <- test_data$log_out_flow

train_pred_y <- predict.SLX(mod,listw=nb2listw(train_nb,zero.policy = T))
test_pred_y <- predict.SLX(mod, newdata = test_data,listw = test.listw,zero.policy=T)

mod2 <- besf(y=y2,x=x2,coords = coords0)


mod2 <-spatialreg::gstsls(log10(out_flow)~flood_disaster+other_disaster+total_claims+
                     coastal+HPI+Unemployment_Rate+Jan_tmin, data = train_data,listw = nb2listw(train_nb,zero.policy = T),
                   na.action = NULL,robust = TRUE)

mod2 <- spatialreg::gstsls(log10(out_flow)~flood_disaster+other_disaster+total_claims+
                            coastal+HPI+Unemployment_Rate+Jan_tmin, data = mig_data_91,listw = nb2listw(mig_data_91_nb,zero.policy = T),
                          na.action = NULL)

mig_data_91 <- mig_clm_shp %>% filter(year==1991|year==1993|year==1998)
mig_data_91_shp <- st_as_sf(mig_data_91) %>% as(Class = "Spatial")
mig_data_91_nb <- poly2nb(mig_data_91_shp,queen = TRUE)

predict.SLX(mod2,listw =test.listw)
str2
resid_outdat <- data.frame()
for (i in unique(cv_outdat$year)) {
  #Get residuals 
  train_data <- filter(cv_outdat,year == i)
  resid <- residuals(train_data)
  
  #Calculate Moran's I
  moran <-moran.mc(resid, nb2listw(train_nb,zero.policy = T),nsim=49)
  print(i)
  Sys.sleep(0.01)
  flush.console()
  #bind data 
  resid_indat <- data.frame(year = i, moran= moran[1], moran_p_val = moran[3])
  resid_outdat <- rbind(resid_outdat, resid_indat)
  
}

resid <- residuals(unique(cv_outdat$year))
unique(cv_outdat$year)
moran.mc(cv_outdat$residuals,nb2listw(train_nb,zero.policy = T),nsim = 99)
?ave()
view(train_resid)
summary(moran_ols)
train_shp <- st_as_sf(train_data)
train_sp <- as(train_shp,Class = "Spatial")
train.nb <- poly2nb(test_sp,queen = TRUE)

test.listw <- nb2listw(test_nb,style = "W",zero.policy = T)
mod <- lm(log10(out_flow)~flood_disaster, data=train_data)

train_data$out_flow
moran.mc(mod$residuals,nb2listw(train.nb),nsim = 49)
length(train.nb)
length(mod$residuals)

?predict.SLX
predict.SLX(gstsls.mig,listw = mig.listw)

?poly2nb
train_shp <- st_as_sf(train_data)
train_sp <- as(train_shp,Class = "Spatial")
train.nb <- poly2nb(train_sp,queen = TRUE)
train.listw <- nb2listw(train.nb,style="W")


set.seed(123)

random_sample <- createDataPartition(mig_dat_v2 $log_out_flow,p=0.8,list=F)

training_data <- mig_dat_v2[random_sample,]
testing_data <- mig_dat_v2[-random_sample,]

model <- lm(log_out_flow ~ flood_disaster+other_disaster+total_claims+HPI+Unemployment_Rate+Jan_tmin,data = training_data)

predictions <- predict(model,testing_data)

data.frame(R2 = R2(predictions,testing_data $ log_out_flow),
           RMSE = RMSE(predictions,testing_data$log_out_flow),
           MAE = MAE(predictions,testing_data$log_out_flow))
# make a raster for polygons 
mig_raster <- raster("mig_raster1.tif",package="raster")
mig_raster <- setMinMax(mig_raster)
plot(mig_raster)


mig_shp_2 <- sf::st_as_sf(mig_clm_shp,crs=crs(mig_raster))

class(mig_shp_2)

mig_shp_2$log_out_flow <- log10(mig_shp_2$out_flow)

r <- raster(ncol=4001, nrow=4001)
extent(r) <- extent(mig_clm_sp)
head(mig_clm_sp@polygons)
mig_clm_sp$ranks <- rank(mig_clm_sp@plotOrder)

mig_raster <- rasterize(mig_clm_sp, r,mig_clm_sp$log_out_flow,fun='first',update=TRUE)
writeOGR(obj=mig_clm_sp,dsn = "Migration_Model",layer = "mig_shp",driver = "ESRI Shapefile")
# sensitivity analysis ----
library(spdep)
library(spatialreg)
library(spmoran)

calc_new_pred <- function(df, perc_increase){
  df$X1 <- (df$flood_disaster+df$other_disaster) + perc_increase #%>% as.numeric()
  return(df)
}

set.seed(12345)
regdat <- data.frame(mig_year_demeaned)
form <- as.formula(out_flow~X1+total_claims+
  coastal+HPI+Unemployment_Rate+Jan_tmin)
# Estimate pooled model
#mod <- esf(y=y,x=X1+mig_year_demeaned$total_claims+mig_year_demeaned$coastal+mig_year_demeaned$HPI+
 #             mig_year_demeaned$Unemployment_Rate+mig_year_demeaned$Jan_tmin,meig = meig_f,fn="all")

mod <- spatialreg::gstsls(form, data = regdat,listw = nb2listw(mig.nb,zero.policy = T),
                          na.action = NULL)
mod <- lm(out_flow~X1+total_claims+coastal+HPI+Unemployment_Rate+Jan_tmin,
                                  data = regdat, na.action = NULL)
#mod <- lm(y ~ X1 + year, data = regdat)
summary(mod)
sum(exp(y_pred_25percent$out_flow))
# Get new prediction data
y_pred_00percent <- calc_new_pred(regdat,0)
y_pred_05percent <- calc_new_pred(regdat,1)
y_pred_10percent <- calc_new_pred(regdat,2)
y_pred_15percent <- calc_new_pred(regdat,3)
y_pred_20percent <- calc_new_pred(regdat,4)
y_pred_25percent <- calc_new_pred(regdat,5)
?predict.SLX
# Get new predictions
y_pred_00 <- exp(predict.SLX(mod)+mod$residuals)
y_pred_05 <- (fitted.gmsar(mod05))
y_pred_10 <- exp(predict.SLX(mod10))
y_pred_15 <- exp(predict.SLX(mod15)+mod$residuals)
y_pred_20 <- exp(predict.SLX(mod20)+mod$residuals)
y_pred_25 <- (predict.SLX(mod25))
mean(y_pred_05)

coefs<- mod$coefficients
y_pred_00 <- exp(diag(x))
vars <- rownames(attr(coefs, "mixedImps")$dirImps)
f <- formula(paste("~", paste(vars, collapse=" + ")))
mod$
# manual prediction 
coefs<- mod$coefficients
pred_manual <- as.matrix(dplyr::select(y_pred_00percent,X1)) %*% coefs
(y_pred_00 - pred_manual)
sum(y_pred_00)
sum(pred_manual)
y_pred_00 <- exp(pred_manual+mod$residuals)
sum(exp(pred_manual))
mean(y_pred_00)
res <- as.matrix(dplyr::select(x)) %*% coefs
names(res) <- row.names(y_pred_05percent)
fitted <- mod$fitted.values
mean(fitted)

mf <- lm(form, regdat, method="model.frame")
mt <- attr(mf, "terms")
x <- model.matrix(mt, mf)
WX <- create_WX(x, listw=mig.listw, zero.policy=T, prefix="lag")
x <- cbind(x, WX)
res <- as.vector(matrix(x) %*% coefs) 
res <- exp(res)

res_manual <- regdat$out_flow - y_pred_00
res <- mod$residuals
sum(res-res_manual)
head(res)
# Aggregate (or boostrap to get standard errors)
mean_00 <- mean(y_pred_00)
mean_05 <- mean(y_pred_05)
mean_10 <- mean(y_pred_10)
mean_15 <- mean(y_pred_15)
mean_20 <- mean(y_pred_20)
mean_25 <- mean(y_pred_25)
head(mod$coefficients)
# Calc percentage change
perc_delta_00 <- (mean_00 - mean_00) / (mean_00)
perc_delta_05 <- (mean_05 - mean_00) / (mean_00)
perc_delta_10 <- (mean_10 - mean_00) / (mean_00)
perc_delta_15 <- (mean_15 - mean_00) / (mean_00)
perc_delta_20 <- (mean_20 - mean_00) / (mean_00)
perc_delta_25 <- (mean_25 - mean_00) / (mean_00)


pdat_agg7 <- data.frame(type = "aggregate", 
                       dis_increase = c("+0", "+1", "+2", "+3", "+4", "+5"),
                       y_pred_perc = c(perc_delta_00, perc_delta_05, perc_delta_10, perc_delta_15, perc_delta_20, perc_delta_25),
                       model = c("M7","M7","M7","M7","M7","M7"))

pdat_agg<-rbind(pdat_agg4,pdat_agg7)
ggplot(pdat_agg7, aes(dis_increase, y_pred_perc,group=model,color=model)) + 
  geom_point() + 
  geom_line() +
  theme_bw() +
  labs(y="Percentage change from Baseline", x="Increase in Disaster Events")

#try spatial block CV ----
install.packages("rgeos")
install.packages("shiny")

sac <- spatialAutoRange(rasterLayer = mig_raster,
                        doParallel = T,
                        showPlots = T)
plot(sac)
st_crs(mig_shp_2) <- crs(mig_raster)
sb <- spatialBlock(speciesData = mig_shp_2,
                   species = "flood_disaster",
                   rasterLayer = mig_raster,
                   rows = 10,
                   cols = 11,
                   k=5,
                   selection = "systematic")
foldExplorer(blocks = sb,
             rasterLayer = mig_raster,
             speciesData = mig_shp_2)
head(sb)

rangeExplorer(rasterLayer = mig_raster)

?spatialBlock

summary(sac)
plot(sac)
plot(sac$variograms[[1]])
mig.raster<-as.raster(mig_clm_sp@polygons)

#use random forest to test data
library(randomForest)
library(precrec)

gc()
# extract the raster values for the species points as a dataframe
mydata <- raster::extract(mig_raster,mig_shp_2$log_out_flow)
view(mydata)
# adding species column to the dataframe
mydata$Species <- as.factor(mig_shp_2$log_out_flow)
# remove extra column (ID)
mydata <- mydata[,-1]

# extract the fold indices from buffering object 
# created in the previous section
# the folds (list) works for all three blocking strategies
folds <- sb$folds

# create a data.frame to store the prediction of each fold (record)
testTable <- mig_dat_v2
testTable$pred <- NA

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(log_out_flow~., mig_dat_v2[trainSet, ], ntree = 250) # model fitting on training set
  testTable$pred[testSet] <- predict(rf, mig_dat_v2[testSet, ], type = "prob")[,2] # predict the test set
}

# calculate Area Under the ROC and PR curves and plot the result
precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$log_out_flow)

autoplot(precrec_obj)



mig_clm_shp.ts <- as.ts(mig_clm_shp$year)
library(fpp) # To load the data set a10
plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")
plot(log(a10), ylab="", xlab="Year", main="Log Antidiabetic drug sales")

k <- 60 # minimum data length for fitting a model
n <- length(a10)
mae1 <- mae2 <- mae3 <- matrix(NA,n-k-1,12)
st <- tsp(a10)[1]+(k-2)/12
view(AirPassengers)
for(i in 1:(n-k-1))
{
  xshort <- window(mig_clm_shp.ts, end=st + i/12)
  xnext <- window(mig_clm_shp.ts, start=st + (i+1)/12, end=st + (i+12)/12)
  fit1 <- tslm(xshort ~ trend, lambda=0)
  fcast1 <- forecast(fit1, h=12)
  fit2 <- Arima(xshort, order=c(3,0,1), 
                include.drift=TRUE, lambda=0, method="ML")
  fcast2 <- forecast(fit2, h=12)
  fit3 <- ets(xshort,model="MMM",damped=TRUE)
  fcast3 <- forecast(fit3, h=12)
  mae1[i,1:length(xnext)] <- abs(fcast1[['mean']]-xnext)
  mae2[i,1:length(xnext)] <- abs(fcast2[['mean']]-xnext)
  mae3[i,1:length(xnext)] <- abs(fcast3[['mean']]-xnext)
}

plot(1:12, colMeans(mae1,na.rm=TRUE), type="l", col=2, xlab="horizon", ylab="MAE",
     ylim=c(0.65,1.05))
lines(1:12, colMeans(mae2,na.rm=TRUE), type="l",col=3)
lines(1:12, colMeans(mae3,na.rm=TRUE), type="l",col=4)
legend("topleft",legend=c("LM","ARIMA","ETS"),col=2:4,lty=1)

mean(mae1)
length(mig_clm_shp)

mig_clm_shp.ts <- as.ts(mig_clm_shp$year)

# regressors including trend 
xreg <- cbind(1, 1:length(mig_clm_shp.ts))


# cross validation with least squares regression
cv <- crossval_ts(y=mig_clm_shp$log_out_flow, x=xreg, fit_func = crossval::fit_lm,
                   predict_func = crossval::predict_lm,
                   initial_window = 5,
                   horizon = 3,
                   fixed_window = TRUE)

# print results
print(colMeans(cv))
plot(res)

?rnorm

set.seed(21)
?createTimeSlices



train_data <- createTimeSlices(mig_clm_shp$log_out_flow,initialWindow = 21,horizon = 21,fixedWindow = T,skip=0)
mig_clm_shp.train <- mig_clm_shp[train_data,]
mig_clm_shp.test <- mig_clm_shp[- train_data, ]

fit.control <- caret::trainControl(method = "timeslice", number = 10)



fit.control <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 5)
summary(fit.control)
## spatial and part two output tables ---------
library(stargazer)

multiply.100 <- function(x) (x*100)


stargazer(mig.ols_lfe,mig.fe_year,mig.fe_state,mig.fe_lfe, lm_lslm,lm_esf,lm_gstsls,type = "latex",
          align = FALSE,no.space = TRUE,style = "aer", digits = 3,
          title = "Modeling Migration Outflows with Natural Disaster Indicators",
          column.labels = c("Non-Spatial Weights","Spatial Weights"),
          dep.var.labels = "Log (Outflows)",
          column.separate = c(4,3),
          model.names = FALSE, omit.stat = c("ser","f"),
          apply.coef = multiply.100, apply.se = multiply.100,
          notes = "Coefficients and Standard Errors multiplied by 100",
          font.size = "footnotesize",
          add.lines = list(c("Year FE","-","Yes","-","Yes","-","-","Yes"),
                           c("Region FE","-","-","Yes","Yes","-","-","-"),
                           c("Moran's I","0.25","0.25","0.15","0.15","0.24","0.08","-0.01"),
                           c("CV Avg. $R2$","0.52","0.57","0.44","0.50","-","-","-"),
                           c("Avg.Train RMSE","0.46","0.43","0.43","0.40","-","-","-"),
                           c("Avg. Test RMSE","0.45","0.42","0.42","0.39","-","-","-"),
                           c("CV Moran's I","0.30","0.30*","0.21*","0.20*","-","-","-")),
          out = "spatial_table_5_cv.tex", 
          notes.append = T, notes.align = "r")

stargazer(mig.ols_lfe,mig.fe_year,mig.fe_state,mig.fe_lfe, lm_lslm,lm_esf,lm_gstsls,type = "latex",
          align = FALSE,no.space = TRUE,style = "aer", digits = 4,
          title = "Cross-Validation of Modeling Efforts",
          column.labels = c("OLS","Year FE","Region FE","Region & Year FE","ESF","SLM","GS2SLS"),
          dep.var.labels = "Log (Outflows)",
          #column.separate = c(4,3),
          model.names = FALSE, omit.stat = c("ser","f"),
          #apply.coef = multiply.100, apply.se = multiply.100,
          notes = "Coefficients and Standard Errors multiplied by 100",
          font.size = "footnotesize",
          add.lines = list(c("Avg. Train Moran's I","0.3360","0.3439","0.2908","0.2976","0.1319","0.3318","-0.0053"),
                           c("Avg. Test Moran's I","0.3526","0.3579","0.3167","0.3218","0.1302","0.3024","-0.0016"),
                           c("Avg. Train RMSE","0.9101","0.8671","0.8579","0.8140","0.6557","0.7647","0.6086"),
                           c("Avg. Test RMSE","0.8804","0.8300","0.8254","0.7714","0.6331","0.7264","0.5974"),
                           c("Avg. Train $R2$","0.6416","0.6721","0.5786","0.6165","-","-","-"),
                           c("Avg. Test $R2$","0.7542","0.7531","0.6969","0.7010","-","-","-")),
          out = "spatial_cv_table.tex", 
          notes.append = T, notes.align = "r")


stargazer(mig.ols_lfe,gstsls.mig,type = "latex",
          align = F,no.space = T,style = "aer", digits = 3,
          title = "Modeling Migration Outflows with Natural Disaster Indicators",
         # column.labels = c("Non-Spatial Weights","Spatial Weights"),
          dep.var.labels = "Log Out-flows",
        #  column.separate = c(4,3),
          model.names = FALSE,omit.stat = c("ser","f"),
          apply.coef = multiply.100, apply.se = multiply.100,
          font.size = "footnotesize",
          add.lines = list(c("Year FE","-","Yes","Yes"),
                           c("Region FE","-","-","Yes"),
                           c("Moran's I","0.30*","0.07*","0.06*")),
          out = "spatial_lag_table-2.tex",
          notes.append = FALSE, notes.align = "r")
          

stargazer(lm_resf,type="html",title = "Spatial Regression Output (RE)",
          coef = res_est,
          se = res_se,
          t = res_t,
          p = res_p,
          omit.stat = c("rsq","f","ser","adj.rsq"),
          apply.coef = multiply.100, apply.se = multiply.100,
          dep.var.labels = "Log Out Flows",
          out = "resf.htm")

stargazer(lm_esf,type="html",title = "Spatial Regression Output (FE)",
          coef = esf_est,
          se = esf_se,
          t = esf_t,
          p = esf_p,
          omit.stat = c("rsq","f","ser","adj.rsq"),
          apply.coef = multiply.100, apply.se = multiply.100,
          dep.var.labels = "Log Out Flows",
          out = "esf.htm")

stargazer(lm_lslm,type="html",title = "Spatial Regression Output (Lag)",
          coef = mig_est,
          se = lslm_se,
          p = lslm_p,
          omit.stat = c("f","ser"),
          apply.coef = multiply.100, apply.se = multiply.100,
          dep.var.labels = "Log Out Flows",
          out = "lslm.htm")


