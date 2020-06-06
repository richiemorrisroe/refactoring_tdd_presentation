require(lme4)
prop_size_num <- with(train_sample, gsub("[A-zA-z]+", "", x=property_size_description))
prop2 <- gsub("\\s+", " ", x=prop_size_num)
train_sample$property_size_description <- fct_explicit_na(prop2)
pprlm0 <- lm(price~property_size_description+county, data=train_sample)
pprlm1 <- lm(price~property_size_description+county+year, data=train_sample)
pprlm2 <- lm(price~property_size_description+county+(year)^2, data=train_sample)
pprlm3_log <- lm(log(price)~property_size_description+county+poly(year, 2), data=train_sample)
ppr_lmer <- lmer(price~property_size_description+county+(1|year), data=train_sample)
ppr_lmer2 <- lmer(price~property_size_description+(1|county)+(1|year), data=train_sample)
ppr_lmer3 <- lmer(price~property_size_description+(county|year), data=train_sample)

save_model <- function(x) saveRDS(x, paste(substitute(x) , ".rds", sep=""))

smaller_sample <- sample_frac(prop_sample, size=.1)

require(rstanarm)
stanlm <- stan_lmer(log(price, base=10)~year+(1|county), data=smaller_sample)
stanlm2 <- stan_lmer(log(price)~(month|year)+(1|county),
                     data=smaller_sample)
stanlm3 <- stan_lmer(log(price)~+postal_code+
                         (month|year)+(1|county),
                     data=smaller_sample)
stanlm4 <- stan_lmer(log(price)~+vat_exclusive+not_full_market_price+
                         (postal_code|year)+(1|county), data=smaller_sample)
dub_sample <- filter(prop_df2, county=="Dublin") %>% sample_frac(size=0.1)

stanlm_dub <- stan_lmer(log(price)~+vat_exclusive+not_full_market_price+(postal_code|year), data=dub_sample)

require(rstanarm)
stanlm <- readRDS("stanlm.rds")
stanlm2 <- readRDS("stanlm2.rds")
stanlm3 <- readRDS("stanlm3.rds")
stanlm_dub <- readRDS("stanlm_dub.rds")

stanlm <- stan_lmer(log(price)~year+(1|county), data=smaller_sample)

dubonly <- filter(ppr_gc2, ppr_county=="Dublin")
dubonly2 <- mutate(dubonly, ed_id=as.numeric(electoral_district_id))
pobal_plus_ppr_dublin <- inner_join(pobal2, dubonly2, by=c("ID06"="ed_id"))
dublin_model_sample <- sample_frac(pobal_plus_ppr_dublin, 0.1)
require(caret)
dub_trainind <- with(dublin_model_sample, createDataPartition(price, p=0.7, times=1, list=FALSE))
dub_train <- dublin_model_sample[trainind,]
dub_test <- dublin_model_sample[-trainind,]
pobal_locs <- select(pobal_plus_ppr, latitude, longitude)
 sp_ppr_pobal <- SpatialPointsDataFrame(pobal_locs, data=ppr_plus_pobal, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
sf_ppr_pobal <- st_as_sf(sp_ppr_pobal)
ppr_plus_shapefile <- merge(sp_ppr_pobal, dubspatial, by.x="ed_id", by.y="CSOED")
