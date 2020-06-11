require(tidyverse)
ppr_train <- readr::read_csv("prep_modelling_output_refactor.csv")
with(ppr_train, caret::BoxCoxTrans(price))

ppr_train3 <- select(
        ppr_train, -postal_code,
        -address,
        -price, -date_of_sale
)
ppr_train_s <- sample_frac(ppr_train3, size = 0.1)
ppr_test_samp <- sample_frac(ppr_train3,
        size = 0.05
)
train_ctrl <- caret::trainControl(
        method = "repeatedcv",
        repeats = 5
)
## ppr_glmnet <- train(log_price ~ .,
##         data = ppr_train_s,
##         method = "glmnet",
##         trControl = train_ctrl
## )
## preds <- predict(ppr_glmnet, newdata = ppr_test_samp)
## ppr_test_samp$glmnet_preds <- preds

## ggplot(ppr_test_samp, aes(x = log_price^10, y = glmnet_preds^10)) +
##         geom_point()

ppr_train_s2 <- filter(
        ppr_train_s,
        log_price <= log(1e6, base = 10)
) %>%
        select(-is_big)
ppr_glmnet2 <- caret::train(log_price ~ .,
        data = ppr_train_s2,
        method = "glmnet",
        trControl = train_ctrl
)
preds2 <- predict(ppr_glmnet2,
        newdata = ppr_test_samp
)
ppr_test_samp$glmnet_preds2 <- preds2

## ppr_gc1 <- filter(ppr_gc, year <= 2016)
## ppr_gc2 <- filter(ppr_gc, year > 2016)
## # github doesn't like large files
## write_csv(ppr_gc1, path = "ppr_gecoded1.csv")
## write_csv(ppr_gc2, path = "ppr_gecoded2.csv")
ppr_gc <- read_csv(
        "~/Dropbox/PPR/ppr_geocoded_till_oct2018.csv"
)
names(ppr_gc)[12:24]

library(sp)
library(rgdal)
shp <- readOGR("~/Dropbox/PPR/electoral_divisions_gps.shp")
# df stored in this slot
names(shp@data[, 13:length(shp@data)])

pobal <- read_csv("~/Dropbox/PPR/pobal_deprivation_index_ed.csv")
names(pobal)[25:45]


library(sp)
library(sf) # spatial simple features library
ppr_pobal <- readRDS("~/Dropbox/Code/DDS/ppr_sf_pobal2.rds")

ppr_gc2 <- filter(ppr_gc, !is.na(latitude), !is.na(electoral_district))
locs <- select(ppr_gc2, longitude, latitude)
sp_ppr <- SpatialPointsDataFrame(locs, data = ppr_gc2,
                                 proj4string =
                                     CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ppr_gc_sf <- st_as_sf(sp_ppr)

count_distincts  <- sapply(ppr_pobal, n_distinct) %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        arrange(desc(`.`))


readr::write_csv(count_distincts, path = "count_distincts_test_data.csv")

max_min2 <- select_if(as.data.frame(ppr_pobal), is.numeric) %>%
        sapply(., function(x) {
                data.frame(
                        min = min(x, na.rm = TRUE),
                        max = max(x, na.rm = TRUE)
                )
        }) %>%
        as_tibble() %>%
        rownames_to_column()
max_min_df <- unnest(max_min2, cols = colnames(max_min2))

readr::write_csv(max_min_df, path = "get_max_and_min_test_data.csv")
count_missings  <- sapply(ppr_pobal, function(x) {
    sum(is.na(x)) / length(x)}) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
        dplyr::arrange(desc(`.`)) %>%
    mutate_if(is.numeric, round, 4) %>%
    head(n = 5)

readr::write_csv(count_missings, path = "count_prop_missings_test_data.csv")
message("got here 2")

num_vars <- as.data.frame(ppr_pobal) %>%
        na.omit() %>%
        select_if(is.numeric)

bc2 <- lapply(num_vars,
        BoxCoxTrans,
        na.rm = TRUE
)
lambdas <- sapply(bc2, function(x) x[["lambda"]])
head(lambdas)

pobal_vars <- dplyr::select(num_vars, 26:64) %>%
        lapply(function(x) {
                scale(x,
                        center = TRUE,
                        scale = TRUE
                )
        })
pca <- princomp(~., data = pobal_vars)

screeplot(pca, type = "lines")

loadings <- pca$loadings
loadings[, 1:6] %>%
        round(3) %>%
        head()

ed_count <- ppr_pobal %>%
        as.data.frame() %>%
        group_by(electoral_district_id) %>%
        summarise(count = n(), median = median(price))
ggplot(ed_count, aes(x = count)) +
        geom_density()

# otherwise the below errors out
ppr_pobal_s <- sample_frac(ppr_pobal, size = 0.1)
## takes a loooooonnnnng time
lm_ed1 <- lm(log(price) ~ electoral_district_id,
        data = ppr_pobal_s
)

require(vtreat)
# not a perfectly independent sample
## don't do it like this
ppr_train_s2 <- sample_frac(ppr_pobal,
        size = 0.05
)
treat <- designTreatmentsN(ppr_train_s2,
        varlist = c("electoral_district_id"),
        outcomename = "price"
)
train_prep <- prepare(treat,
        dframe = ppr_pobal_s
)

ppr_pobal_test <- sample_frac(ppr_pobal, size = 0.1)
test_vtreat <- prepare(treat, ppr_pobal_test)
## glmnet_preds <- predict(glmnet_ed1,
##         newx = as.matrix(test_vtreat),
##         type = "response"
## )
## predsvobs <- data.frame(
##         preds = exp(glmnet_preds),
##         obs = ppr_pobal_test$price
## )
## ggplot(predsvobs, aes(x = obs, y = preds.s0)) +
##         geom_point()
## ggplot(predsvobs, aes(x = obs, y = preds.s60)) +
##         geom_point()
## predsvobs <- data.frame(preds = exp(glmnet_preds), obs = ppr_pobal_test$price)
## with(predsvobs, cor(preds.s60, obs))

tf <- designTreatmentsN(ppr_train_s2,
        varlist =
                setdiff(
                        names(ppr_train_s2),
                        "price"
                ),
        outcomename = "price"
)
ppr_p_t <- prepare(tf,
        dframe = ppr_pobal_s
        )
require(glmnet)
ppr_glmnet_full <- cv.glmnet(
        x = as.matrix(ppr_p_t),
        y = log(ppr_pobal_s$price)
)
test_vtreat_full <- prepare(tf, ppr_pobal_test)

## ggplot(predsvobs, aes(x = obs, y = X1)) +
##         geom_point() +
##         geom_smooth()

## ggplot(predsvobs, aes(x = log(abs(obs - X1), base = 10))) +
##         geom_density()
readr::write_csv(test_vtreat_full, path="feature_eng_results.csv")
