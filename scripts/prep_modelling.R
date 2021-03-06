library(tidyverse)
library(readxl)
ppr <- read_excel("~/Dropbox/PPR/PPR-ALL.xlsx", sheet = "PPR-ALL")
names(ppr)
normalise_names <- function(df) {
        nms <- names(df)
        normed <- iconv( # covert from one encoding to another
                tolower(
                        gsub("([[:space:]]|[[:punct:]])+", "_",
                                x = nms
                        )
                ),
                "latin1", # from encoding
                "ASCII", # to encoding
                sub = ""
        )
        drop_usc <- gsub("([a-z_])_*$",
                         "\\1", x = normed) # drop extra underscores
        names(df) <- drop_usc # update names
        df # return dataframe
}
fix_price <- function(x) {
        nopunct <- gsub(",", "", x = x)
        nums <- as.numeric(
                iconv(nopunct, "latin1",
                        "ASCII",
                        sub = ""
                )
        )
}

generate_bootstrap_results <- function(df, ind) {
        # results list (always generate something to hold your results first)
        trainresults <- vector(mode = "list", length = 10)
        for (i in 1:length(ind)) {
                nm <- names(ind[i])
                train <- df[ind[[i]], ]
                test <- df[-ind[[i]], ]
                model <- lm(log_price ~ property_size_description + year,
                        data = train
                )
                preds <- predict(model,
                        newdata = test, type = "response",
                        na.action = na.exclude
                )
                trainresults[[i]] <- preds
        }
        names(trainresults) <- names(ind)
        trainresults
}

ppr2 <- normalise_names(ppr)

with(ppr2, mean(price))

select(ppr2, date_of_sale_dd_mm_yyyy, price, postal_code) %>% head()


ppr2$price <- with(ppr2, fix_price(price))
with(ppr2, mean(price))

ggplot(ppr2, aes(x = price)) +
        geom_density() +
        scale_x_continuous(labels = scales::dollar_format(prefix = "€"))

with(ppr2, summary(price))

ggplot(ppr2, aes(x = price / 100)) +
        geom_density() +
        scale_x_continuous(labels = scales::dollar_format(prefix = "€"))

ppr3 <- mutate(ppr2, price = price / 100, is_big = ifelse(price >= 2e6, "Big", "Not Big"))
ppr3 %>%
        ggplot(aes(x = price)) +
        geom_density() +
        scale_x_continuous(labels = scales::dollar_format(prefix = "€")) +
        facet_wrap(~is_big, scales = "free")

ggplot(ppr3, aes(x = log(price))) +
        geom_density() +
        scale_x_continuous(
                labels = scales::dollar_format(prefix = "€")
        ) +
        xlab("log price, euros")

ppr4 <- mutate(ppr3, log_price = log(price, base = 10)) # saves time overall
ggplot(ppr4, aes(x = log_price)) +
        geom_density() +
        xlab("log price, euros")

names(ppr4)

sapply(ppr4, class)

daily_data <- ppr4 %>%
        rename(date_of_sale = date_of_sale_dd_mm_yyyy) %>%
        mutate(date_of_sale = lubridate::dmy(date_of_sale)) %>%
        group_by(date_of_sale) %>%
        summarise(
                count = n(), mean_price = mean(price, na.rm = TRUE),
                median_price = median(price, na.rm = TRUE),
        )
ggplot(daily_data, aes(x = date_of_sale, y = mean_price, size = count)) +
        geom_point()

sapply(ppr4, function(x) n_distinct(x)) %>% as.data.frame()

post_codes_table <-
        with(ppr4, table(postal_code, useNA = "always")) %>%
        as.data.frame() %>%
        arrange(desc(Freq))
head(post_codes_table, n = 12)

ppr5 <- mutate(ppr4, no_postcode = ifelse(is.na(postal_code), "Yes", "No"))
no_postcode_county <-
        filter(ppr5, no_postcode == "Yes") %>%
        group_by(county) %>%
        summarise(count = n()) %>%
        arrange(desc(count))

postcode_county <-
        filter(ppr5, no_postcode == "No") %>%
        group_by(county) %>%
        summarise(count = n()) %>%
        arrange(desc(count))

head(postcode_county, n = 5)

with(ppr4, table(not_full_market_price, useNA = "always"))

ppr5 <- mutate(ppr4,
        is_full_market_price = ifelse(
                not_full_market_price == "No",
                "Yes",
                "No"
        )
) %>%
        select(-not_full_market_price) # remove old var - what happens if we don't?

with(ppr5, table(vat_exclusive, useNA = "always"))

with(ppr5, table(description_of_property))

ppr6 <- mutate(ppr5,
        description_of_property =
                ifelse(
                        grepl("Nua", x = description_of_property),
                        "New Dwelling house /Apartment",
                        ifelse(
                                grepl("Ath", x = description_of_property),
                                "Second-Hand Dwelling house /Apartment",
                                ## nested ifelse are pretty useful
                                description_of_property
                        )
                )
)

with(ppr6, table(description_of_property, useNA = "always"))

with(ppr6, table(property_size_description, useNA = "always"))

ppr7 <- mutate(ppr6, prop_description = ifelse(
        grepl("cothrom", x = property_size_description),
        "greater than or equal to 38 sq metres and less than 125 sq metres",
        ifelse(
                grepl("cearnach", x = property_size_description),
                "less than 38 sq metres",
                property_size_description
        )
))

ppr8 <- mutate(ppr7, prop_description = ifelse(
        prop_description == "greater than 125 sq metres",
        "ge_125_square_meters",
        ifelse(
                prop_description ==
                        "greater than or equal to 125 sq metres",
                "ge_125_square_meters",
                prop_description
        )
))

ppr9 <- mutate(ppr8, property_size_description = ifelse(
        prop_description == "less than 38 sq metres",
        "lt_38_square_meters",
        ifelse(
                prop_description ==
                        "greater than or equal to 38 sq metres and less than 125 sq metres",
                "ge_38_lt_125_square_meters",
                prop_description
        )
)) %>%
        select(-prop_description) # pointless now
ppr10 <- mutate(ppr9,
        property_size_description = fct_explicit_na(property_size_description)
)

library(caret)
# set.seed ensures that future samples are consistent so this is a bad
# idea
## generated by taking the integer part of set.seed(rnorm(1, mean=50, sd=100))
set.seed(34)
# create sample of 70% of rows, weighted by quantiles of y (i.e.
# log_price)
ppr_train_indices <- with(
        ppr10,
        createDataPartition(log_price,
                times = 1,
                p = 0.7,
                list = FALSE
        )
)

# apply this sample to the dataset
ppr_train <- ppr10[ppr_train_indices, ]
# get not in this sample (i.e. test)
ppr_not_train <- ppr10[-ppr_train_indices, ]
# create another sample of 50% size
ppr_test_indices <- with(
        ppr_not_train,
        createDataPartition(log_price,
                times = 1,
                p = 0.5,
                list = FALSE
        )
) # get a 50% sample

# generate test dataset
ppr_test <- ppr_not_train[ppr_test_indices, ]
# generate validation dataset (hide this under a rock until the very,
# very end)
ppr_validation <- ppr_not_train[ppr_test_indices, ]
write_csv(x = ppr_validation, path = "ppr_validation_set.csv")
rm(ppr_validation)

ppr_train2 <- rename(ppr_train, date_of_sale = date_of_sale_dd_mm_yyyy) %>%
        mutate(
                year = lubridate::year(date_of_sale),
                month = lubridate::month(date_of_sale),
                quarter = lubridate::quarter(date_of_sale)
        )
ppr_bootstrap_indices <- with(ppr_train, createResample(log_price, times = 100))

generate_bootstrap_results <- function(df, ind) {
        # results list (always generate something to hold your results first)
        trainresults <- vector(mode = "list", length = 10)
        for (i in 1:length(ind)) {
                nm <- names(ind[i])
                train <- df[ind[[i]], ]
                test <- df[-ind[[i]], ]
                model <- lm(log_price ~ property_size_description + year,
                        data = train
                )
                preds <- predict(model,
                        newdata = test, type = "response",
                        na.action = na.exclude
                )
                trainresults[[i]] <- preds
        }
        names(trainresults) <- names(ind)
        trainresults
}
g <- generate_bootstrap_results(ppr_train2, ppr_bootstrap_indices)
hist(sapply(g, mean)^10)

readr::write_csv(ppr_train2, path = "prep_modelling_output_old.csv")
