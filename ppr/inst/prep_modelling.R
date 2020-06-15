library(tidyverse)
require(ppr)
print(ls(pos = 2))
data(ppr)
names(ppr)

ppr2 <- ppr::normalise_names(ppr)

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

ppr3 <- mark_values_as_large(ppr2, large = 2e6)
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

ppr4 <- mutate(ppr3, log_price = log_column(price))
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

ppr5 <- invert_field(ppr4, not_full_market_price)

## ppr5 <- mutate(ppr4,
##         is_full_market_price = ifelse(
##                 not_full_market_price == "No",
##                 "Yes",
##                 "No"
##         )
## ) %>%
##         select(-not_full_market_price) # remove old var - what happens if we don't?

with(ppr5, table(vat_exclusive, useNA = "always"))

with(ppr5, table(description_of_property))

ppr10 <- fix_property_description(ppr5)

readr::write_csv(ppr10, path = "ppr_data_cleaning_done.csv")

ppr_train_test <- split_data(ppr10)
ppr_train  <- ppr_train_test$train

ppr_train2 <- rename(ppr_train, date_of_sale = date_of_sale_dd_mm_yyyy) %>%
        mutate(
                year = lubridate::year(date_of_sale),
                month = lubridate::month(date_of_sale),
                quarter = lubridate::quarter(date_of_sale)
        )
ppr_bootstrap_indices <- with(ppr_train, createResample(log_price, times = 100))

g <- generate_bootstrap_results(ppr_train2, ppr_bootstrap_indices)
hist(sapply(g, mean)^10)

## readr::write_csv(ppr_train2, path = "prep_modelling_output_old.csv")
readr::write_csv(ppr_train2, path = "prep_modelling_output_refactor.csv")
