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

load_data <- function(path) {
  ppr <- readxl::read_excel(path, sheet = "PPR-ALL")
  return(ppr)
  }

mark_values_as_large <- function(df, large) {
  large <- rlang::enquo(large)
  ppr3 <- dplyr::mutate(df,
                        is_big = ifelse(.data$price >= !!large,
                                        "Big", "Not Big"))
  return(ppr3)
  }

log_column <- function(df, col) {
    col <- rlang::enquo(col)
    res <- dplyr::mutate(df, log_price = log(!!col, base = 10))
    return(res)
    }

invert_field <- function(df, field) {
  field  <- rlang::enquo(field)
  ppr5 <- dplyr::mutate(df,
             is_full_market_price = ifelse(
                     !!field == "No",
                     "Yes",
                     "No"
             )
             ) %>%
    dplyr::select(-not_full_market_price)
  return(ppr5)
}

fix_property_description  <- function(df) {
remove_irish <- dplyr::mutate(df, prop_description = ifelse(
        grepl("cothrom", x = property_size_description),
        "greater than or equal to 38 sq metres and less than 125 sq metres",
        ifelse(
                grepl("cearnach", x = property_size_description),
                "less than 38 sq metres",
                property_size_description
        )
        ))

shorten_greater_than <- dplyr::mutate(remove_irish, prop_description = ifelse(
        prop_description == "greater than 125 sq metres",
        "ge_125_square_meters",
        ifelse(
                prop_description ==
                        "greater than or equal to 125 sq metres",
                "ge_125_square_meters",
                prop_description
        )
))

shorten_less_than_greater_than <-
  dplyr::mutate(shorten_greater_than,
                property_size_description = ifelse(
                  prop_description == "less than 38 sq metres",
                  "lt_38_square_meters",
                                            ifelse(
                                              prop_description ==
                                              "greater than or equal to 38 sq metres and less than 125 sq metres",
                                              "ge_38_lt_125_square_meters",
                                              prop_description
                                            )
                )) %>%
        dplyr::select(-prop_description) # pointless now
result <- dplyr::mutate(shorten_less_than_greater_than,
                        property_size_description = as.character(
                          forcats::fct_explicit_na(property_size_description))
        )
return(result)
}

split_data <- function(df) {
  set.seed(34)
  ppr_train_indices <- with(
    df,
    createDataPartition(log_price,
                        times = 1,
                        p = 0.7,
                        list = FALSE
                        )
  )
  ppr_train <- df[ppr_train_indices, ]

  ppr_not_train <- df[-ppr_train_indices, ]
  ppr_test_indices <- with(
    ppr_not_train,
    createDataPartition(log_price,
                        times = 1,
                        p = 0.5,
                        list = FALSE
                        )
  ) 

  ppr_test <- ppr_not_train[ppr_test_indices, ]
  ppr_validation <- ppr_not_train[-ppr_test_indices, ]
  write_csv(x = ppr_validation, path = "ppr_validation_set.csv")
  rm(ppr_validation)
  return(list(train=ppr_train, test=ppr_test))
}
