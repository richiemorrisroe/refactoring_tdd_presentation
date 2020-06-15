##' remove non ASCII characters from name vector of df
##'
##' replaces spaces with underscores
##' @title normalise_names
##' @param df an object inheriting from data.frame
##' @return a data.frame with fixed names
##' @author richie
##' @export
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

##' fix_price
##'
##' remove commas from vector
##' @title fix_price
##' @param x a vector of numbers with commas
##' @return the same vector, without commas
##' @author richie
##' @export
fix_price <- function(x) {
        nopunct <- gsub(",", "", x = x)
        nums <- as.numeric(
                iconv(nopunct, "latin1",
                        "ASCII",
                        sub = ""
                )
        )
}
##' log column
##'
##' as above
##' @title log_column
##' @param df 
##' @param col 
##' @return a data.frame with a log_price column added
##' @author richie
##' @export
log_column <- function(df, col) {
      col <- rlang::enquo(col)
      res <- dplyr::mutate(df,
                           log_price =
                             log(!!col, base = 10))
      return(res)
      }
##' fit a simple model, bootstrap the results
##'
##' see above
##' @title generate_bootstrap_results
##' @param df a data.frame
##' @param ind a set of indices representing which observations to retain
##' @return a list of results
##' @author richie
##' @export
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

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title mark_values_as_large
##' @param df 
##' @param large 
##' @return 
##' @author richie
##' @export
mark_values_as_large <- function(df, large) {
  large <- rlang::enquo(large)
  ppr3 <- dplyr::mutate(df,
                        is_big =
                          ifelse(
                            .data$price >= !!large,
                            "Big", "Not Big"))
  return(ppr3)
  }

log_column <- function(df, col) {
    col <- rlang::enquo(col)
    res <- dplyr::mutate(df,
                         log_price =
                           log(!!col, base = 10))
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
    caret::createDataPartition(log_price,
                               times = 1,
                               p = 0.7,
                               list = FALSE
                               )
  ) %>% as.vector() #because tibble sucks
  ppr_train <- df[ppr_train_indices, ]

  ppr_not_train <- df[-ppr_train_indices, ]
  ppr_test_indices <- with(
    ppr_not_train,
    caret::createDataPartition(log_price,
                               times = 1,
                               p = 0.5,
                               list = FALSE
                               )
  ) %>% as.vector()

  ppr_test <- ppr_not_train[ppr_test_indices, ]
  ppr_validation <- ppr_not_train[-ppr_test_indices, ]
  readr::write_csv(x = ppr_validation, path = "ppr_validation_set.csv")
  rm(ppr_validation)
  return(list(train=ppr_train, test=ppr_test))
}

##' count unique values in all columns of a dataframe
##'
##' return an ordered data.frame
##' @title count_distinct_values
##' @param df data.frame
##' @return a data.frame
##' @author richie
##' @export
count_distinct_values <- function(df) {
  res1  <- sapply(df, dplyr::n_distinct) %>%
    as.data.frame()
  res2  <- res1 %>% 
    tibble::rownames_to_column() 
    result  <- res2 %>%
    dplyr::arrange(desc(`.`))
  return(result)
  }
##' count proportion of NA's in each col of data.frame
##'
##' see above
##' @title count_proportion_missing
##' @param df a data.frame
##' @return a data.frame with the proportion missing in each variable
##' @author richie
##' @export
count_proportion_missing <- function(df) {
  result <- sapply(df, function(x) {
              sum(is.na(x)) / length(x)
      }) %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        dplyr::arrange(desc(`.`)) %>%
    dplyr::mutate_if(is.numeric, round, 4)
  return(result)
  }
#+begin_src R :session :tangle ppr/R/ppr.R
  ##' get maximum & minimum of all numeric columns in a data.frame
  ##'
  ##' see above
  ##' @title get_max_and_min
  ##' @param df 
  ##' @return a data.frame with col, max and min columns
  ##' @author richie
  ##' @export
        get_max_and_min <- function(df) {
          maxmin  <- dplyr::select_if(as.data.frame(df),
                                               is.numeric) %>%
            sapply(., function(x) {
              data.frame(
                min = min(x, na.rm = TRUE),
                max = max(x, na.rm = TRUE)
              )
            }) %>%
            tibble::as_tibble() %>%
            tibble::rownames_to_column()

          max_min_df  <- tidyr::unnest(max_min,
                                       cols=colnames(max_min2))
          return(max_min_df) }
  ##' count distinct values in col of data.frame
  ##'
  ##' returns an data.frame ordered by counts
  ##' @title value_counts
  ##' @param df a data.frame
  ##' @param col a character or factor column
  ##' @return a data.frame ordered by count of values in col
  ##' @author richie
  ##' @export
  value_counts <- function(df, col) {
    result <-
      table(eval(substitute(col), envir=df),
            useNA = "always") %>% 
      as.data.frame() %>%
      arrange(desc(Freq))

  }
##' load test data
##'
##' see above
##' @title load_test_data
##' @param name 
##' @return a data.frame with test data
##' @author richie
##' @export
  load_test_data <- function(name) {
    bpath  <- "../../inst/"
    result <- readr::read_csv(paste0(bpath, name))
    return(result)
  }
