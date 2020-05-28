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
