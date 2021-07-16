#' Standardize data
#'
#' @param data A df/tibble
#' @param exclude A vector of characters indicating columns to ignore when
#' standardizing - if missing defaults to `"date"`.
#' @param reduce A logical incating whether to reduce data or only center - if
#' missing defaults to `FALSE``.`
#'
#' @return A df/tibble with standardized columns
#' @export
standardize_data <- function(data, exclude = "date", reduce = FALSE) {
    if (isTRUE(reduce)) {
        standardized_data <- data %>%
            dplyr::mutate(dplyr::across(-exclude, ~ (. - mean(., na.rm = TRUE)) /
                                            sd(., na.rm = TRUE)))
        return(standardized_data)
    } else if (isFALSE(reduce)) {
        standardized_data <- data %>%
            dplyr::mutate(
                dplyr::across(-exclude, ~ (. - mean(., na.rm = TRUE)))
            )
        return(standardized_data)
    }
}


#' Add first-difference to the dataset
#'
#' @param data A df/tibble
#' @param exclude A vector of characters indicating columns to ignore when
#' computing the fd - if missing defaults to `"date"`.
#'
#' @return A df/tibble augmented with fd
add_firstdiff <- function(data, exclude = "date") {
    data <- data %>%
        dplyr::mutate(
            dplyr::across(-exclude, list("fd" =  ~ . - dplyr::lag(., n = 1L)))
        )
    return(data)
}


#' Add dummies to the dataset
#'
#' @param data A df/tibble
#' @param names A list of characters indicating names for the dummies
#' @param ... Logical expressions
#'
#' @return A df/tibble with additional columns with 1s if `conds` is `TRUE`
#' and 0s otherwise.
#' @export
#'
#' @examples
#' X <- add_dummy(X, names = list("dummy1", "dummy2"), cond1, cond2)
add_dummy <- function(data, names, ...) {
    conds <- rlang::quos(...)

    if (length(names) != length(conds)) {
        stop("The number of \"names\" must be equal to the number of ",
             "\"conds\".", call. = FALSE)
    }

    for (i in seq_along(names)) {
        data <- data %>%
            dplyr::mutate(!!names[[i]] := dplyr::if_else(!!conds[[i]], 1, 0))
    }
    return(data)
}


#' Extend series
#'
#' @param data A df/tibble
#' @param cols A vector of characters indicating columns to extend
#' @param n An integer indicating the number of samples to remove and predict
#' TODO: generalize to n > 1
#' TODO: select cols with dplyr selection helpers
#'
#' @return A df/tibble
#' @export
extend_series <- function(data, cols, n = 1) {
    n_max <- dim(data)[1]
    for (col in cols) {
        serie <- data %>%
            dplyr::pull(col)
        serie <- serie[-n_max]
        arima_model <- forecast::auto.arima(serie, max.p = 4, max.q = 4, max.d = 1)
        serie[n_max] <- predict(arima_model)$pred[1]
        data <- data %>%
            dplyr::mutate(!!col := serie)
    }
    return(data)
}
