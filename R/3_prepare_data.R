#' Add dummies to the dataset
#'
#' @param df A df/tibble
#' @param names A character or vector of characters, names for the dummies
#' @param ... Logical expressions
#'
#' @return A df/tibble with additional columns with 1s if `conds` is `TRUE`
#' and 0s otherwise.
#' @export
#'
#' @example
#' X <- add_dummy(X, names = list("dummy1", "dummy2), cond1, cond2)
add_dummy <- function(df, names, ...) {
    conds <- dplyr::quos(...)

    if (length(names) != length(conds)) {
        stop("The number of \"names\" must be equal to the number of ",
             "\"conds\".")
    }

    for (i in seq_along(names)) {
        df <- df %>%
            dplyr::mutate(!! names[[i]] := dplyr::if_else(!! conds[[i]], 1, 0))
    }
    return(df)
}


#' Standardize data
#'
#' @param df A df/tibble
#' @param exclude A vector of characters, indicates columns to ignore when
#' standardizing - if missing defaults to "c("year", "quarter")".
#'
#' @return A df/tibble with standardized columns
#' @export
#'
#' @examples
standardize_data <- function(df, exclude = c("year", "quarter")) {
    standardized_data <- df %>%
        dplyr::mutate(dplyr::across(-excluded_vars, ~ (. - mean(., na.rm = TRUE)) /
                                        sd(., na.rm = TRUE)))
    return(standardized_data)
}
