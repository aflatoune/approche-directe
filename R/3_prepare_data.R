#' Add dummies to the dataset
#'
#' @param data A df/tibble
#' @param names A character or vector of characters, names for the dummies
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
             "\"conds\".")
    }

    for (i in seq_along(names)) {
        data <- data %>%
            dplyr::mutate(!!names[[i]] := dplyr::if_else(!!conds[[i]], 1, 0))
    }
    return(df)
}


#' Standardize data
#'
#' @param data A df/tibble
#' @param exclude A vector of characters, indicates columns to ignore when
#' standardizing - if missing defaults to "c("year", "quarter")".
#'
#' @return A df/tibble with standardized columns
#' @export
standardize_data <- function(data, exclude = c("year", "quarter")) {
    standardized_data <- data %>%
        dplyr::mutate(dplyr::across(-excluded_vars, ~ (. - mean(., na.rm = TRUE)) /
                                        sd(., na.rm = TRUE)))
    return(standardized_data)
}
