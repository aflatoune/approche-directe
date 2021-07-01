#' Add dummies to the dataset
#'
#' @param df A df/tibble
#' @param names A character or list of characters, names for the dummies
#' @param conds A logical expression or list of logical expressions
#'
#' @return A df/tibble with additional columns with 1s if \code(conds) is TRUE
#' and 0s otherwise.
#' @export
#'
#' @examples
add_dummy <- function(df, names, conds) {
    if (!(identical(length(names), length(conds)))) {
        stop("The number of \" names\" must be equal to the number ",
             "of \"conditions\".")
    }
    df <- df %>%
        dplyr::mutate(!!name := dplyr::case_when(cond ~ 1, TRUE ~ 0))
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
