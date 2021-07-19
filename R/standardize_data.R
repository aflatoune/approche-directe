#' Standardize data
#'
#' @param data A df/tibble
#' @param exclude A vector of characters indicating columns to ignore when
#' standardizing - if missing defaults to `NULL`.
#' @param scale A logical indicating whether to scale data or only center - if
#' missing defaults to `FALSE``.`
#'
#' @return A df/tibble with standardized columns
#' @export
standardize_data <- function(data, exclude = NULL, scale = FALSE) {
    if (isTRUE(scale)) {
        standardized_data <- data %>%
            dplyr::mutate(dplyr::across(-exclude, ~ (scale(., scale = scale))))
        return(standardized_data)
    } else if (isFALSE(scale)) {
        standardized_data <- data %>%
            dplyr::mutate(
                dplyr::across(-exclude, ~ (. - mean(., na.rm = TRUE)))
            )
        return(standardized_data)
    }
}
