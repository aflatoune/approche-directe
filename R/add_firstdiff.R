#' Add first-difference to the dataset
#'
#' @param data A df/tibble
#' @param exclude A vector of characters indicating columns to ignore when
#' computing the fd - if missing defaults to `NULL`.
#'
#' @return A df/tibble augmented with fd
add_firstdiff <- function(data, exclude = NULL) {
    data <- data %>%
        dplyr::mutate(
            dplyr::across(-exclude, list("fd" =  ~ . - dplyr::lag(., n = 1L)))
        )
    return(data)
}
