#' Standardize data
#'
#' @param X A df/tibble
#' @param exclude A vector of columns names indicating columns to ignore when
#' computing the fd. Names can be passed directly as if they were variables in
#' the environment - if missing defaults to `NULL`.
#' @param scale A logical indicating whether to scale data or only center - if
#' missing defaults to `FALSE``.`
#'
#' @return A df/tibble with standardized columns
#' @export
standardize <- function(X, exclude = NULL, scale = FALSE) {
    if (isTRUE(scale)) {
        standardized_X <- X %>%
            dplyr::mutate(dplyr::across(-{{ exclude }},
                                        ~ (scale(., scale = scale))))
        return(standardized_X)
    } else if (isFALSE(scale)) {
        standardized_X <- X %>%
            dplyr::mutate(
                dplyr::across(-{{ exclude }}, ~ (. - mean(., na.rm = TRUE)))
            )
        return(standardized_X)
    }
}
