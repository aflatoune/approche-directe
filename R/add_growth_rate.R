#' Convert series to growth rate
#'
#' @param X A df/tibble
#' @param exclude A vector of columns names indicating columns to ignore when
#' computing the fd. Names can be passed directly as if they were variables in
#' the environment - if missing defaults to `NULL`.
#' @param inplace A logical indicating whether to replace values or add values
#' as new columns to `X` - if missing defaults to `TRUE`.
#'
#' @return A df/tibble augmented with converted variables.
#' @export
add_growth_rate <- function(X, exclude = NULL, inplace = TRUE) {
    if (isFALSE(inplace)) {
        X <- X %>%
            dplyr::mutate(
                dplyr::across(-{{ exclude }},
                              list(~ (. / dplyr::lag(.)) - 1),
                              .names = "{.col}_gr")
            )
    } else {
        X  <- X %>% dplyr::mutate(dplyr::across(-{{ exclude }},
                                                ~ ((. / dplyr::lag(.)) - 1))
        )
    }
    return(X)
}
