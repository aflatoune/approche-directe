#' Add n-difference to the dataset
#'
#' @param X A df/tibble
#' @param exclude A vector of columns names indicating columns to ignore when
#' computing the fd. Names can be passed directly as if they were variables in
#' the environment - if missing defaults to `NULL`.
#' @param n A numeric indicating the order of the differencied variables.
#'
#' @return A df/tibble augmented with differencied variables.
#' @export
add_diff <- function(X, exclude = NULL, n = 1L) {
    n <- as.integer(n)
    X <- X %>%
        dplyr::mutate(
            dplyr::across(-{{ exclude }},
                          list(~ . - dplyr::lag(., n = n)),
                          .names = "{.col}_fd{n}")
        )
    return(X)
}
