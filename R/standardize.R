#' Standardize data
#'
#' @param X A df/tibble
#' @param exclude A vector of columns names indicating columns to ignore when
#' computing the fd. Names can be passed directly as if they were variables in
#' the environment - if missing defaults to `NULL`.
#' @param scale A character indicating whether to scale data or only center.
#' Must be one of "center" or "scale".
#'
#' @return A df/tibble with standardized columns
#' @export
standardize <- function(X, exclude = NULL, mode = c("center", "scale")) {
    mode <- match.arg(mode)

    if (identical(mode, "scale")) {
        standardized_X <- X %>%
            dplyr::mutate(dplyr::across(-{{ exclude }},
                                        ~ (scale(., scale = TRUE))))
        return(standardized_X)
    } else if (identical(mode, "center")) {
        standardized_X <- X %>%
            dplyr::mutate(
                dplyr::across(-{{ exclude }}, ~ (. - mean(., na.rm = TRUE)))
            )
        return(standardized_X)
    }
}
