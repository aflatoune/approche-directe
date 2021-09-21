#' Add dummies to the dataset
#'
#' @param X A df/tibble.
#' @param names A list of characters indicating names for the dummies.
#' @param ... Logical expressions.
#'
#' @return A df/tibble with additional columns with 1s if `conds` is `TRUE`
#' and 0s otherwise.
#' @export
add_dummy <- function(X, names, ...) {
    conds <- rlang::quos(...)

    if (!identical(length(names), length(conds))) {
        stop("The number of \"names\" must be equal to the number of ",
             "\"conds\".", call. = FALSE)
    }

    for (i in seq_along(names)) {
        X <- X %>%
            dplyr::mutate(!!names[[i]] := dplyr::if_else(!!conds[[i]], 1, 0))
    }
    return(X)
}
