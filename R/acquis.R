#' Compute growth gains (acquis de croissance) at a given month.
#'
#'  Should be used on quarterly data returned by `month_to_quarter` function.
#'
#' @param X A tibble/df with quarterly data returned by
#' @param cols Vector of characters, indicators for which the gain is computed.
#' @param month Numeric indicating the month of the quarter at which the gain is
#' computed.
#'
#' @return
#' @export
acquis <- function(X,
                   cols,
                   month) {
    if (!month %in% c(0,1,2)) {
        stop("\'month\' must be equal to 0, 1 or 2.")
    }

    for (col in cols) {
        m1 <- paste0(col, "_1")
        m2 <- paste0(col, "_2")
        m3 <- paste0(col, "_3")
        col_name <- glue::glue("acquis_{col}_m{month}")
        X[[col_name]] <- NA

        if (identical(month, 0)) {
            X[col_name] <-
                (3 * lag(X[m3]) / (dplyr::lag(X[m1]) +
                                       dplyr::lag(X[m2]) +
                                       dplyr::lag(X[m3])) - 1)
        } else if (identical(month, 1)) {
            X[col_name] <-
                (3 * X[m1] / (dplyr::lag(X[m1]) +
                                  dplyr::lag(X[m2]) +
                                  dplyr::lag(X[m3])) - 1)
        } else if (identical(month, 2)) {
            X[col_name] <-
                ((X[m1] + 2*X[m2]) / (dplyr::lag(X[m1]) +
                                          dplyr::lag(X[m2]) +
                                          dplyr::lag(X[m3])) - 1)
        }

        X <- X %>% dplyr::select(-dplyr::starts_with({{ col }}))
    }

    return(X)
}
