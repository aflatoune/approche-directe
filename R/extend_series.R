#' Extend series with the best ARIMA(p,d,q) model
#'
#' This function relies on forecast::auto.arima.
#' See <https://www.rdocumentation.org/packages/forecast/versions/8.15/topics/auto.arima>
#'
#' @param X A df/tibble.
#' @param cols A vector of characters indicating columns to extend.
#' @param n An integer indicating the number of samples to remove and predict
#' - if missing defaults to 1L.
#'
#' @return A df/tibble
#' @export
extend_series <- function(X, cols, n = 1L) {
    n_max <- dim(X)[1]
    for (col in cols) {
        serie <- X %>%
            dplyr::pull(col)
        serie <- serie[-c((n_max - n + 1):n_max)]
        arima_model <-
            forecast::auto.arima(serie,
                                 max.p = 4,
                                 max.q = 4,
                                 max.d = 1)
        serie[(n_max - n + 1):n_max] <- predict(arima_model, n.ahead = n)$pred
        X <- X %>%
            dplyr::mutate({{ col }} := serie)
    }
    return(X)
}
