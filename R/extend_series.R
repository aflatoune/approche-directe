#' Extend series with the best ARIMA(p,d,q) model
#'
#' The function relies on forecast::auto.arima.
#' See <https://www.rdocumentation.org/packages/forecast/versions/8.15/topics/auto.arima>
#'
#' @param X A df/tibble.
#' @param cols A vector of characters indicating columns to extend.
#' @param n An integer indicating the number of samples to remove and predict.
#' TODO: generalize to n > 1
#' TODO: select cols with dplyr selection helpers
#'
#' @return A df/tibble
#' @export
extend_series <- function(X, cols, n = 1) {
    n_max <- dim(X)[1]
    for (col in cols) {
        serie <- X %>%
            dplyr::pull(col)
        serie <- serie[-n_max]
        arima_model <-
            forecast::auto.arima(serie,
                                 max.p = 4,
                                 max.q = 4,
                                 max.d = 1)
        serie[n_max] <- predict(arima_model)$pred[1]
        X <- X %>%
            dplyr::mutate({{ col }} := serie)
    }
    return(X)
}
