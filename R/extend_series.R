#' Extend series
#'
#' @param data A df/tibble
#' @param cols A vector of characters indicating columns to extend
#' @param n An integer indicating the number of samples to remove and predict
#' TODO: generalize to n > 1
#' TODO: select cols with dplyr selection helpers
#'
#' @return A df/tibble
#' @export
extend_series <- function(data, cols, n = 1) {
    n_max <- dim(data)[1]
    for (col in cols) {
        serie <- data %>%
            dplyr::pull(col)
        serie <- serie[-n_max]
        arima_model <- forecast::auto.arima(serie, max.p = 4, max.q = 4, max.d = 1)
        serie[n_max] <- predict(arima_model)$pred[1]
        data <- data %>%
            dplyr::mutate(!!col := serie)
    }
    return(data)
}
