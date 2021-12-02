#' Extend series
#'
#' This function relies on forecast::auto.arima.
#' See <https://www.rdocumentation.org/packages/forecast/versions/8.15/topics/auto.arima>
#'
#' @param X A df/tibble.
#' @param columns A vector of characters indicating columns to extend.
#' @param n An vector indicating the number of samples to remove and predict for
#' each column name in `columns`. Be careful to respect the order in `columns`.
#' @param mode Indicates to extend columns using an ARMA(p,d,q) model or by
#' replacing missing values with the last observed value .
#' Must be one of  `"ARIMA"` or `"constant"`.
#'
#' @return A df/tibble
#' @export
extend_series <-
    function(X, columns, n, mode = c("ARIMA", "constant")) {
        if (!identical(length(columns), length(n))) {
            stop("\"columns\" and \"n\" must have the same length.")
        }
        mode <- match.arg(mode)
        n_max <- dim(X)[1]
        for (i in seq_along(columns)) {
            col = columns[i]
            serie <- X %>%
                dplyr::pull(col)
            serie <- serie[-c((n_max - n[i] + 1):n_max)]

            if (identical(mode, "ARIMA")) {
                arima_model <-
                    forecast::auto.arima(y = serie,
                                         max.p = 4,
                                         max.q = 4,
                                         max.d = 1)
                serie[(n_max - n[i] + 1):n_max] <-
                    forecast::forecast(arima_model, h = n[i])$mean
            } else if (identical(mode, "constant")) {
                serie[(n_max - n[i] + 1):n_max] <- serie[length(serie)]
            }

            X <- X %>%
                dplyr::mutate({
                    {
                        col
                    }
                } := serie)
        }
        return(X)
    }
